# ============================================================
# insectecol --- Degree-day module: fitting engines
# Data cleaning, analytic linear fit, bounded nonlinear fits (nls port),
# and the all-models engine used by model = "auto" and gdd_compare().
# All internal (not exported).
# ============================================================

## Clean / standardise data into temp / duration / rate / group
gdd_clean <- function(data, cols, by, min_n = 3) {
  T_raw <- suppressWarnings(as.numeric(data[[cols$temp]]))
  D_raw <- suppressWarnings(as.numeric(data[[cols$duration]]))
  if (all(is.na(T_raw)) || all(is.na(D_raw)))
    stop("Temperature/duration columns cannot be parsed as numeric; ",
         "please check the data format.", call. = FALSE)

  bad <- !is.finite(T_raw) | !is.finite(D_raw) | D_raw <= 0
  if (any(bad))
    warning("Removed ", sum(bad), " invalid row(s) ",
            "(NA / non-finite / duration <= 0).", call. = FALSE)

  grp <- if (is.null(by)) {
    rep("Overall", nrow(data))
  } else {
    if (!all(by %in% names(data)))
      stop("Grouping variable(s) not found: ",
           paste(setdiff(by, names(data)), collapse = ", "), call. = FALSE)
    apply(data[by], 1, paste, collapse = "-")
  }

  df <- data.frame(temp     = T_raw[!bad],
                   duration = D_raw[!bad],
                   rate     = 1 / D_raw[!bad],
                   group    = grp[!bad])

  tab_n <- table(df$group)
  drop_g <- names(tab_n)[tab_n < min_n]
  if (length(drop_g))
    warning("The following groups have fewer than ", min_n,
            " rows and were skipped: ",
            paste(drop_g, collapse = ", "), call. = FALSE)
  df <- df[!df$group %in% drop_g, , drop = FALSE]
  if (!nrow(df))
    stop("No usable data after cleaning: each group needs at least ",
         min_n, " temperature points.", call. = FALSE)
  df
}

## Analytic OLS fit of the linear model (exact standard errors)
gdd_fit_linear <- function(T_, V_, conf_level = 0.95) {
  n <- length(T_)
  if (n < 3)
    stop("The linear model needs at least 3 temperature points.",
         call. = FALSE)
  Vbar <- mean(V_); Tbar <- mean(T_)
  Lvv <- sum((V_ - Vbar)^2)
  Ltv <- sum((V_ - Vbar) * (T_ - Tbar))
  Ltt <- sum((T_ - Tbar)^2)
  if (Lvv < .Machine$double.eps)
    stop("Developmental rates show no variation; cannot fit.",
         call. = FALSE)

  K <- Ltv / Lvv              # effective accumulated temperature
  C <- Tbar - K * Vbar        # developmental threshold temperature
  T_hat <- C + K * V_
  Q <- sum((T_ - T_hat)^2); df <- n - 2; s2 <- Q / df
  se_K <- sqrt(s2 / Lvv)
  se_C <- sqrt(s2 * (1 / n + Vbar^2 / Lvv))

  r <- Ltv / sqrt(Lvv * Ltt); r2 <- r * r
  Fv <- if (r2 < 1) r2 * df / (1 - r2) else Inf
  pv <- stats::pf(Fv, 1, df, lower.tail = FALSE)
  tc <- stats::qt(1 - (1 - conf_level) / 2, df)

  params <- c(C = C, K = K); se <- c(C = se_C, K = se_K)
  tval <- params / se
  ctab <- data.frame(
    parameter = names(params), estimate = params, se = se,
    t_value = tval, p_value = 2 * stats::pt(-abs(tval), df),
    lower = params - tc * se, upper = params + tc * se, row.names = NULL)

  ## Log-likelihood / information criteria (k = 2 params + sigma^2)
  ll  <- -n / 2 * (log(2 * pi) + log(Q / n) + 1)
  aic <- -2 * ll + 2 * 3
  bic <- -2 * ll + log(n) * 3
  aicc <- if (n - 4 > 0) aic + 2 * 3 * 4 / (n - 4) else Inf

  list(model = "linear", label = gdd_model_label[["linear"]],
       n = n, k = 2L, df = df, params = params, se = se,
       coef_table = ctab,
       C = C, se_C = se_C, ci_C = C + c(-1, 1) * tc * se_C,
       K = K, se_K = se_K, ci_K = K + c(-1, 1) * tc * se_K,
       Topt = NA_real_, Vmax = NA_real_, Tmax_est = NA_real_,
       r = r, r_squared = r2, F = Fv, p_value = pv,
       rss = Q, rmse = sqrt(Q / n), logLik = ll,
       aic = aic, aicc = aicc, bic = bic,
       fitted = T_hat, residuals = T_ - T_hat,
       obs_temp_range = range(T_), conf_level = conf_level)
}

## Bounded nonlinear fit via nls(port) with deterministic restarts.
## Returns NULL on failure (caller decides what to do).
## 'start' entries not belonging to the fitted model are ignored.
gdd_fit_nl <- function(T_, V_, model, start = NULL,
                       conf_level = 0.95, maxiter = 1000) {
  sb <- gdd_start_bounds(model, T_, V_)
  pn <- names(sb$start)
  st0 <- sb$start
  if (!is.null(start)) {
    keep <- names(start) %in% pn
    if (any(keep)) st0[names(start)[keep]] <- unlist(start)[keep]
  }

  clip <- function(v) pmin(pmax(v, sb$lower), sb$upper)
  ## Restarts rescale only rate-like parameters. Rescaling temperature
  ## parameters (Tm, T0, Tl, Th), width parameters (Delta) or shape
  ## exponents (m) would push the start out of its meaningful range.
  sc <- switch(model,
               logan   = c("psi", "rho"),
               lactin  = c("rho", "lambda"),
               briere1 = "a",
               briere2 = "a",
               wang    = c("K", "k3", "k4"),
               character(0))
  scale1 <- function(fac) {
    s <- st0
    hit <- names(s) %in% sc
    s[hit] <- s[hit] * fac
    s
  }
  attempts <- list(clip(st0), clip(scale1(0.5)),
                   clip(scale1(2)), clip(scale1(0.25)))
  d <- data.frame(T = T_, V = V_)
  fml <- stats::as.formula(paste("V ~", gdd_rhs[[model]]))

  ok <- list()
  for (a in attempts) {
    fit <- tryCatch(
      withCallingHandlers(
        stats::nls(fml, data = d, start = as.list(a),
                   algorithm = "port", lower = sb$lower, upper = sb$upper,
                   control = stats::nls.control(maxiter = maxiter,
                                               tol = 1e-8, warnOnly = TRUE)),
        ## nls convergence warnings are expected here: the attempt is
        ## judged by its convergence flag below, so they are muffled
        warning = function(w) {
          if (grepl("Convergence failure", conditionMessage(w)))
            invokeRestart("muffleWarning")
        }),
      error = function(e) NULL)
    if (!is.null(fit) && isTRUE(fit$convInfo$isConv))
      ok[[length(ok) + 1L]] <- fit
  }
  if (!length(ok)) return(NULL)
  fit <- ok[[which.min(vapply(ok, stats::deviance, numeric(1)))]]

  n <- length(V_); k <- length(stats::coef(fit)); df <- n - k
  if (df < 1) return(NULL)
  p_hat <- stats::coef(fit)
  se_hat <- tryCatch(sqrt(diag(stats::vcov(fit))),
                     error = function(e) rep(NA_real_, k))
  names(se_hat) <- names(p_hat)

  rss <- sum(stats::residuals(fit)^2)
  tss <- sum((V_ - mean(V_))^2)
  r2 <- if (tss > 0) 1 - rss / tss else NA_real_
  rmse <- sqrt(rss / n)
  aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
  bic <- tryCatch(stats::BIC(fit), error = function(e) NA_real_)
  ktot <- k + 1     # + sigma^2, consistent with logLik.nls
  aicc <- if (is.finite(aic) && n - ktot - 1 > 0)
    aic + 2 * ktot * (ktot + 1) / (n - ktot - 1) else Inf

  tc <- if (df > 0) stats::qt(1 - (1 - conf_level) / 2, df) else NA_real_
  tval <- ifelse(is.na(se_hat) | se_hat == 0, NA_real_, p_hat / se_hat)
  ctab <- data.frame(
    parameter = names(p_hat), estimate = p_hat, se = se_hat,
    t_value = tval,
    p_value = ifelse(is.na(tval), NA_real_,
                     2 * stats::pt(-abs(tval), df)),
    lower = p_hat - tc * se_hat, upper = p_hat + tc * se_hat,
    row.names = NULL)

  der <- gdd_derive(model, p_hat, range(T_))
  ## C has a direct SE only when it is a model parameter itself
  se_C <- switch(model,
                 briere1 = se_hat[["T0"]], briere2 = se_hat[["T0"]],
                 wang = se_hat[["Tl"]], NA_real_)

  list(model = model, label = gdd_model_label[[model]],
       n = n, k = k, df = df, params = p_hat, se = se_hat,
       coef_table = ctab,
       C = der$C, se_C = se_C, ci_C = c(NA_real_, NA_real_),
       K = NA_real_, se_K = NA_real_, ci_K = c(NA_real_, NA_real_),
       Topt = der$Topt, Vmax = der$Vmax, Tmax_est = der$Tmax_est,
       r = NA_real_, r_squared = r2, F = NA_real_, p_value = NA_real_,
       rss = rss, rmse = rmse, aic = aic, aicc = aicc, bic = bic,
       fitted = as.numeric(stats::fitted(fit)),
       residuals = as.numeric(stats::residuals(fit)),
       obs_temp_range = range(T_), conf_level = conf_level)
}

## Fit all candidate models for one group; used by "auto" and
## gdd_compare(). Never stops: failures are recorded in the table.
gdd_fit_all <- function(g, gname,
                        models = c("linear", "logan", "lactin",
                                   "briere1", "briere2", "wang"),
                        start = NULL, conf_level = 0.95, maxiter = 1000) {
  rows <- list(); fits <- list()
  for (m in models) {
    need <- gdd_min_n(m)
    f <- NULL
    if (nrow(g) >= need) {
      f <- tryCatch(
        if (m == "linear")
          gdd_fit_linear(g$temp, g$rate, conf_level)
        else gdd_fit_nl(g$temp, g$rate, m, start, conf_level, maxiter),
        error = function(e) NULL)
    }
    if (is.null(f)) {
      rows[[m]] <- data.frame(
        group = gname, model = m, n = nrow(g), converged = FALSE,
        k = NA_integer_, R2 = NA_real_, RMSE = NA_real_,
        AIC = NA_real_, AICc = NA_real_, BIC = NA_real_,
        note = if (nrow(g) < need) "insufficient n" else "fit failed")
    } else {
      fits[[m]] <- f
      rows[[m]] <- data.frame(
        group = gname, model = m, n = f$n, converged = TRUE, k = f$k,
        R2 = f$r_squared, RMSE = f$rmse,
        AIC = f$aic, AICc = f$aicc, BIC = f$bic, note = "")
    }
  }
  tab <- do.call(rbind, rows)
  ok <- tab[tab$converged, , drop = FALSE]
  cand <- ok[is.finite(ok$AICc), , drop = FALSE]
  if (!nrow(cand)) cand <- ok    # fallback if all AICc are Inf/NA
  if (!nrow(cand)) {
    tab$delta_AICc <- NA_real_; tab$best <- FALSE
    return(list(best = NULL, table = tab, fits = fits))
  }
  ## AICc is undefined (Inf) when n - k - 1 <= 0; fall back to AIC so a
  ## best model still exists for tiny samples
  crit <- cand$AICc
  if (!any(is.finite(crit))) crit <- cand$AIC
  b <- cand$model[which.min(crit)]
  fin <- is.finite(tab$AICc)
  ref <- if (any(fin)) min(tab$AICc[fin]) else min(tab$AIC, na.rm = TRUE)
  tab$delta_AICc <- tab$AICc - ref
  tab$best <- tab$model == b
  list(best = fits[[b]], table = tab, fits = fits)
}