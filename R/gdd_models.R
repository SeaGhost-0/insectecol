# ============================================================
# insectecol --- Degree-day module: model library
# Temperature-dependent development models (linear + nonlinear).
# All functions here are internal (not exported).
# ============================================================

## Right-hand sides of V(T); used to build nls() formulas and to
## evaluate fitted curves via gdd_curve().
gdd_rhs <- c(
  linear  = "(T - C) / K",
  logan   = "psi * (exp(rho * T) - exp(rho * Tm - (Tm - T) / Delta))",
  lactin  = "exp(rho * T) - exp(rho * Tm - (Tm - T) / Delta) + lambda",
  briere1 = "a * T * (T - T0) * sqrt(Tm - T)",
  briere2 = "a * T * (T - T0) * (Tm - T)^(1 / m)",
  wang    = "K / (1 + exp(k1 + k2 * T)) * (1 - exp(-k3 * (T - Tl))) * (1 - exp(-k4 * (Th - T)))"
)

gdd_params <- list(
  linear  = c("C", "K"),
  logan   = c("psi", "rho", "Tm", "Delta"),
  lactin  = c("rho", "Tm", "Delta", "lambda"),
  briere1 = c("a", "T0", "Tm"),
  briere2 = c("a", "T0", "Tm", "m"),
  wang    = c("K", "k1", "k2", "k3", "Tl", "k4", "Th")
)

gdd_model_label <- c(
  linear  = "Linear (degree-day law)",
  logan   = "Logan-6 (Logan et al. 1976)",
  lactin  = "Lactin (Lactin et al. 1995)",
  briere1 = "Briere-1 (Briere et al. 1999)",
  briere2 = "Briere-2 (Briere et al. 1999)",
  wang    = "Wang (Wang et al. 1982)"
)

## Minimum number of temperature points required by each model
gdd_min_n <- function(model) {
  if (model == "linear") 3L else length(gdd_params[[model]]) + 2L
}

## Fitted curve: returns function(T) given a named parameter vector
gdd_curve <- function(model, p) {
  rhs <- parse(text = gdd_rhs[[model]])[[1]]
  function(T) eval(rhs, envir = c(as.list(p), list(T = T)))
}

## Internal: safe univariate root search (NA if no sign change)
gdd_safe_root <- function(f, lo, hi) {
  tryCatch(stats::uniroot(f, lower = lo, upper = hi)$root,
           error = function(e) NA_real_)
}

## Heuristic starting values and box constraints for nls(port)
gdd_start_bounds <- function(model, T, V) {
  Tmin <- min(T); Tmax <- max(T)
  i <- which.max(V); Tstar <- T[i]; Vstar <- V[i]
  switch(model,
    linear = NULL,
    logan = {
      rho0 <- 0.10; Tm0 <- Tmax + 3; D0 <- 4
      den <- exp(rho0 * Tstar) -
             exp(rho0 * Tm0 - (Tm0 - Tstar) / D0)
      psi0 <- if (is.finite(den) && den > 1e-8) Vstar / den else 1e-3
      list(start = c(psi = psi0, rho = rho0, Tm = Tm0, Delta = D0),
           lower = c(psi = 1e-8, rho = 0.005, Tm = Tmax, Delta = 0.1),
           upper = c(psi = 1e6,  rho = 1.5,   Tm = Tmax + 20, Delta = 50))
    },
    lactin = {
      rho0 <- 0.10; Tm0 <- Tmax + 3; D0 <- 4
      den <- exp(rho0 * Tstar) -
             exp(rho0 * Tm0 - (Tm0 - Tstar) / D0)
      lam0 <- if (is.finite(den)) Vstar - den else -1  # pass through max point
      list(start = c(rho = rho0, Tm = Tm0, Delta = D0, lambda = lam0),
           lower = c(rho = 0.005, Tm = Tmax, Delta = 0.1, lambda = -50),
           upper = c(rho = 1.5,   Tm = Tmax + 20, Delta = 50, lambda = 0.01))
    },
    briere1 = {
      T00 <- Tmin - 2; Tm0 <- Tmax + 2
      den <- Tstar * (Tstar - T00) * sqrt(Tm0 - Tstar)
      a0 <- if (is.finite(den) && den > 1e-8) Vstar / den else 1e-4
      list(start = c(a = a0, T0 = T00, Tm = Tm0),
           lower = c(a = 1e-12, T0 = Tmin - 15, Tm = Tmax),
           upper = c(a = 10,    T0 = Tmin + 2,  Tm = Tmax + 20))
    },
    briere2 = {
      T00 <- Tmin - 2; Tm0 <- Tmax + 2; m0 <- 2
      den <- Tstar * (Tstar - T00) * (Tm0 - Tstar)^(1 / m0)
      a0 <- if (is.finite(den) && den > 1e-8) Vstar / den else 1e-4
      list(start = c(a = a0, T0 = T00, Tm = Tm0, m = m0),
           lower = c(a = 1e-12, T0 = Tmin - 15, Tm = Tmax, m = 0.5),
           upper = c(a = 10,    T0 = Tmin + 2,  Tm = Tmax + 20, m = 10))
    },
    wang = {
      K0 <- 1.2 * max(V); k2_0 <- -0.25
      k1_0 <- -4 - k2_0 * Tmax
      list(start = c(K = K0, k1 = k1_0, k2 = k2_0, k3 = 0.15,
                     Tl = Tmin - 3, k4 = 0.15, Th = Tmax + 3),
           lower = c(K = 1e-8, k1 = -50, k2 = -5, k3 = 1e-4,
                     Tl = Tmin - 15, k4 = 1e-4, Th = Tmax),
           upper = c(K = 1e6, k1 = 50, k2 = -0.005, k3 = 10,
                     Tl = Tmin + 2, k4 = 10, Th = Tmax + 25))
    }
  )
}

## Derived quantities: lower threshold C, optimum Topt, upper threshold
## Tmax_est, maximal rate Vmax. NA where not defined by the model.
gdd_derive <- function(model, p, obs_range) {
  cf <- gdd_curve(model, p)
  out <- list(C = NA_real_, Topt = NA_real_,
              Vmax = NA_real_, Tmax_est = NA_real_)
  switch(model,
    linear  = { out$C <- p[["C"]] },
    logan   = {                     # Logan-6 never crosses zero at low T
      out$Tmax_est <- p[["Tm"]]
    },
    lactin  = {
      out$C <- gdd_safe_root(cf, obs_range[1] - 15, obs_range[1] + 2)
      out$Tmax_est <- gdd_safe_root(cf, p[["Tm"]] - 15, p[["Tm"]])
    },
    briere1 =, briere2 = {
      out$C <- p[["T0"]]; out$Tmax_est <- p[["Tm"]]
    },
    wang    = {
      out$C <- p[["Tl"]]; out$Tmax_est <- p[["Th"]]
    })
  if (model != "linear") {
    lo <- if (is.finite(out$C)) out$C + 0.25 else obs_range[1] - 10
    hi <- if (is.finite(out$Tmax_est)) out$Tmax_est - 0.25 else obs_range[2] + 5
    if (is.finite(lo) && is.finite(hi) && hi > lo) {
      o <- stats::optimize(cf, c(lo, hi), maximum = TRUE)
      out$Topt <- o$maximum; out$Vmax <- o$objective
    }
  }
  out
}