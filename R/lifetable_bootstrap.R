# ==================== lifetable_bootstrap.R ====================
# Bootstrap standard errors and paired bootstrap tests for the
# age-stage, two-sex life table.
#
# The resampling scheme follows 'TWOSEX-MSChart' (Chi & Liu 1985;
# Chi 1988; see also Meyer et al. 1986 and Huang & Chi 2012):
#   * the bootstrap unit is the COMPLETE RECORD of one individual
#     (stage durations + adult days + sex + daily oviposition),
#   * individuals that died before the adult stage are resampled
#     as well (they contribute zero fecundity and their mortality),
#   * every resampled cohort has the same size as the original one.
# The standard error of a parameter is the standard deviation of the
# B bootstrap replicates and the confidence interval is the percentile
# interval. Two cohorts are compared with the paired bootstrap test
# based on the confidence interval of the difference.

## ---------- internal: extract the per-individual records ----------

# Extracts everything the bootstrap needs from a life_table object as
# per-individual matrices, so that a bootstrap replicate only has to
# reweight rows. The egg matrix follows exactly the alignment used by
# .calc_fxj_raw(): the k-th oviposition record of a female is placed
# at absolute age (total immature duration + k).
.boot_extract <- function(lt) {
  d <- lt$data
  n_col <- lt$n
  N <- nrow(d)
  k <- n_col - 3L                        # number of immature stages
  if (k < 1L) stop("The data contain no immature stage columns")

  sex <- as.character(d[[n_col]])
  if (anyNA(sex) || !all(sex %in% c("F", "M", "N")))
    stop("The sex column may only contain F, M or N")

  to_num <- function(j) suppressWarnings(as.numeric(as.character(d[[j]])))
  stage_mat <- vapply(seq_len(k), function(j) to_num(j + 1L), numeric(N))
  dimnames(stage_mat) <- NULL
  adult <- to_num(n_col - 1L)

  is_f <- sex == "F"
  is_m <- sex == "M"
  imm <- rowSums(stage_mat, na.rm = TRUE)        # immature duration
  adult0 <- ifelse(is.na(adult), 0, adult)
  longevity <- imm + adult0
  X <- max(1L, as.integer(max(longevity, 0)))    # maximum age (days)

  ## per-individual egg matrix (N x X): eggs laid at absolute age a
  has_ovi <- ncol(d) >= lt$n_1
  eggs <- matrix(0, N, X)
  if (has_ovi) {
    ovi <- vapply(lt$n_1:ncol(d), to_num, numeric(N))
    dimnames(ovi) <- NULL
    for (i in which(is_f)) {
      vals <- ovi[i, ]
      vals <- vals[!is.na(vals)]
      if (length(vals)) {
        a_i <- imm[i] + seq_along(vals)
        keep <- a_i >= 1 & a_i <= X
        eggs[i, a_i[keep]] <- vals[keep]
      }
    }
  }

  stage_names <- get_stage_names(lt)
  if (length(stage_names) != k + 2L) {
    warning("The stage names could not be extracted from the header; default names are used")
    stage_names <- c(default_stage_names(k), "Female", "Male")
  }

  list(N = N, k = k, X = X, sex = sex, is_f = is_f, is_m = is_m,
       stage_mat = stage_mat, adult = adult, imm = imm,
       longevity = longevity, eggs = eggs, has_ovi = has_ovi,
       stage_names = stage_names[seq_len(k)],
       total_eggs = rowSums(eggs))
}

## ---------- internal: solve the Euler-Lotka equation ----------

# Vectorised bisection for many bootstrap replicates at once. Bx is a
# c x X matrix of B_x values (total eggs laid at age x in each
# replicate); the equation is sum_a exp(-r * a) * B_a = N with age a
# indexed from 1, which is algebraically identical to the
# sum(exp(-r * (x + 1)) * l_x * m_x) = 1 solved by calc_r() because
# l_x * m_x = B_x / N (the same age-from-1 discounting as
# 'TWOSEX-MSChart'). The search interval is widened automatically
# so that declining cohorts (R0 < 1, i.e. r < 0) are
# handled as well; the exponent is capped to keep the bisection free
# of Inf/NaN at the probe points. Replicates without any offspring
# return NA.
.boot_solve_r <- function(Bx, N) {
  out <- rep(NA_real_, nrow(Bx))
  if (nrow(Bx) == 0L || ncol(Bx) == 0L) return(out)
  tot <- rowSums(Bx)
  pos <- which(tot > 0)
  if (!length(pos)) return(out)

  Bxa <- Bx[pos, , drop = FALSE]
  act <- colSums(Bxa) > 0
  if (!any(act)) return(out)
  Bxa <- Bxa[, act, drop = FALSE]
  ages <- seq_len(ncol(Bx))[act]  # day index a, running from 1

  bound <- abs(log(tot[pos] / N)) + 3
  lo <- -bound
  hi <- bound
  for (it in seq_len(60)) {
    mid <- (lo + hi) / 2
    E <- exp(pmin(-outer(mid, ages), 700))
    fm <- rowSums(Bxa * E) - N          # f is decreasing in r
    neg <- fm < 0
    hi[neg] <- mid[neg]
    lo[!neg] <- mid[!neg]
  }
  out[pos] <- (lo + hi) / 2
  out
}

## ---------- internal: run the bootstrap ----------

# Returns the raw B x p replicate matrix plus the summary. Parameter
# names deliberately match the column names used by
# lifeTable_calculate_all() / lifeTable_export() so that standard errors
# can be attached to the summary tables with a simple _SE suffix.
.boot_run <- function(ext, B, conf.level) {
  N <- ext$N
  k <- ext$k
  eggs <- ext$eggs
  is_f_d <- as.numeric(ext$is_f)
  is_m_d <- as.numeric(ext$is_m)
  adult <- ext$adult
  stage_mat <- ext$stage_mat

  stage0 <- stage_mat
  stage0[is.na(stage0)] <- 0
  entered <- matrix(as.numeric(!is.na(stage_mat)), N, k)
  af <- adult * ext$is_f                   # female adult days (0 elsewhere)
  af[is.na(af)] <- 0
  am <- adult * ext$is_m
  am[is.na(am)] <- 0

  stage_nm <- ext$stage_names
  pars <- c(paste0("Developmental_time_", stage_nm),
            "Adult_longevity_Female", "Adult_longevity_Male")
  if (ext$has_ovi)
    pars <- c(pars, "Mean_fecundity_F", "Net_reproductive_rate_R0",
              "Intrinsic_rate_of_increase_r",
              "Finite_rate_of_increase_lambda", "Mean_generation_time_T")
  p <- length(pars)
  boot_mat <- matrix(NA_real_, B, p, dimnames = list(NULL, pars))

  chunk <- max(1L, min(10000L, as.integer(2e7 / max(N, 1L))))
  n_ch <- ceiling(B / chunk)
  for (ch in seq_len(n_ch)) {
    b0 <- (ch - 1L) * chunk
    c <- min(chunk, B - b0)
    idx <- matrix(sample.int(N, c * N, replace = TRUE), nrow = c)
    W <- t(vapply(seq_len(c), function(b) tabulate(idx[b, ], nbins = N),
                  integer(N)))            # c x N bootstrap weights
    storage.mode(W) <- "double"

    ## stage durations: mean over the individuals that entered the stage
    num <- W %*% stage0
    den <- W %*% entered
    stage_b <- num / den
    stage_b[!is.finite(stage_b)] <- NA_real_

    ## adult longevities (mean over the females / males of the replicate)
    nf <- as.vector(W %*% is_f_d)
    nm <- as.vector(W %*% is_m_d)
    fl_b <- ifelse(nf > 0, as.vector(W %*% af) / pmax(nf, 1), NA_real_)
    ml_b <- ifelse(nm > 0, as.vector(W %*% am) / pmax(nm, 1), NA_real_)

    ## fecundity-related parameters
    if (ext$has_ovi) {
      Bx <- W %*% eggs                    # c x X eggs laid at each age
      tot <- rowSums(Bx)
      R0_b <- tot / N
      F_b <- ifelse(nf > 0, tot / pmax(nf, 1), NA_real_)
      r_b <- .boot_solve_r(Bx, N)
      lam_b <- ifelse(is.finite(r_b), exp(r_b), NA_real_)
      T_b <- ifelse(is.finite(r_b) & R0_b > 0 & abs(r_b) > 1e-12,
                    log(R0_b) / r_b, NA_real_)
    }

    i0 <- b0 + seq_len(c)
    boot_mat[i0, seq_len(k)] <- stage_b
    boot_mat[i0, k + 1L] <- fl_b
    boot_mat[i0, k + 2L] <- ml_b
    if (ext$has_ovi) {
      boot_mat[i0, k + 3L] <- F_b
      boot_mat[i0, k + 4L] <- R0_b
      boot_mat[i0, k + 5L] <- r_b
      boot_mat[i0, k + 6L] <- lam_b
      boot_mat[i0, k + 7L] <- T_b
    }
  }

  ## ---- original point estimates (weights = identity) ----
  .mean_or_na <- function(v) {
    v <- v[is.finite(v)]
    if (length(v)) mean(v) else NA_real_
  }
  orig_stage <- suppressWarnings(colMeans(stage_mat, na.rm = TRUE))
  orig_stage[is.nan(orig_stage)] <- NA_real_
  orig <- c(orig_stage,
            .mean_or_na(adult[ext$is_f]),
            .mean_or_na(adult[ext$is_m]))
  if (ext$has_ovi) {
    tot0 <- sum(ext$total_eggs)
    nf0 <- sum(ext$is_f)
    R0_0 <- tot0 / N
    r_0 <- if (tot0 > 0)
      .boot_solve_r(matrix(colSums(eggs), nrow = 1), N)[1] else NA_real_
    orig <- c(orig,
              if (nf0 > 0) tot0 / nf0 else NA_real_,
              R0_0,
              r_0,
              if (is.finite(r_0)) exp(r_0) else NA_real_,
              if (is.finite(r_0) && R0_0 > 0 && abs(r_0) > 1e-12)
                log(R0_0) / r_0 else NA_real_)
  }

  ## ---- summary: bootstrap mean, SE and percentile CI ----
  alpha <- 1 - conf.level
  qs <- c(alpha / 2, 1 - alpha / 2)
  est <- vapply(seq_len(p), function(j) .mean_or_na(boot_mat[, j]), numeric(1))
  se <- vapply(seq_len(p), function(j) {
    v <- boot_mat[, j]; v <- v[is.finite(v)]
    if (length(v) > 1L) sd(v) else NA_real_
  }, numeric(1))
  ci <- vapply(seq_len(p), function(j) {
    v <- boot_mat[, j]; v <- v[is.finite(v)]
    if (length(v) > 1L) quantile(v, probs = qs, names = FALSE)
    else c(NA_real_, NA_real_)
  }, numeric(2))
  n_valid <- vapply(seq_len(p), function(j) sum(is.finite(boot_mat[, j])),
                    integer(1))

  summary_df <- data.frame(
    Parameter = pars,
    Original = unname(orig),
    Boot_mean = est,
    Boot_SE = se,
    CI_low = ci[1, ],
    CI_high = ci[2, ],
    Valid_B = n_valid,
    row.names = NULL)

  structure(
    list(summary = summary_df, boot = boot_mat, B = B,
         conf.level = conf.level, N = N, parameters = pars),
    class = "life_table_boot")
}

#' Bootstrap Standard Errors of Life Table Parameters
#'
#' Estimates the standard errors and percentile confidence intervals
#' of all scalar life table parameters with the bootstrap technique
#' used by 'TWOSEX-MSChart': complete individual records (stage
#' durations, adult days, sex and daily oviposition) are resampled
#' with replacement \code{B} times, and every parameter is recomputed
#' from each resampled cohort. Individuals that died before the adult
#' stage are resampled as well - they carry their mortality and zero
#' fecundity into the replicates.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{lifeTable_read}} or \code{\link{lifeTable_build}}.
#' @param B Integer; number of bootstrap replicates. The published
#'   'TWOSEX-MSChart' standard is \code{100000} (the default); smaller
#'   values run faster but give rougher standard errors.
#' @param seed Integer; seed for the random number generator. Set it
#'   to make the results exactly reproducible; \code{NULL} (default)
#'   uses the current R session state. The session state is restored
#'   when the function exits.
#' @param conf.level Numeric; confidence level of the percentile
#'   intervals (default \code{0.95}).
#'
#' @details The parameters covered are the mean developmental time of
#'   every immature stage (averaged over the individuals that entered
#'   the stage, including those that died during it), the female and
#'   male adult longevity, and - when oviposition data are present -
#'   the mean fecundity F, the net reproductive rate R0, the intrinsic
#'   rate of increase r, the finite rate of increase lambda and the
#'   mean generation time T. The intrinsic rate is solved per
#'   replicate from the Euler-Lotka equation with the same age
#'   convention as \code{\link{calc_r}}, but with a widened search
#'   interval so that declining cohorts (R0 < 1, i.e. r < 0) are also
#'   handled. Replicates in which no female is drawn have \code{NA}
#'   fecundity, and replicates without any offspring have \code{NA}
#'   \code{r}, \code{lambda} and \code{T}; such replicates are
#'   excluded from the means, standard errors and intervals of the
#'   affected parameters (column \code{Valid_B} shows how many
#'   remained). Curve-type outputs (s_xj, l_x, m_x, e_x) are not
#'   bootstrapped.
#'
#'   Attach the result to an analysis to have it exported by
#'   \code{\link{lifeTable_export}}:
#'   \code{results$boot <- lifeTable_bootstrap(lt)}.
#'
#' @return An object of class \code{life_table_boot}: a list with
#'   \item{summary}{data frame; one row per parameter with the
#'     original point estimate, the bootstrap mean, the bootstrap
#'     standard error and the percentile confidence interval}
#'   \item{boot}{numeric matrix; the raw B x p replicate values}
#'   \item{B, conf.level, N, parameters}{the settings used}
#'
#' @references
#' Meyer, J. S., Ingersoll, C. G., McDonald, L. L. and Boyce, M. S.
#' (1986) Estimating uncertainty in population growth rates: jackknife
#' vs. bootstrap. \emph{Ecological Modelling} 29, 251-271.
#'
#' Efron, B. and Tibshirani, R. J. (1993) \emph{An Introduction to the
#' Bootstrap}. New York: Chapman and Hall.
#'
#' Chi, H., You, M. S., Atlihan, R., Smith, C. L., Kavousi, A.,
#' Ozgokce, M. S., Guncan, A. and Tuan, S. J. (2020) Age-stage,
#' two-sex life table: an introduction to theory, data analysis, and
#' application. \emph{Entomologia Generalis} 40(2), 103-124.
#'
#' @seealso \code{\link{lifeTable_boot_test}} for the paired
#'   bootstrap test between two cohorts,
#'   \code{\link{lifeTable_calculate_all}} for the point estimates.
#' @importFrom stats sd quantile
#' @export
#' @examples
#' f <- system.file("extdata", "lifetable_example.csv", package = "insectecol")
#' lt <- lifeTable_read(f)
#'
#' ## B = 100000 is the recommended setting for publications; a smaller
#' ## B is used here so that the example runs fast
#' bt <- lifeTable_bootstrap(lt, B = 2000, seed = 1)
#' bt$summary
lifeTable_bootstrap <- function(lt, B = 100000, seed = NULL,
                                conf.level = 0.95) {
  B <- as.integer(B[1])
  if (is.na(B) || B < 2L) stop("B must be a single integer >= 2")
  if (length(conf.level) != 1 || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("conf.level must be a single number in (0, 1)")
  if (!inherits(lt, "life_table"))
    stop("lt must be a life_table object, as returned by lifeTable_read() or lifeTable_build()")

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed)
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    set.seed(seed[1])
    on.exit({
      if (had_seed)
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
  }

  ext <- .boot_extract(lt)
  message(sprintf("Bootstrap running: B = %d replicates, cohort size N = %d ...",
                  B, ext$N))
  .boot_run(ext, B, conf.level)
}

#' Paired Bootstrap Test Between Two Life Tables
#'
#' Compares the life table parameters of two cohorts with the paired
#' bootstrap test used by 'TWOSEX-MSChart': both cohorts are resampled
#' independently \code{B} times, the differences
#' \code{d = parameter(group 1) - parameter(group 2)} are formed
#' replicate by replicate, and the 95 percent percentile interval of
#' the differences is inspected. Following the convention of the
#' life table literature, the difference is considered significant
#' when the confidence interval of the difference does not include
#' zero.
#'
#' @param lt1,lt2 \code{life_table} objects (e.g. two treatments or
#'   two host plants) returned by \code{\link{lifeTable_read}} or
#'   \code{\link{lifeTable_build}}.
#' @param B Integer; number of bootstrap replicates per group
#'   (default \code{100000}, the 'TWOSEX-MSChart' standard).
#' @param seed Integer; seed for the random number generator; the
#'   session state is restored when the function exits.
#' @param conf.level Numeric; confidence level of the intervals of
#'   the differences (default \code{0.95}).
#'
#' @details The two cohorts are resampled independently, so they do
#'   not need to have the same number of individuals. The parameters
#'   compared are those of \code{\link{lifeTable_bootstrap}} that
#'   occur in BOTH cohorts; if the stage structures differ, only the
#'   common parameters are compared and a warning is issued. A
#'   bootstrap p-value is reported in addition to the interval test:
#'   \code{2 * min(P(d <= 0), P(d >= 0))} over the valid replicates.
#'   Replicates in which a parameter is undefined in either group
#'   (e.g. no female drawn) are dropped from the comparison.
#'
#' @return A data frame with one row per compared parameter:
#'   \item{Parameter}{parameter name (as in
#'     \code{\link{lifeTable_bootstrap}})}
#'   \item{Boot_mean_1, Boot_mean_2}{bootstrap means of the two
#'     groups}
#'   \item{Diff}{mean of the bootstrap differences (group 1 minus
#'     group 2)}
#'   \item{CI_low, CI_high}{percentile interval of the differences}
#'   \item{Significant}{logical; \code{TRUE} when the interval
#'     excludes zero}
#'   \item{P_bootstrap}{two-sided bootstrap p-value}
#'   The attribute \code{groups} contains the file names of the two
#'   cohorts.
#'
#' @references
#' Meyer, J. S., Ingersoll, C. G., McDonald, L. L. and Boyce, M. S.
#' (1986) Estimating uncertainty in population growth rates: jackknife
#' vs. bootstrap. \emph{Ecological Modelling} 29, 251-271.
#'
#' Chi, H., You, M. S., Atlihan, R., Smith, C. L., Kavousi, A.,
#' Ozgokce, M. S., Guncan, A. and Tuan, S. J. (2020) Age-stage,
#' two-sex life table: an introduction to theory, data analysis, and
#' application. \emph{Entomologia Generalis} 40(2), 103-124.
#'
#' @seealso \code{\link{lifeTable_bootstrap}} for the standard errors
#'   of a single cohort.
#' @export
#' @examples
#' f <- system.file("extdata", "lifetable_example.csv", package = "insectecol")
#' lt1 <- lifeTable_read(f)
#'
#' ## compare the full cohort with its first half (demo only - a real
#' ## comparison would use two different treatments)
#' lt2 <- lt1
#' lt2$data <- lt1$data[1:12, ]
#' rownames(lt2$data) <- NULL
#' lt2$file_name <- "Example (first half)"
#'
#' ## B = 100000 is the recommended setting for publications; a smaller
#' ## B is used here so that the example runs fast
#' lifeTable_boot_test(lt1, lt2, B = 2000, seed = 1)
lifeTable_boot_test <- function(lt1, lt2, B = 100000, seed = NULL,
                                 conf.level = 0.95) {
  B <- as.integer(B[1])
  if (is.na(B) || B < 2L) stop("B must be a single integer >= 2")
  if (length(conf.level) != 1 || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("conf.level must be a single number in (0, 1)")
  for (lt in list(lt1, lt2))
    if (!inherits(lt, "life_table"))
      stop("lt1 and lt2 must be life_table objects, as returned by lifeTable_read() or lifeTable_build()")

  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed)
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    set.seed(seed[1])
    on.exit({
      if (had_seed)
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
  }

  message(sprintf("Paired bootstrap test running: B = %d replicates per group ...", B))
  r1 <- .boot_run(.boot_extract(lt1), B, conf.level)
  r2 <- .boot_run(.boot_extract(lt2), B, conf.level)

  common <- intersect(r1$parameters, r2$parameters)
  if (!length(common))
    stop("The two life tables have no parameters in common")
  if (!setequal(r1$parameters, r2$parameters))
    warning("The stage structures of the two cohorts differ; only the common parameters are compared")

  alpha <- 1 - conf.level
  qs <- c(alpha / 2, 1 - alpha / 2)
  b1 <- r1$boot[, common, drop = FALSE]
  b2 <- r2$boot[, common, drop = FALSE]

  out <- do.call(rbind, lapply(common, function(nm) {
    d <- b1[, nm] - b2[, nm]
    v1 <- b1[, nm]; v2 <- b2[, nm]
    ok <- is.finite(v1) & is.finite(v2)
    d <- d[ok]; v1 <- v1[ok]; v2 <- v2[ok]
    if (!length(d)) {
      row <- c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA, NA_real_)
    } else {
      ci <- quantile(d, probs = qs, names = FALSE)
      pv <- 2 * min(mean(d <= 0), mean(d >= 0))
      row <- c(mean(v1), mean(v2), mean(d), ci[1], ci[2],
               !(ci[1] <= 0 && ci[2] >= 0), min(1, pv))
    }
    data.frame(t(row))
  }))
  names(out) <- c("Boot_mean_1", "Boot_mean_2", "Diff",
                  "CI_low", "CI_high", "Significant", "P_bootstrap")
  out <- cbind(Parameter = common, out, row.names = NULL)
  out$Significant <- as.logical(out$Significant)
  attr(out, "groups") <- c(lt1$file_name, lt2$file_name)
  out
}

#' @export
print.life_table_boot <- function(x, digits = 4, ...) {
  cat(sprintf("Bootstrap of age-stage, two-sex life table (B = %d, %.0f%% percentile CI)\n\n",
              x$B, 100 * x$conf.level))
  print(x$summary, digits = digits, row.names = FALSE)
  invisible(x)
}
