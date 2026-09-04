#' LC Estimation by the Traditional Linear Regression Method
#'
#' Estimates the lethal concentration by fitting an ordinary (unweighted)
#' least-squares line to the probit-transformed mortality.
#'
#' @param d A data frame with the columns \code{Concentration},
#'   \code{Tested} and \code{Dead}, as returned by
#'   \code{\link{read_lc50}}; rows with \code{Concentration = 0} are
#'   treated as the control group.
#' @param lc Numeric; the lethal proportion for which the concentration
#'   is estimated. The default 0.5 gives the LC50, 0.9 the LC90.
#'
#' @details The corrected mortalities are transformed to probits
#'   (\code{y = qnorm(p) + 5}) and the concentrations to common
#'   logarithms (\code{x = log10(concentration)}); the line
#'   \code{y = a + b * x} is fitted by ordinary least squares with all
#'   points weighted equally. Inverting the line at the probit that
#'   corresponds to the requested lethal proportion gives
#'   \code{LC = 10^((y0 - a) / b)} with \code{y0 = qnorm(lc) + 5}. The
#'   95% confidence interval is computed with the delta method from the
#'   covariance matrix of the regression coefficients on the log scale
#'   and back-transformed to the concentration scale. A Pearson
#'   chi-square goodness-of-fit test of the observed against the fitted
#'   mortalities is attached.
#'
#'   Mortalities are corrected for natural mortality in the control
#'   group with the Abbott (1925) formula, and concentrations with a
#'   corrected mortality of exactly 0% or 100% (whose probits are
#'   undefined) are dropped before fitting; at least 3 valid
#'   concentrations are required.
#'
#' @return A list with elements
#'   \item{method}{name of the estimation method}
#'   \item{lc}{the lethal proportion used}
#'   \item{estimate}{the LC estimate}
#'   \item{lower, upper}{limits of the 95\% confidence interval}
#'   \item{intercept, slope}{the regression coefficients a and b}
#'   \item{se_slope}{standard error of the slope}
#'   \item{equation}{the regression equation as a character string}
#'   \item{r2}{coefficient of determination}
#'   \item{chisq, chi_df, p_chi}{Pearson chi-square statistic, degrees
#'     of freedom and p value of the goodness-of-fit test}
#'   \item{n_groups}{number of concentration groups used in the fit}
#'   \item{dropped}{number of concentration groups dropped}
#'   \item{fit}{the fitted model object}
#'   \item{prep}{the preprocessed data}
#'
#' @references
#' Finney, D. J. (1971) \emph{Probit Analysis}, 3rd edition. Cambridge
#' University Press, Cambridge.
#'
#' Abbott, W. S. (1925) A method of computing the effectiveness of an
#' insecticide. \emph{Journal of Economic Entomology} 18(2), 265-267.
#'
#' @seealso \code{\link{lc50_improved}} for the weighted version,
#'   \code{\link{lc50_probit}} for the maximum-likelihood version,
#'   \code{\link{lc50_calculate}} for the batch workflow.
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' lcd <- read_lc50(f)
#' lc50_traditional(lcd$bioassay)
#' lc50_traditional(lcd$bioassay, lc = 0.9)  # LC90
lc50_traditional <- function(d, lc = 0.5) {
  lc50_fit_ols(d, lc, weighted = FALSE)
}

#' LC Estimation by the Improved Linear Regression Method
#'
#' Estimates the lethal concentration with a weighted least-squares line:
#' the same probit-log transformation as the traditional method, but
#' each concentration is weighted by the inverse of the variance of its
#' observed mortality, which makes the regression more robust.
#'
#' @param d Same as \code{\link{lc50_traditional}}.
#' @param lc Same as \code{\link{lc50_traditional}}.
#'
#' @details The weights are the binomial weights
#'   \code{w = n * p * (1 - p) / phi(z)^2}, where \code{n} is the number
#'   of insects tested, \code{p} the corrected mortality and \code{z =
#'   qnorm(p)} the corresponding standard normal quantile. Because the
#'   variance of a probit is smallest at intermediate mortalities,
#'   concentrations with mortalities near 50% and large sample sizes
#'   dominate the fit, while near-0% and near-100% concentrations get
#'   little weight. The LC value, its delta-method confidence interval
#'   and the goodness-of-fit test are computed exactly as in
#'   \code{\link{lc50_traditional}}.
#'
#' @return Same as \code{\link{lc50_traditional}}.
#'
#' @references
#' Finney, D. J. (1971) \emph{Probit Analysis}, 3rd edition. Cambridge
#' University Press, Cambridge.
#'
#' @seealso \code{\link{lc50_traditional}}, \code{\link{lc50_probit}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' lcd <- read_lc50(f)
#' lc50_improved(lcd$bioassay)
#' lc50_improved(lcd$bioassay, lc = 0.9)  # LC90
lc50_improved <- function(d, lc = 0.5) {
  lc50_fit_ols(d, lc, weighted = TRUE)
}

#' LC Estimation by Probit Analysis (Maximum Likelihood)
#'
#' Estimates the lethal concentration by maximum-likelihood probit
#' analysis in the sense of Finney: a binomial generalized linear model
#' with probit link fitted by iteratively reweighted least squares.
#'
#' @param d Same as \code{\link{lc50_traditional}}.
#' @param lc Same as \code{\link{lc50_traditional}}.
#'
#' @details Unlike the two regression methods, the model is fitted to the
#'   raw dead/tested counts (after Abbott correction of the mortalities)
#'   rather than to transformed points, so no information is lost and no
#'   group needs to be excluded because of an extreme mortality. A
#'   quasibinomial family is used, so the covariance matrix of the
#'   coefficients incorporates the heterogeneity factor (Pearson
#'   chi-square divided by the residual degrees of freedom): the
#'   confidence intervals are automatically widened when the data show
#'   more variation than the binomial assumption allows. The reported
#'   chi-square statistic and its p value serve as a goodness-of-fit
#'   test of the probit-log concentration line.
#'
#' @return Same as \code{\link{lc50_traditional}} (with \code{fit} being
#'   the fitted glm object).
#'
#' @references
#' Finney, D. J. (1971) \emph{Probit Analysis}, 3rd edition. Cambridge
#' University Press, Cambridge.
#'
#' @seealso \code{\link{lc50_traditional}}, \code{\link{lc50_improved}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' lcd <- read_lc50(f)
#' lc50_probit(lcd$bioassay)
#' lc50_probit(lcd$bioassay, lc = 0.9)  # LC90
lc50_probit <- function(d, lc = 0.5) {
  prep <- lc50_prepare(d)
  dat <- data.frame(x = log10(prep[["Concentration"]]),
                    y = attr(prep, "p"),
                    n = prep[["Tested"]])
  fit <- stats::glm(y ~ x, family = stats::quasibinomial(link = "probit"),
                    weights = n, data = dat)
  cf <- stats::coef(fit)
  a <- unname(cf[1]) + 5
  b <- unname(cf[2])
  if (b <= 0) warning("Regression slope <= 0; please check the concentration series")
  V <- stats::vcov(fit)
  inv <- lc50_invert(a, b, V, lc, df = fit$df.residual)
  chi2 <- sum(stats::residuals(fit, type = "pearson")^2)
  df_chi <- fit$df.residual
  pv <- if (df_chi > 0) stats::pchisq(chi2, df_chi, lower.tail = FALSE) else NA_real_
  list(
    method = "Probit analysis",
    lc = lc,
    estimate = inv$lc_value,
    lower = inv$lower,
    upper = inv$upper,
    intercept = a,
    slope = b,
    se_slope = sqrt(V[2, 2]),
    equation = sprintf("y = %.4f + %.4f x lg(C)", a, b),
    r2 = 1 - fit$deviance / fit$null.deviance,
    chisq = chi2,
    chi_df = df_chi,
    p_chi = pv,
    n_groups = nrow(dat),
    dropped = attr(prep, "dropped"),
    fit = fit,
    prep = prep
  )
}

# Internal: shared implementation of OLS / weighted OLS
lc50_fit_ols <- function(d, lc, weighted) {
  prep <- lc50_prepare(d)
  x <- log10(prep[["Concentration"]])
  p <- attr(prep, "p")
  y <- stats::qnorm(p) + 5
  n <- prep[["Tested"]]
  if (weighted) {
    w <- n * p * (1 - p) / stats::dnorm(stats::qnorm(p))^2
    fit <- stats::lm(y ~ x, weights = w)
  } else {
    fit <- stats::lm(y ~ x)
  }
  cf <- stats::coef(fit)
  a <- unname(cf[1])
  b <- unname(cf[2])
  if (b <= 0) warning("Regression slope <= 0; please check the concentration series")
  V <- stats::vcov(fit)
  inv <- lc50_invert(a, b, V, lc, df = fit$df.residual)
  p_hat <- stats::pnorm(stats::fitted(fit) - 5)
  chi2 <- sum(n * (p - p_hat)^2 / (p_hat * (1 - p_hat)))
  df_chi <- length(p) - 2
  pv <- if (df_chi > 0) stats::pchisq(chi2, df_chi, lower.tail = FALSE) else NA_real_
  list(
    method = if (weighted) "Improved linear regression" else "Traditional linear regression",
    lc = lc,
    estimate = inv$lc_value,
    lower = inv$lower,
    upper = inv$upper,
    intercept = a,
    slope = b,
    se_slope = sqrt(V[2, 2]),
    equation = sprintf("y = %.4f + %.4f x lg(C)", a, b),
    r2 = summary(fit)$r.squared,
    chisq = chi2,
    chi_df = df_chi,
    p_chi = pv,
    n_groups = length(x),
    dropped = attr(prep, "dropped"),
    fit = fit,
    prep = prep
  )
}

# Internal: invert the regression for the LC value + delta-method 95% CI
# x0 = (y0-a)/b, Var(x0) = (Va + x0^2*Vb + 2*x0*Cab) / b^2
lc50_invert <- function(a, b, V, lc, df) {
  y0 <- stats::qnorm(lc) + 5
  x0 <- (y0 - a) / b
  Va <- V[1, 1]
  Vb <- V[2, 2]
  Cab <- V[1, 2]
  var_x0 <- (Va + x0^2 * Vb + 2 * x0 * Cab) / b^2
  se <- sqrt(var_x0)
  tcrit <- stats::qt(0.975, df = df)
  ci <- sort(10^(x0 + c(-1, 1) * tcrit * se))
  list(lc_value = 10^x0, lower = ci[1], upper = ci[2])
}
