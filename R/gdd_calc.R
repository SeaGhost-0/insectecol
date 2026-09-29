# ============================================================
# insectecol --- Degree-day module: main entry
# Linear (default) and nonlinear temperature-development models.
# ============================================================

#' Compute Effective Accumulated Temperature and Developmental
#' Parameters (Linear or Nonlinear Models)
#'
#' Fits a temperature-dependent development model to constant-
#' temperature data. By default the classic linear degree-day model
#' \deqn{V = (T - C) / K}{V = (T - C) / K} is fitted by ordinary least
#' squares, yielding the developmental threshold temperature \eqn{C}
#' (deg C) and the effective accumulated temperature \eqn{K}
#' (degree-days). Nonlinear models describing the full response curve
#' (optimum and upper threshold included) are available via
#' \code{model}.
#'
#' @details Available models:
#' \itemize{
#'   \item \code{"linear"} (default): \eqn{V = (T - C)/K}. Analytic OLS;
#'         the only model providing \eqn{K}.
#'   \item \code{"logan"}: Logan-6 (Logan et al. 1976). Does NOT define
#'         a lower threshold; gives \eqn{T_m} and \eqn{T_{opt}}.
#'   \item \code{"lactin"}: Lactin et al. (1995). \eqn{\lambda < 0} lets
#'         the curve cross zero, so the lower threshold is derived
#'         numerically.
#'   \item \code{"briere1"}: \eqn{V = aT(T - T_0)\sqrt{T_m - T}}
#'         (Briere et al. 1999); \eqn{T_0} is the lower threshold.
#'   \item \code{"briere2"}: \eqn{V = aT(T - T_0)(T_m - T)^{1/m}};
#'         \eqn{m} adds flexibility.
#'   \item \code{"wang"}: Wang et al. (1982), 7 parameters --- needs at
#'         least 9 temperature points.
#'   \item \code{"auto"}: fits all candidates and selects the best per
#'         group by AICc; the full comparison table is stored in
#'         \code{object$comparison}.
#' }
#'
#' Nonlinear fits use \code{\link[stats]{nls}} (port algorithm,
#' bounded) with heuristic starting values and deterministic restarts
#' (\code{start} entries not belonging to the fitted model are
#' ignored). Standard errors are asymptotic; derived quantities
#' (\code{Topt}, \code{Vmax}, numerically derived \code{C}) carry no
#' SE. Each nonlinear model requires at least (number of parameters +
#' 2) temperature points.
#'
#' @param data A data.frame with at least a temperature column and a
#'   duration column.
#' @param temp_col,duration_col Column names (auto-detected by default).
#' @param by Grouping variable(s), e.g. \code{"stage"}; NULL fits the
#'   overall model.
#' @param model Single model name or \code{"auto"}; default
#'   \code{"linear"}.
#' @param start Optional named list of starting values for a nonlinear
#'   model, e.g. \code{list(a = 1e-4, T0 = 10, Tm = 35)}.
#' @param conf_level Confidence level, default 0.95.
#' @param min_n Minimum rows per group, default 3.
#' @param maxiter Iteration limit passed to \code{\link[stats]{nls.control}}.
#' @return A \code{"gdd"} object: \code{results} (summary table, one row
#'   per group; the selected model in \code{"auto"} mode), \code{fits}
#'   (per-group details), \code{data} (cleaned data), and
#'   \code{comparison} (model comparison table, \code{"auto"} only).
#' @seealso [gdd_read()], [gdd_check()],
#'   [gdd_compare()], [gdd_predict()], [gdd_plot()], [gdd_export()],
#'   [gdd_daily()]
#' @examples
#' # csv example shipped with the package (inst/extdata)
#' f  <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' df <- gdd_read(f)
#' fit1 <- gdd_calc(df, by = "stage")                       # linear (default)
#' \donttest{
#' fit2 <- gdd_calc(df, by = "stage", model = "briere1")    # nonlinear
#' fit3 <- gdd_calc(df, by = "stage", model = "auto")       # AICc selection
#' }
#' @export
gdd_calc <- function(data, temp_col = NULL, duration_col = NULL,
                     by = NULL,
                     model = c("linear", "logan", "lactin", "briere1",
                               "briere2", "wang", "auto"),
                     start = NULL, conf_level = 0.95, min_n = 3,
                     maxiter = 1000) {
  call <- match.call()
  model <- match.arg(model)
  if (length(conf_level) != 1L || !is.numeric(conf_level) ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be a single number strictly between 0 and 1.",
         call. = FALSE)
  if (!is.data.frame(data)) data <- as.data.frame(data)
  cols <- gdd_detect_cols(data, temp_col, duration_col)
  df <- gdd_clean(data, cols, by, min_n)

  gs <- split(df, df$group)
  fits <- list(); comparison <- NULL

  if (model != "auto") {
    need <- gdd_min_n(model)
    for (gn in names(gs)) {
      g <- gs[[gn]]
      if (nrow(g) < need)
        stop("Group '", gn, "' has only ", nrow(g),
             " temperature points; model '", model,
             "' requires at least ", need, ".", call. = FALSE)
      f <- if (model == "linear") {
        gdd_fit_linear(g$temp, g$rate, conf_level)
      } else {
        gdd_fit_nl(g$temp, g$rate, model, start, conf_level, maxiter)
      }
      if (is.null(f))
        stop("Model '", model, "' failed to converge for group '", gn,
             "'. Try another model, or supply start = list(...).",
             call. = FALSE)
      fits[[gn]] <- f
    }
  } else {
    for (gn in names(gs)) {
      fa <- gdd_fit_all(gs[[gn]], gn, start = start,
                        conf_level = conf_level, maxiter = maxiter)
      if (is.null(fa$best)) {
        warning("Group '", gn, "': no model could be fitted; skipped.",
                call. = FALSE)
      } else {
        fits[[gn]] <- fa$best
      }
      comparison <- rbind(comparison, fa$table)
    }
    if (!length(fits))
      stop("No group could be fitted with any model.", call. = FALSE)
  }
  if (!is.null(comparison)) rownames(comparison) <- NULL

  results <- do.call(rbind, lapply(names(fits), function(g) {
    f <- fits[[g]]
    data.frame(
      group = g, model = f$model, n = f$n,
      C = f$C, se_C = f$se_C,
      ci_C_lower = f$ci_C[1], ci_C_upper = f$ci_C[2],
      K = f$K, se_K = f$se_K,
      ci_K_lower = f$ci_K[1], ci_K_upper = f$ci_K[2],
      Topt = f$Topt, Vmax = f$Vmax, Tmax_est = f$Tmax_est,
      r = f$r, r_squared = f$r_squared, RMSE = f$rmse,
      AIC = f$aic, AICc = f$aicc, BIC = f$bic,
      F = f$F, p_value = f$p_value, row.names = NULL)
  }))
  low <- names(fits)[vapply(fits,
                            function(f) isTRUE(f$r_squared < 0.85),
                            logical(1))]
  if (length(low))
    warning("The following groups have R-squared < 0.85; use gdd_plot() ",
            "to inspect the fit: ", paste(low, collapse = ", "),
            call. = FALSE)

  structure(list(call = call, model = model, conf_level = conf_level,
                 cols = cols, by = by, results = results, fits = fits,
                 data = df, comparison = comparison),
            class = "gdd")
}