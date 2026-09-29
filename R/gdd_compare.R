# ============================================================
# insectecol --- Degree-day module: model comparison
# ============================================================

#' Compare Temperature-Development Models
#'
#' Fits all candidate models (linear + nonlinear) per group and returns
#' a comparison table with R-squared, RMSE, AIC, AICc, BIC, delta-AICc
#' and the best model (smallest AICc). Useful for reporting model
#' selection tables in publications.
#'
#' @param data,temp_col,duration_col,by Same as in [gdd_calc()].
#' @param models Character vector of candidate models; default is all
#'   six. \code{"auto"} is not allowed here (this function IS the
#'   comparison).
#' @param start,conf_level,min_n,maxiter Same as in [gdd_calc()].
#' @return A data.frame with columns: \code{group}, \code{model},
#'   \code{n}, \code{converged}, \code{k}, \code{R2}, \code{RMSE},
#'   \code{AIC}, \code{AICc}, \code{BIC}, \code{note},
#'   \code{delta_AICc}, \code{best}.
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' gdd_compare(gdd_read(f), by = "stage")
#' @export
gdd_compare <- function(data, temp_col = NULL, duration_col = NULL,
                        by = NULL,
                        models = c("linear", "logan", "lactin",
                                   "briere1", "briere2", "wang"),
                        start = NULL, conf_level = 0.95,
                        min_n = 3, maxiter = 1000) {
  models <- match.arg(models, names(gdd_rhs), several.ok = TRUE)
  if (!is.data.frame(data)) data <- as.data.frame(data)
  cols <- gdd_detect_cols(data, temp_col, duration_col)
  df <- gdd_clean(data, cols, by, min_n)

  gs <- split(df, df$group)
  tabs <- lapply(names(gs), function(gn)
    gdd_fit_all(gs[[gn]], gn, models = models, start = start,
                conf_level = conf_level, maxiter = maxiter)$table)
  out <- do.call(rbind, tabs)
  rownames(out) <- NULL
  out
}