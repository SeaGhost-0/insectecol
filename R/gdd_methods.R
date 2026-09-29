# ============================================================
# insectecol --- Degree-day module: S3 methods
# Handles both linear and nonlinear fits uniformly.
# ============================================================

## Internal: pretty formatting of possibly-NA numbers
gdd_fmt <- function(v, digits = 3)
  if (is.finite(v)) sprintf("%.*f", digits, v) else "not defined"

#' @method print gdd
#' @export
print.gdd <- function(x, digits = 3, ...) {
  cat("Temperature-dependent development analysis\n")
  cat("Model:", if (x$model == "auto")
    "auto (best per group by AICc)" else gdd_model_label[[x$model]],
    "\n")
  if (!is.null(x$by))
    cat("Grouping variable(s):", paste(x$by, collapse = ", "), "\n")
  cat("Confidence level:", x$conf_level, "\n\n")

  for (g in names(x$fits)) {
    f <- x$fits[[g]]
    cat(sprintf("--- %s (n = %d, %s) ---\n", g, f$n, f$label))
    if (f$model == "linear") {
      cat(sprintf("  Threshold temperature   C = %.*f +/- %.*f deg C\n",
                  digits, f$C, digits, f$se_C))
      cat(sprintf("  Accumulated temperature K = %.*f +/- %.*f degree-days\n",
                  digits, f$K, digits, f$se_K))
      cat(sprintf("  r = %.*f, R-squared = %.*f, F(1,%d) = %.2f, p = %.4g\n",
                  digits, f$r, digits, f$r_squared, f$df, f$F, f$p_value))
    } else {
      cat(sprintf("  Threshold temperature  C    = %s deg C\n",
                  gdd_fmt(f$C, digits)))
      if (is.finite(f$se_C))
        cat(sprintf("  SE(C) = %.*f deg C\n", digits, f$se_C))
      cat(sprintf("  Optimum temperature   Topt = %s deg C (Vmax = %s 1/d)\n",
                  gdd_fmt(f$Topt, digits), gdd_fmt(f$Vmax, digits)))
      cat(sprintf("  Upper threshold       Tmax = %s deg C\n",
                  gdd_fmt(f$Tmax_est, digits)))
    }
    cat(sprintf("  R-squared = %.*f, RMSE = %.3g, AIC = %.2f, AICc = %.2f\n",
                digits, f$r_squared, f$rmse, f$aic, f$aicc))
  }

  if (!is.null(x$comparison)) {
    cat("\nModel comparison (AICc; 'auto' mode):\n")
    print(x$comparison[, c("group", "model", "n", "converged",
                           "AICc", "delta_AICc", "best")],
          row.names = FALSE)
  }
  invisible(x)
}

#' @method summary gdd
#' @export
summary.gdd <- function(object, digits = 4, ...) {
  x <- object
  cat("Temperature-dependent development analysis --- detailed results\n")
  cat("Confidence level:", x$conf_level * 100, "%\n\n")
  for (g in names(x$fits)) {
    f <- x$fits[[g]]
    cat(sprintf("--- %s (n = %d) --- model: %s ---\n", g, f$n, f$label))
    print(f$coef_table, row.names = FALSE, digits = digits)
    cat(sprintf("  Derived: C = %s, Topt = %s, Tmax = %s, Vmax = %s\n",
                gdd_fmt(f$C, 3), gdd_fmt(f$Topt, 3),
                gdd_fmt(f$Tmax_est, 3), gdd_fmt(f$Vmax, 4)))
    cat(sprintf("  Fit: R2 = %.4f, RMSE = %.5g, AIC = %.2f, AICc = %.2f, BIC = %.2f",
                f$r_squared, f$rmse, f$aic, f$aicc, f$bic))
    if (f$model == "linear")
      cat(sprintf(", F(1,%d) = %.2f, p = %.4g", f$df, f$F, f$p_value))
    cat("\n")
    if (f$model == "logan")
      cat("  Note: Logan-6 does not define a lower threshold C.\n")
    cat("\n")
  }
  if (!is.null(x$comparison)) {
    cat("Model comparison (AICc):\n")
    print(x$comparison, row.names = FALSE, digits = digits)
  }
  invisible(x$fits)
}