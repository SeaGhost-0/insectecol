# ============================================================
# insectecol --- Emergence-period module: S3 methods
# print / summary / predict for the "emergence" object.
# ============================================================

#' Print an Emergence-Period Projection
#'
#' Prints the projected eclosion dates (and hatch dates, when
#' requested) of the emergence quantiles.
#'
#' @param x A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param ... Further arguments (unused).
#' @method print emergence
#' @export
print.emergence <- function(x, ...) {
  cat("Emergence-period projection (stage-grading method)\n")
  cat("Survey date:", format(x$survey_date))
  if (is.finite(x$n))
    cat(sprintf("   (n = %g individuals, %d stages)", x$n, x$n_stages))
  else
    cat(sprintf("   (%d stages, percentages input)", x$n_stages))
  cat("\n")
  if (x$has_hatch)
    cat("Hatch projection: pre-oviposition", x$pre_ovip,
        "d + egg", x$egg_days, "d after eclosion\n")
  cat("\nAdult eclosion:\n")
  for (i in seq_len(nrow(x$predictions))) {
    pr <- x$predictions[i, ]
    cat(sprintf("  %-18s %s   (%.2f d after survey)\n",
                pr$label, format(pr$date), pr$days))
  }
  if (x$has_hatch) {
    cat("\nLarval hatch:\n")
    for (i in seq_len(nrow(x$predictions))) {
      pr <- x$predictions[i, ]
      cat(sprintf("  %-18s %s   (%.2f d after survey)\n",
                  pr$label, format(pr$hatch_date),
                  pr$days + x$pre_ovip + x$egg_days))
    }
  }
  invisible(x)
}

#' Summarise an Emergence-Period Projection
#'
#' Prints the full cumulative-development table behind the
#' projection and the quantile predictions.
#'
#' @param object A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param ... Further arguments (unused).
#' @return Invisibly, a list with \code{table} (the cumulative
#'   development) and \code{predictions} (the quantile dates).
#' @method summary emergence
#' @export
summary.emergence <- function(object, ...) {
  x <- object
  cat("Emergence-period projection --- cumulative development\n")
  cat("Survey date:", format(x$survey_date), "\n\n")
  tab <- x$table
  out <- data.frame(
    stage = tab$stage,
    proportion = sprintf("%.2f%%", tab$proportion * 100),
    cumulative = sprintf("%.2f%%", tab$cumulative * 100),
    days = tab$days,
    eclosion_date = format(tab$eclosion_date),
    stringsAsFactors = FALSE, check.names = FALSE)
  names(out)[2:3] <- c("share", "cumulative")
  print(out, row.names = FALSE)
  cat("\nQuantile predictions:\n")
  print(x$predictions, row.names = FALSE)
  invisible(list(table = x$table, predictions = x$predictions))
}

#' Quantiles of an Emergence-Period Projection
#'
#' Returns the projected eclosion (or hatch) dates of arbitrary
#' emergence quantiles.
#'
#' @param object A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param p Optional numeric vector of quantiles (0-1). The default
#'   (missing \code{p}) returns the stored predictions of the
#'   original call.
#' @param event \code{"eclosion"} (default) or \code{"hatch"}: which
#'   event to date.
#' @param ... Further arguments (unused).
#' @return A data.frame with columns \code{p}, \code{days}
#'   (fractional days after the survey date) and \code{date}.
#' @method predict emergence
#' @export
#' @examples
#' d <- data.frame(stage = c("Pupa 2", "Pupa 1", "Prepupa"),
#'                 count = c(3, 4, 3), days = c(12, 14, 16))
#' fit <- emergence_calc(d, survey_date = "2026-03-20",
#'                       pre_ovip = 3, egg_days = 10)
#' predict(fit)                     # the stored 16/50/84 predictions
#' predict(fit, c(0.25, 0.75))      # arbitrary eclosion quantiles
#' predict(fit, c(0.25, 0.75), event = "hatch")
predict.emergence <- function(object, p, event = c("eclosion", "hatch"),
                              ...) {
  event <- match.arg(event)
  if (event == "hatch" && !object$has_hatch)
    stop("No hatch projection in this object ",
         "(pre_ovip and egg_days are both 0).", call. = FALSE)
  if (missing(p)) {
    out <- object$predictions
    if (event == "hatch") {
      out$date <- out$hatch_date
      out$hatch_date <- NULL
    }
    return(out)
  }
  p <- suppressWarnings(as.numeric(p))
  if (length(p) == 0 || any(!is.finite(p)) || any(p <= 0 | p >= 1))
    stop("p must contain probabilities strictly between 0 and 1.",
         call. = FALSE)
  days_q <- object$interpolate(p)
  if (event == "hatch")
    days_q <- days_q + object$pre_ovip + object$egg_days
  data.frame(p = p, days = days_q,
             date = as.Date(object$survey_date + floor(days_q)),
             stringsAsFactors = FALSE, check.names = FALSE)
}
