# ============================================================
# insectecol --- Emergence-period module: plotting
# Cumulative development curve against the projected eclosion
# date, with the 16% / 50% / 84% quantile crossings marked and
# optional hatch arrows.
# ============================================================

#' Plot an Emergence-Period Projection
#'
#' Draws the cumulative development curve of the survey (stage
#' shares accumulated from the most developed stage downwards)
#' against the projected eclosion dates, marks the quantile
#' crossings (16% / 50% / 84% by default) and, when a hatch
#' projection exists, arrows from each eclosion date to the
#' corresponding hatch date.
#'
#' @param x A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param show_hatch Logical (default TRUE); whether to draw the
#'   hatch arrows when a hatch projection exists.
#' @param title Plot title; \code{NULL} (default) uses the automatic
#'   caption (survey date and sample size). Use \code{""} to drop
#'   the title.
#' @param sub Plot subtitle; \code{NULL} (default) shows the hatch
#'   parameters when \code{show_hatch} is active.
#' @param family Text font family. Default \code{"serif"} --- a
#'   portable alias that maps to Times New Roman on Windows and to
#'   the system serif font elsewhere; Chinese characters are
#'   rendered through the device's font fallback (SimSun on Chinese
#'   Windows). Set to \code{""} for the device default.
#' @param xlab,ylab Axis labels.
#' @param ... Further graphical parameters passed to \code{plot}.
#' @importFrom graphics abline arrows axis box legend lines mtext par
#'   plot points text
#' @method plot emergence
#' @export
#' @examples
#' f <- system.file("extdata", "emergence_example.csv",
#'                  package = "insectecol")
#' fit <- emergence_calc(emergence_read(f), survey_date = "2026-03-20",
#'                       pre_ovip = 3, egg_days = 10)
#' plot(fit)
#'
#' ## Chinese labels work on the interactive Windows and ragg devices
#' ## (the Chinese characters fall back to SimSun); the plain pdf()
#' ## device cannot render CJK glyphs on many platforms, so this part
#' ## is not run automatically:
#' \dontrun{
#' plot(fit, title = "二化螟越冬代发生期预测",
#'      xlab = "推算日期", ylab = "累计发育进度 (%)")
#' }
plot.emergence <- function(x, show_hatch = TRUE, title = NULL,
                           sub = NULL, family = "serif",
                           xlab = "Projected eclosion date",
                           ylab = "Cumulative development (%)", ...) {
  if (!inherits(x, "emergence"))
    stop("x must be an 'emergence' object returned by ",
         "emergence_calc().", call. = FALSE)

  ## font handling: identical to gdd_plot --- showtext off (it
  ## renders whole strings in one font, losing the per-glyph CJK
  ## fallback), classic devices resolve the family via windowsFonts
  if (requireNamespace("showtext", quietly = TRUE))
    try(showtext::showtext_auto(enable = FALSE), silent = TRUE)
  tryCatch(
    grDevices::windowsFonts(`Times New Roman` =
                              grDevices::windowsFont("Times New Roman")),
    error = function(e) NULL)
  op <- par(mar = c(4.5, 4.5, 3, 1), family = family)
  on.exit(par(op), add = TRUE)

  d <- x$table
  xs <- d$eclosion_date
  ys <- d$cumulative * 100
  pr <- x$predictions
  use_hatch <- x$has_hatch && show_hatch
  col_acc <- "#D55E00"

  allx <- c(xs, pr$date, x$survey_date)
  if (use_hatch) allx <- c(allx, pr$hatch_date)
  xr <- range(allx)
  pad <- max(as.numeric(diff(xr)) * 0.04, 1)
  xr <- c(xr[1] - pad, xr[2] + pad)

  plot(NA, xlim = xr, ylim = c(0, 106), type = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, ...)
  ats <- pretty(xs)
  axis(1, at = ats, labels = format(ats, "%m-%d"))
  axis(2, at = seq(0, 100, 20), las = 1)
  box()

  ## survey date reference line
  abline(v = x$survey_date, col = "grey60", lty = 3)
  text(x$survey_date, 103, "survey", cex = 0.7, col = "grey40")

  ## cumulative development curve
  lines(xs, ys, col = "grey20", lwd = 1.4)
  points(xs, ys, pch = 19, cex = 0.8, col = "grey20")

  ## quantile crossings
  for (i in seq_len(nrow(pr))) {
    abline(h = pr$p[i] * 100, col = col_acc, lty = 2, lwd = 0.7)
    abline(v = pr$date[i], col = col_acc, lty = 2, lwd = 0.7)
    points(pr$date[i], pr$p[i] * 100, pch = 21, bg = "white",
           col = col_acc, cex = 1.2, lwd = 1.2)
  }

  ## hatch arrows and markers
  if (use_hatch) {
    for (i in seq_len(nrow(pr)))
      arrows(pr$date[i], pr$p[i] * 100, pr$hatch_date[i],
             pr$p[i] * 100, length = 0.08, col = "grey45",
             lwd = 0.7, lty = 1)
    points(pr$hatch_date, pr$p * 100, pch = 24, bg = "white",
           col = "grey30", cex = 0.95, lwd = 1)
  }

  ## legend block: quantile dates (and hatch dates)
  leg <- if (use_hatch)
    sprintf("%-18s %s  ->  %s", pr$label, format(pr$date, "%m-%d"),
            format(pr$hatch_date, "%m-%d"))
  else
    sprintf("%-18s %s", pr$label, format(pr$date, "%m-%d"))
  legend("topleft", legend = leg, bty = "n", cex = 0.85,
         text.col = col_acc, inset = c(0.01, 0.01))

  ## title / subtitle resolution
  auto_main <- if (is.finite(x$n))
    sprintf("Stage-grading projection (survey %s, n = %g)",
            format(x$survey_date), x$n)
  else
    sprintf("Stage-grading projection (survey %s)",
            format(x$survey_date))
  auto_sub <- if (use_hatch)
    sprintf("Arrows: larval hatch = eclosion + pre-oviposition %.0f d + egg %.0f d",
            x$pre_ovip, x$egg_days)
  else NULL
  main <- if (is.null(title)) auto_main else title
  sub_ <- if (!is.null(sub)) sub else auto_sub
  title(main = main, sub = sub_, cex.main = 1.05)
  invisible(NULL)
}
