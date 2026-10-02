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
#' corresponding hatch date. The quantile dates are listed in a
#' legend box on the right-hand side of the figure (one row per
#' quantile, with a true arrow glyph instead of the character
#' combination "->").
#'
#' @param x A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param show_hatch Logical (default TRUE); whether to draw the
#'   hatch arrows when a hatch projection exists.
#' @param title Plot title; \code{NULL} (default) uses the automatic
#'   caption (survey date and sample size). Use \code{""} to drop
#'   the title.
#' @param sub Plot footnote; \code{NULL} (default) shows the hatch
#'   parameters when \code{show_hatch} is active (drawn below the
#'   x-axis title). Use \code{""} to drop it.
#' @param family Text font family. Default \code{"serif"} --- a
#'   portable alias that maps to Times New Roman on Windows and to
#'   the system serif font elsewhere; Chinese characters are
#'   rendered through the device's font fallback (SimSun on Chinese
#'   Windows). Set to \code{""} for the device default.
#' @param cex Overall text-size multiplier. Default \code{2}: the
#'   exported figure is meant to be placed at half the text width of
#'   a manuscript, where the labels, ticks and title then appear at
#'   about the size of the body text (12 pt). Set to \code{1} for
#'   full-screen viewing; the margins scale with \code{cex}
#'   automatically.
#' @param lwd Overall line-width multiplier. Default \code{2}.
#' @param legend_right Logical (default TRUE); whether the quantile
#'   legend is placed outside the panel on the right-hand side
#'   (\code{TRUE}) or in the top-left corner of the panel
#'   (\code{FALSE}).
#' @param xlab,ylab Axis labels.
#' @param ... Further graphical parameters passed to \code{plot}.
#' @importFrom graphics abline arrows axis box legend lines mtext par
#'   plot points rect text
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
                           cex = 2, lwd = 2, legend_right = TRUE,
                           xlab = "Projected eclosion date",
                           ylab = "Cumulative development (%)", ...) {
  if (!inherits(x, "emergence"))
    stop("x must be an 'emergence' object returned by ",
         "emergence_calc().", call. = FALSE)
  cex <- max(cex, 0.2)
  lwd <- max(lwd, 0.2)

  ## font handling: identical to gdd_plot --- showtext off (it
  ## renders whole strings in one font, losing the per-glyph CJK
  ## fallback), classic devices resolve the family via windowsFonts
  if (requireNamespace("showtext", quietly = TRUE))
    try(showtext::showtext_auto(enable = FALSE), silent = TRUE)
  tryCatch(
    grDevices::windowsFonts(`Times New Roman` =
                              grDevices::windowsFont("Times New Roman")),
    error = function(e) NULL)

  d <- x$table
  xs <- d$eclosion_date
  ys <- d$cumulative * 100
  pr <- x$predictions
  use_hatch <- x$has_hatch && show_hatch
  col_acc <- "#D55E00"
  d1 <- format(pr$date, "%m-%d")
  if (use_hatch) d2 <- format(pr$hatch_date, "%m-%d")

  auto_sub <- if (use_hatch)
    sprintf(paste0("Arrows: larval hatch = eclosion + pre-oviposition ",
                   "%.0f d + egg %.0f d"), x$pre_ovip, x$egg_days)
  else NULL
  sub_ <- if (!is.null(sub)) sub else auto_sub

  ## ---- layout: margins and axis-title positions scale with cex ----
  ## lab_line = margin line of the axis titles (moved away from the
  ## tick labels as cex grows); the footnote sits clearly below the
  ## x-axis title and the bottom margin reserves room for both.
  lab_line <- 3 + 1.4 * (cex - 1)
  mar1 <- lab_line + 1 + 0.9 * cex * (if (is.null(sub_) || nzchar(sub_))
                                        2 else 1) + 0.3
  mar4 <- 1
  if (legend_right) {
    ## measure the legend column (inches) and reserve it on the right;
    ## two lines per quantile keep the column narrow: the label on the
    ## first line, "date -> date" with a true arrow glyph below it
    lin_in  <- par("csi")                     # inches per margin line
    gap_in  <- 0.12 * cex
    arr_in  <- if (use_hatch) 0.35 * cex else 0
    w_lab <- max(strwidth(pr$label, units = "inches", cex = 0.85 * cex))
    w_d1  <- max(strwidth(d1, units = "inches", cex = 0.8 * cex))
    w_d2  <- if (use_hatch)
      max(strwidth(d2, units = "inches", cex = 0.8 * cex)) else 0
    row_w <- if (use_hatch) w_d1 + arr_in + w_d2 + 2 * gap_in else w_d1
    leg_w <- 1.08 * max(w_lab, row_w) + 2.2 * gap_in
    mar4  <- (leg_w + gap_in) / lin_in
  }
  op <- par(
    mar = c(mar1, lab_line + 1.9 * cex, 1.5 * cex + 0.5, mar4),
    mgp = c(lab_line, 0.6, 0), family = family)
  on.exit(par(op), add = TRUE)

  allx <- c(xs, pr$date, x$survey_date)
  if (use_hatch) allx <- c(allx, pr$hatch_date)
  xr <- range(allx)
  pad <- max(as.numeric(diff(xr)) * 0.04, 1)
  xr <- c(xr[1] - pad, xr[2] + pad)

  plot(NA, xlim = xr, ylim = c(0, 106), type = "n", axes = FALSE,
       xlab = xlab, ylab = ylab, cex.lab = cex, ...)
  ats <- pretty(xs)
  axis(1, at = ats, labels = format(ats, "%m-%d"), cex.axis = 0.85 * cex)
  axis(2, at = seq(0, 100, 20), las = 1, cex.axis = 0.85 * cex)
  box(lwd = lwd * 0.8)

  ## survey date reference line; the label sits just above the x axis
  ## so that it cannot collide with anything in the corners
  abline(v = x$survey_date, col = "grey60", lty = 3, lwd = lwd * 0.8)
  text(x$survey_date, 3.5, "survey", cex = 0.7 * cex, col = "grey40",
       pos = 4, offset = 0.3, xpd = TRUE)

  ## quantile reference lines
  for (i in seq_len(nrow(pr))) {
    abline(h = pr$p[i] * 100, col = col_acc, lty = 2, lwd = lwd * 0.6)
    abline(v = pr$date[i], col = col_acc, lty = 2, lwd = lwd * 0.6)
  }

  ## cumulative development curve, THEN the points (markers on top)
  lines(xs, ys, col = "grey20", lwd = lwd)
  points(xs, ys, pch = 19, cex = 0.55 * cex, col = "grey20")
  points(pr$date, pr$p * 100, pch = 21, bg = "white",
         col = col_acc, cex = 0.75 * cex, lwd = lwd)

  ## hatch arrows (stopped short of the triangle marker) and markers
  if (use_hatch) {
    eps <- 0.035 * as.numeric(diff(range(allx)))
    for (i in seq_len(nrow(pr)))
      arrows(pr$date[i], pr$p[i] * 100, pr$hatch_date[i] - eps,
             pr$p[i] * 100, length = 0.08, col = "grey45",
             lwd = lwd * 0.7, lty = 1)
    points(pr$hatch_date, pr$p * 100, pch = 24, bg = "white",
           col = "grey30", cex = 0.6 * cex, lwd = lwd * 0.8)
  }

  ## ---- quantile legend ----
  if (legend_right) {
    ux_in <- diff(par("usr")[1:2]) / par("pin")[1]  # user units / inch
    uy_in <- diff(par("usr")[3:4]) / par("pin")[2]
    dx <- function(inches) inches * ux_in
    dy <- function(inches) inches * uy_in
    x0 <- grconvertX(1, "nfc") - dx(leg_w)  # legend left inside mar4
    yto <- par("usr")[4] + dy(gap_in)               # just above the panel
    pitch_in <- 1.5 * 0.85 * cex * par("csi")       # line pitch, inches
    blk_in   <- 2.45 * pitch_in                     # per-quantile block
    n <- nrow(pr)
    ylab_row <- yto - (0.55 + (seq_len(n) - 1) * 2.45) * dy(pitch_in)
    ydat_row <- ylab_row - dy(1.15 * pitch_in)
    ## background box, then per quantile: label line, date/arrow line
    rect(x0 - dx(gap_in * 0.5), min(ydat_row) - dy(0.9 * pitch_in),
         grconvertX(1, "nfc") - dx(gap_in * 0.5), yto + dy(gap_in * 0.3),
         col = "white", border = "grey75", lwd = lwd * 0.5, xpd = NA)
    for (i in seq_len(n)) {
      text(x0, ylab_row[i], pr$label[i], adj = c(0, 0.5), cex = 0.85 * cex,
           col = col_acc, xpd = NA)
      text(x0, ydat_row[i], d1[i], adj = c(0, 0.5), cex = 0.8 * cex,
           col = col_acc, xpd = NA)
      if (use_hatch) {
        xa <- x0 + dx(w_d1 + gap_in)
        xb <- xa + dx(arr_in)
        arrows(xa, ydat_row[i], xb, ydat_row[i], length = 0.07,
               col = "grey45", lwd = lwd * 0.7, xpd = NA)
        text(xb + dx(gap_in), ydat_row[i], d2[i], adj = c(0, 0.5),
             cex = 0.8 * cex, col = col_acc, xpd = NA)
      }
    }
  } else {
    leg <- if (use_hatch)
      sprintf("%-18s %s  ->  %s", pr$label, d1, d2)
    else
      sprintf("%-18s %s", pr$label, d1)
    legend("topleft", legend = leg, bty = "o", cex = 0.85 * cex,
           text.col = col_acc, box.col = "grey75", box.lwd = lwd * 0.5,
           bg = "white", inset = c(0.01, 0.01))
  }

  ## title and footnote (the footnote sits below the x-axis title)
  auto_main <- if (is.finite(x$n))
    sprintf("Stage-grading projection (survey %s, n = %g)",
            format(x$survey_date), x$n)
  else
    sprintf("Stage-grading projection (survey %s)",
            format(x$survey_date))
  main <- if (is.null(title)) auto_main else title
  title(main = main, cex.main = 0.95 * cex)
  if (!is.null(sub_) && nzchar(sub_))
    mtext(sub_, side = 1, line = lab_line + 0.9 * cex + 0.7,
          cex = 0.7 * cex, col = "grey30")
  invisible(NULL)
}
