# ============================================================
# insectecol --- Degree-day module: plotting
# Linear: fitted line + C marker; nonlinear: fitted curve + Topt.
# ============================================================

#' Plot Developmental Rate vs Temperature with the Fitted Model
#'
#' Draws a scatter plot of the developmental rate against temperature
#' with the fitted model. For the linear model the threshold
#' temperature C is marked where the fitted line crosses V = 0; for
#' nonlinear models the fitted curve is drawn and C / Topt / the upper
#' threshold are marked where defined.
#'
#' @param x A \code{"gdd"} object returned by [gdd_calc()].
#' @param group Character vector of group names to plot; default is all
#'   groups.
#' @param show_C Logical; whether to mark the threshold temperature C.
#'   Default TRUE.
#' @param show_Topt Logical; whether to mark the optimum temperature
#'   (nonlinear models only). Default TRUE.
#' @param title Plot title. \code{NULL} (default) uses the automatic
#'   per-group caption (group name + C / K / R-squared for the linear
#'   model, group name + model label + R-squared / AIC for nonlinear
#'   models). A single character string is used for every plotted
#'   group; a named character vector is matched by group name
#'   (e.g. \code{c(Egg = "egg", Pupa = "pupa")}); use \code{""} to drop
#'   the title. When a custom title is given, the automatic statistics
#'   caption is moved to the \code{sub} line unless \code{sub} is also
#'   supplied.
#' @param sub Plot subtitle (bottom line). \code{NULL} = the automatic
#'   statistics caption when \code{title} is customised, otherwise
#'   none.
#' @param family Text font family used for the title, axis labels and
#'   tick labels. Default \code{"serif"} --- a portable alias that maps
#'   to Times New Roman on 'Windows' (the journal standard) and to the
#'   system serif font elsewhere, and is valid on every device
#'   (including \code{pdf()}). Latin characters are rendered with this
#'   font; Chinese characters are rendered through the device's font
#'   fallback, which on Chinese 'Windows' is SimSun, so mixed
#'   English-Chinese titles work without extra settings. Set to
#'   \code{""} to use the device default, or pass an explicit family
#'   such as \code{"Times New Roman"} (widely available on 'Windows';
#'   may be unknown to the \code{pdf()} device on other platforms).
#' @param xlab,ylab Axis labels.
#' @param ... Further graphical parameters passed to \code{plot}.
#' @importFrom graphics abline lines par plot points text
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' fit <- gdd_calc(gdd_read(f), by = "stage")
#' gdd_plot(fit, group = "Egg")
#'
#' ## Custom title, subtitle and axis labels (portable English example):
#' gdd_plot(fit, group = "Egg",
#'          title = "Developmental rate vs temperature",
#'          sub = "Constant-temperature rearing experiment, 2026")
#'
#' ## Chinese titles are rendered through the device's font fallback
#' ## (SimSun on Chinese 'Windows'); note that the plain pdf() device
#' ## cannot embed CJK glyphs on many platforms, so draw on a png or
#' ## cairo device for Chinese titles.
#' @export
gdd_plot <- function(x, group = NULL, show_C = TRUE, show_Topt = TRUE,
                     title = NULL, sub = NULL, family = "serif",
                     xlab = "Temperature T (deg C)",
                     ylab = "Developmental rate V (1/d)", ...) {
  if (!inherits(x, "gdd"))
    stop("x must be a 'gdd' object returned by gdd_calc().", call. = FALSE)
  d <- x$data
  if (is.null(group)) group <- as.character(unique(d$group))
  group <- as.character(group)
  miss <- setdiff(group, as.character(unique(d$group)))
  if (length(miss))
    stop("Group(s) not found: ", paste(miss, collapse = ", "),
         call. = FALSE)
  nofit <- setdiff(group, names(x$fits))
  if (length(nofit)) {
    message("Skipped (no successful fit): ", paste(nofit, collapse = ", "))
    group <- setdiff(group, nofit)
  }
  if (!length(group))
    stop("No group with a successful fit to plot.", call. = FALSE)

  ng <- length(group)
  ## ---- font handling: Latin via `family`, Chinese via device fallback ----
  ## The gdd module deliberately does NOT use showtext: showtext renders
  ## every string with a single font and cannot fall back per glyph, so
  ## mixed English-Chinese titles would lose one of the two scripts.
  ## ragg and the classic Windows devices (GDI font linking) fall back
  ## per glyph: Latin chars use `family` (Times New Roman), Chinese
  ## chars use the system serif CJK font (SimSun on Chinese 'Windows').
  if (requireNamespace("showtext", quietly = TRUE))
    try(showtext::showtext_auto(enable = FALSE), silent = TRUE)   # see comment above
  ## classic windows devices resolve families through windowsFonts()
  tryCatch(
    grDevices::windowsFonts(`Times New Roman` = grDevices::windowsFont("Times New Roman")),
    error = function(e) NULL)
  op <- par(mfrow = if (ng > 1) c(ceiling(ng / 2), min(ng, 2)) else c(1, 1),
            mar = c(4.5, 4.5, 3, 1), family = family)
  on.exit(par(op), add = TRUE)

  for (g in group) {
    sub_ <- d[d$group == g, ]
    f <- x$fits[[g]]

    ## ---- title / subtitle resolution ----
    auto_main <- if (f$model == "linear")
      sprintf("%s:  C = %.2f deg C,  K = %.1f degree-days,  R2 = %.3f",
              g, f$C, f$K, f$r_squared)
    else
      sprintf("%s [%s]:  R2 = %.3f, AIC = %.1f",
              g, f$label, f$r_squared, f$aic)
    if (is.null(title)) {
      main_g <- auto_main
      sub_g <- NULL
    } else {
      main_g <- if (!is.null(names(title)) && g %in% names(title))
        title[[g]] else title[1]
      sub_g <- if (!is.null(sub)) sub else auto_main
    }
    ## the subtitle needs extra room below the x-axis title
    if (!is.null(sub_g)) par(mar = c(6.5, 4.5, 3, 1))

    if (f$model == "linear") {
      ## ---- linear: classic V-T line ----
      xr <- range(c(sub_$temp, if (show_C) f$C))
      pad <- 0.05 * diff(xr)
      xr <- c(xr[1] - pad, xr[2] + pad)
      ylim <- if (show_C) range(c(0, sub_$rate)) else range(sub_$rate)
      plot(sub_$temp, sub_$rate, xlim = xr, ylim = ylim, pch = 19,
           xlab = xlab, ylab = ylab, main = main_g, sub = sub_g, ...)
      abline(a = -f$C / f$K, b = 1 / f$K, col = "steelblue", lwd = 2)  # V = (T - C) / K
      if (show_C) {
        abline(h = 0, col = "grey60", lty = 3)
        abline(v = f$C, col = "grey60", lty = 3)
        points(f$C, 0, pch = 19, col = "firebrick", cex = 1.4)
        text(f$C, 0, sprintf("C = %.2f", f$C),
             pos = 3, offset = 0.9, col = "firebrick", cex = 0.8, xpd = TRUE)
      }
    } else {
      ## ---- nonlinear: fitted curve ----
      cf <- gdd_curve(f$model, f$params)
      lo <- min(sub_$temp) - 2
      if (is.finite(f$C)) lo <- min(lo, f$C)
      hi <- if (is.finite(f$Tmax_est)) f$Tmax_est else max(sub_$temp) + 3
      if (hi <= lo) hi <- max(sub_$temp) + 3
      grid <- seq(lo, hi, length.out = 400)
      Vg <- cf(grid)
      keep <- is.finite(Vg)
      plot(sub_$temp, sub_$rate, pch = 19, xlab = xlab, ylab = ylab,
           xlim = range(grid),
           ylim = range(c(0, sub_$rate, Vg[keep])),
           main = main_g, sub = sub_g, ...)
      lines(grid, Vg, col = "steelblue", lwd = 2)
      abline(h = 0, col = "grey60", lty = 3)
      if (show_C && is.finite(f$C)) {
        abline(v = f$C, col = "grey60", lty = 3)
        points(f$C, 0, pch = 19, col = "firebrick", cex = 1.3)
      }
      if (show_Topt && is.finite(f$Topt)) {
        abline(v = f$Topt, col = "darkgreen", lty = 3)
        points(f$Topt, f$Vmax, pch = 17, col = "darkgreen", cex = 1.3)
      }
      if (is.finite(f$Tmax_est))
        points(f$Tmax_est, 0, pch = 15, col = "grey40", cex = 1.2)
    }
  }
  invisible(x)
}

## S3 generic compatibility: plot(fit)
#' @method plot gdd
#' @export
plot.gdd <- function(x, group = NULL, ...) {
  gdd_plot(x, group = group, ...)
}