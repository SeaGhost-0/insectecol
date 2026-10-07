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
#' @param xlab,ylab Axis labels; character or plotmath expression.
#'   Defaults \code{expression("Temperature " * italic(T) * " (" * degree *
#'   "C)")} and \code{expression("Developmental rate " * italic(V) *
#'   " (d"^-1 * ")")} --- the variable letters are italic, as journal
#'   style requires.
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
                     xlab = expression("Temperature " * italic(T) *
                                         " (" * degree * "C)"),
                     ylab = expression("Developmental rate " * italic(V) *
                                         " (d"^-1 * ")"), ...) {
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
  ## switched off for this figure only; the previous state is put back on
  ## exit so that a later lc50 or life table figure is unaffected
  prev_showtext <- pkg_showtext_set(FALSE)          # see comment above
  on.exit(pkg_showtext_set(prev_showtext), add = TRUE)
  ## classic windows devices resolve families through windowsFonts()
  tryCatch(
    grDevices::windowsFonts(`Times New Roman` = grDevices::windowsFont("Times New Roman")),
    error = function(e) NULL)
  op <- par(mfrow = if (ng > 1) c(ceiling(ng / 2), min(ng, 2)) else c(1, 1),
            mar = if (ng > 1) c(2.8, 3.4, 2, 0.7) else c(3.2, 3.4, 2.2, 0.8),
            family = family, las = 1, mgp = c(2.2, 0.5, 0), tcl = -0.3)
  on.exit(par(op), add = TRUE)

  ## ---- figure style: matched to the lc50 plot ----
  ## Same line widths (fitted curve ~0.8 mm, axis lines and ticks
  ## ~0.65 mm, dashed reference lines ~0.7 mm --- base-graphics lwd is in
  ## 1/96 inch, so these are true physical widths, like ggplot's mm-based
  ## linewidth). The lc50 figure renders its text through showtext, whose
  ## internal dpi stays at 96 while the png is 300 dpi, so its effective
  ## text sizes are nominal*96/300. The cex values below reproduce those
  ## EFFECTIVE sizes on the 12 pt device pointsize: tick labels ~8.6 pt,
  ## both axis titles ~11.5 pt (the same size, journal style), statistics
  ## caption slightly below the axis titles, in-panel notes smaller.
  style <- list(axis = 2, tick = 1.5, curve = 3, dash = 2.6,
                cex_axis = 0.72, cex_lab = 0.96,         # ~8.6 / ~11.5 pt
                cex_main = 0.88, cex_sub = 0.75,         # ~10.6 / 9 pt
                cex_note = 0.64, cex_pt = 1.2,           # note / points
                x_mgp_lab = 0.42, x_line = 1.7)
  ## shrink an overlong caption (the automatic statistics caption can be
  ## long) until it fits the given dimension of the panel
  fit_cex <- function(txt, cex0, floor_cex = 0.85, limit) {
    cw <- cex0
    while (cw > floor_cex &&
             graphics::strwidth(txt, units = "inches", cex = cw) > limit)
      cw <- cw - 0.05
    cw
  }
  ## axis() draws its line only between the extreme tick marks, so the
  ## leading/trailing padded stretches of the panel edge would stay
  ## empty; draw the axis lines manually across the full panel, like
  ## ggplot's axis.line in the lc50 figure
  axis_line <- function(side, lwd) {
    u <- par("usr")
    if (side == 1)
      graphics::segments(u[1], u[3], u[2], u[3], lwd = lwd, xpd = FALSE)
    else
      graphics::segments(u[1], u[3], u[1], u[4], lwd = lwd, xpd = FALSE)
  }

  for (g in group) {
    sub_ <- d[d$group == g, ]
    f <- x$fits[[g]]

    ## ---- title / subtitle resolution ----
    ## auto titles are plotmath: R^2 superscripted, degree C, and the
    ## fitted-parameter letters in italic. The equals signs are quoted
    ## strings (" = "), not the == operator, which cannot be chained in
    ## the R grammar.
    auto_main <- if (f$model == "linear")
      bquote(.(g) * ":  " * italic(C) * " = " * .(sprintf("%.2f", f$C)) *
               " " * degree * "C,  " * italic(K) * " = " *
               .(sprintf("%.1f", f$K)) * " degree-days,  " *
               italic(R)^2 * " = " * .(sprintf("%.3f", f$r_squared)))
    else
      bquote(.(g) * "  [" * .(f$label) * "]:  " * italic(R)^2 * " = " *
               .(sprintf("%.3f", f$r_squared)) * ",  " * "AIC" * " = " *
               .(sprintf("%.1f", f$aic)))
    if (is.null(title)) {
      main_g <- auto_main
      sub_g <- NULL
    } else {
      main_g <- if (!is.null(names(title)) && g %in% names(title))
        title[[g]] else title[1]
      sub_g <- if (!is.null(sub)) sub else auto_main
    }
    ## the subtitle needs extra room below the x-axis title
    if (!is.null(sub_g))
      par(mar = c(4.2, 3.4, if (ng > 1) 2 else 2.2,
                  if (ng > 1) 0.7 else 0.8))
    ## captions wider than the panel are auto-shrunk (plotmath included);
    ## with several panels per figure the halves are narrow, so allow
    ## shrinking further before giving up; axis titles shrink against the
    ## panel width / height for the same reason (a long y title would
    ## otherwise run past the top and bottom of half-height panels)
    main_cex <- fit_cex(main_g, style$cex_main,
                        if (ng > 1) 0.5 else 0.7, par("pin")[1])
    sub_cex <- if (!is.null(sub_g))
      fit_cex(sub_g, style$cex_sub,
              if (ng > 1) 0.45 else 0.6, par("pin")[1]) else 1
    xlab_cex <- fit_cex(xlab, style$cex_lab, 0.6, par("pin")[1])
    ylab_cex <- fit_cex(ylab, style$cex_lab, 0.85, par("pin")[2])

    if (f$model == "linear") {
      ## ---- linear: classic V-T line ----
      xr <- range(c(sub_$temp, if (show_C) f$C))
      pad <- 0.05 * diff(xr)
      xr <- c(xr[1] - pad, xr[2] + pad)
      ylim <- if (show_C) range(c(0, sub_$rate)) else range(sub_$rate)
      plot(sub_$temp, sub_$rate, xlim = xr, ylim = ylim, pch = 19,
           xlab = "", ylab = "", main = main_g, sub = sub_g,
           cex.axis = style$cex_axis, cex.main = main_cex,
           cex.sub = sub_cex, axes = FALSE, ...)
      ## ticks point OUTWARD (negative tcl) and are shorter and thinner
      ## than the axis lines, like the lc50 figure; the x tick labels
      ## sit a little closer to the axis than the y labels, and the x
      ## title line is tuned so the title-to-label gap matches the y side
      par(mgp = c(style$x_line, style$x_mgp_lab, 0))
      axis(1, lwd = 0, lwd.ticks = style$tick)
      axis_line(1, style$axis)
      title(xlab = xlab, cex.lab = xlab_cex)
      par(mgp = c(2.2, 0.5, 0))
      axis(2, lwd = 0, lwd.ticks = style$tick)
      axis_line(2, style$axis)
      title(ylab = ylab, cex.lab = ylab_cex)
      abline(a = -f$C / f$K, b = 1 / f$K, col = "steelblue",
             lwd = style$curve)                            # V = (T - C) / K
      if (show_C) {
        abline(h = 0, col = "grey60", lty = 3, lwd = style$dash)
        abline(v = f$C, col = "grey60", lty = 3, lwd = style$dash)
        points(f$C, 0, pch = 19, col = "firebrick", cex = style$cex_pt * 1.1)
        ## the C label sits at the upper end of the dashed line, on the
        ## side with more room, shrinking a little when needed --- the
        ## old position right above the marker was crossed by the fitted
        ## line (placement check in the spirit of the lc50 LC labels)
        u <- par("usr")
        clab <- bquote(italic(C) == .(sprintf("%.2f", f$C)))
        room_r <- u[2] - f$C
        room_l <- f$C - u[1]
        side <- if (room_r > room_l) 4 else 2
        room <- if (side == 4) room_r else room_l
        cw <- style$cex_note
        while (cw > 0.4 &&
                 graphics::strwidth(clab, units = "user", cex = cw) >
                   room - 0.02 * (u[2] - u[1]))
          cw <- cw - 0.05
        text(f$C, u[4] - 0.05 * (u[4] - u[3]), clab, pos = side,
             offset = 0.3, col = "firebrick", cex = cw)
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
      plot(sub_$temp, sub_$rate, pch = 19, xlab = "", ylab = "",
           xlim = range(grid),
           ylim = range(c(0, sub_$rate, Vg[keep])),
           main = main_g, sub = sub_g,
           cex.axis = style$cex_axis, cex.main = main_cex,
           cex.sub = sub_cex, axes = FALSE, ...)
      par(mgp = c(style$x_line, style$x_mgp_lab, 0))
      axis(1, lwd = 0, lwd.ticks = style$tick)
      axis_line(1, style$axis)
      title(xlab = xlab, cex.lab = xlab_cex)
      par(mgp = c(2.2, 0.5, 0))
      axis(2, lwd = 0, lwd.ticks = style$tick)
      axis_line(2, style$axis)
      title(ylab = ylab, cex.lab = ylab_cex)
      lines(grid, Vg, col = "steelblue", lwd = style$curve)
      abline(h = 0, col = "grey60", lty = 3, lwd = style$dash)
      if (show_C && is.finite(f$C)) {
        abline(v = f$C, col = "grey60", lty = 3, lwd = style$dash)
        points(f$C, 0, pch = 19, col = "firebrick", cex = style$cex_pt * 1.1)
      }
      if (show_Topt && is.finite(f$Topt)) {
        abline(v = f$Topt, col = "darkgreen", lty = 3, lwd = style$dash)
        points(f$Topt, f$Vmax, pch = 17, col = "darkgreen",
               cex = style$cex_pt * 1.1)
      }
      if (is.finite(f$Tmax_est))
        points(f$Tmax_est, 0, pch = 15, col = "grey40",
               cex = style$cex_pt)
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