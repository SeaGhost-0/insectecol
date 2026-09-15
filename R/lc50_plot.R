#' LC50 Regression Plots
#'
#' Plots every data set: observed points, the fitted curve of the computed
#' method with its pointwise confidence band, and dashed reference lines
#' marking the LC estimate. By default the concentration axis is on a
#' log10 scale, which gives the classical symmetric S-shaped curve;
#' \code{shape = "linear"} restores the original linear axis.
#'
#' @param results Result list of \code{\link{lc50_calculate}}.
#' @param save_path Folder for the png files; \code{NULL} (default)
#'   displays the plots only.
#' @param font Font family (default \code{"TNM"}).
#' @param width,height Figure size in inches (default 7 x 6).
#' @param dpi Resolution of the saved files (default 300); at any dpi the
#'   figures keep the physical size they have at 300 dpi.
#' @param unit Unit of the concentration (e.g. \code{"mg/L"}), used in
#'   the LC label and the x-axis title. \code{NULL} (the default) is
#'   treated as \code{"mg/L"}; pass \code{""} to show no unit at all.
#' @param shape \code{"sigmoid"} (default): log10 concentration axis, the
#'   symmetric S-shaped dose-response curve. \code{"linear"}: the original
#'   linear concentration axis.
#' @param ci Logical (default \code{TRUE}): draw the pointwise confidence
#'   band of the fitted curve.
#' @param ci_level Confidence level of the curve band and of the replicate
#'   error bars (default 0.95).
#' @param error_bar Logical (default \code{TRUE}): replicate rows of the
#'   same concentration are pooled to a single point, the Abbott-corrected
#'   \code{sum(Dead) / sum(Tested)} (equal to the replicate mean when the
#'   replicate groups are of equal size), with a Wilson score interval at
#'   \code{ci_level} as the error bar, clipped to [0, 1]. \code{FALSE}
#'   draws every raw row as a plain point (the previous behaviour).
#' @param move_thres Numeric (default 0.5). A dashed reference line that
#'   does not land on a regular tick normally gets an extra tick whose
#'   value is labelled next to the axis like a regular tick. If the LC
#'   position is at most \code{move_thres} regular tick spacings away
#'   from the nearest tick (measured on the display axis, i.e. log10
#'   concentrations for \code{shape = "sigmoid"}), that label would
#'   overlap the neighbouring tick label, so the value is drawn inside
#'   the panel instead: the concentration just above the x axis to the
#'   right of the vertical dashed line, the mortality just right of the
#'   y axis above the horizontal dashed line (each flips to the other
#'   side of its dashed line when it would not fit). \code{0} disables
#'   the move; with evenly spaced ticks 0.5 moves every value that is
#'   not midway between two ticks.
#' @param method Character scalar, which methods to plot: a subset of
#'   \code{c("traditional", "improved", "probit")}, or \code{"all"}
#'   (default) for every method present in the results object.
#'
#' @details Replicates of the same concentration are pooled and drawn as
#'   the Abbott-corrected pooled mortality with Wilson score intervals.
#'
#' @return Named list of ggplot objects (invisibly).
#' @seealso \code{\link{save_lc50}}, \code{\link{save_lc50_plot}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' res <- lc50_calculate(read_lc50(f))
#' plots <- plot_lc50(res, save_path = tempdir())
#' plots <- plot_lc50(res, shape = "linear", save_path = tempdir())  # original axis
#' plots <- plot_lc50(res, ci = FALSE, error_bar = FALSE,
#'                    save_path = tempdir())                          # bare version
plot_lc50 <- function(results, save_path = NULL, font = "TNM",
                      width = 7, height = 6, dpi = 300, unit = NULL,
                      shape = c("sigmoid", "linear"),
                      ci = TRUE, ci_level = 0.95,
                      error_bar = TRUE, move_thres = 0.5, method = NULL) {
  showtext::showtext_auto(enable = TRUE)
  font <- pkg_resolve_font(font)
  shape <- match.arg(shape)
  if (is.null(unit)) unit <- "mg/L"

  plot_list <- list()

  for (nm in names(results$results)) {
    # when several methods are stored, the file names get a method suffix
    mtag <- if (!is.null(method) && length(results$results[[nm]]) > 1)
      paste0("_", paste(method, collapse = "_")) else ""
    gp <- lc50_plot_one(nm, results$results[[nm]], font, unit,
                        shape = shape, ci = ci, ci_level = ci_level,
                        error_bar = error_bar, move_thres = move_thres,
                        method = method)
    if (is.null(gp)) next
    attr(gp, "lc50_name") <- paste0(
      if (shape == "sigmoid") nm else paste0(nm, "_linear"), mtag)
    plot_list[[nm]] <- gp
    if (!is.null(save_path)) {
      lc50_ggsave(file.path(save_path,
                            paste0("LC50_", attr(gp, "lc50_name"), ".png")),
                  gp, width = width, height = height, dpi = dpi)
    } else {
      print(gp)
    }
  }
  showtext_auto(enable = FALSE)
  invisible(plot_list)
}

#' Save LC50 Plots
#'
#' Saves one plot or a list of plots from \code{\link{plot_lc50}}, like
#' \code{ggsave(path, plot, device = "tiff", width = 12, height = 8,
#' dpi = 300, units = "cm", bg = "white")} but with the dpi handling of
#' \code{plot_lc50} applied. The same plot object can be written at any
#' dpi without being re-created.
#'
#' @param plot A ggplot or a (named) list of ggplots.
#' @param path Output file (single plot) or folder (list of plots, or a
#'   path without extension); \code{NULL} (default) opens a folder
#'   selection dialog.
#' @param device,width,height,units,bg Passed on to \code{ggsave}
#'   (defaults \code{"tiff"}, 12, 8, \code{"cm"}, \code{"white"}).
#' @param dpi Resolution of the written file (default 300).
#' @param ... Further arguments passed on to \code{ggsave}.
#'
#' @return Path(s) of the written file(s), invisibly.
#' @seealso \code{\link{plot_lc50}}, \code{\link{save_lc50}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' plots <- plot_lc50(lc50_calculate(read_lc50(f)))
#' save_lc50_plot(plots$bioassay, file.path(tempdir(), "LC50_demo.tiff"))
save_lc50_plot <- function(plot, path = NULL, device = "tiff",
                           width = 12, height = 8, dpi = 300,
                           units = "cm", bg = "white", ...) {
  if (is.null(path)) {
    path <- utils::choose.dir()
    if (is.na(path)) stop("No output folder selected")
  }
  ext <- if (is.character(device)) tolower(device) else "tiff"

  # List: save the plots one by one, named after the data sets
  if (!inherits(plot, "ggplot")) {
    if (!dir.exists(path))
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    nms <- names(plot)
    if (is.null(nms) || any(!nzchar(nms)))
      nms <- paste0("Plot", seq_along(plot))
    out <- vapply(seq_along(plot), function(i) {
      save_lc50_plot(plot[[i]],
                     file.path(path, paste0("LC50_", nms[i], ".", ext)),
                     device = device, width = width, height = height,
                     dpi = dpi, units = units, bg = bg, ...)
    }, character(1))
    return(invisible(out))
  }

  # Single plot: if the path is a folder or has no extension, it is also
  # auto-named after the data set
  if (dir.exists(path) || !grepl("\\.[[:alnum:]]+$", path)) {
    nm <- attr(plot, "lc50_name")
    if (is.null(nm) || !nzchar(nm)) nm <- "plot"
    path <- file.path(path, paste0("LC50_", nm, ".", ext))
  }
  if (!dir.exists(dirname(path)))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  showtext::showtext_auto(enable = TRUE)
  lc50_ggsave(path, plot, width = width, height = height, dpi = dpi,
              device = device, units = units, bg = bg, ...)
  invisible(path)
}

# Current internal dpi of showtext (default 96)
lc50_showtext_dpi <- function() {
  opts <- tryCatch(showtext::showtext_opts(), error = function(e) NULL)
  if (is.list(opts) && is.numeric(opts$dpi) &&
      length(opts$dpi) == 1 && is.finite(opts$dpi)) opts$dpi else 96
}

# showtext renders/measures text only at its own fixed internal resolution
# (default 96), ignoring the device dpi, so text, points and spacing in the
# output scale with dpi as a whole. When saving, the internal dpi is set to
# eff * 96/300 so the "internal/actual" ratio matches the one at 300 dpi and
# the physical size of the output matches 300 dpi exactly; afterwards the
# default is restored, leaving screen previews unaffected.
lc50_ggsave <- function(filename, plot, width, height, dpi,
                        device = "png", units = "in", bg = "white", ...) {
  ref <- lc50_showtext_dpi()
  # Vector devices have no pixels; convert at 72 pt/in
  eff <- if (is.character(device) &&
             tolower(device) %in% c("pdf", "cairo_pdf", "eps", "ps",
                                    "postscript", "cairo_ps")) 72 else dpi
  showtext::showtext_opts(dpi = eff * ref / 300)
  on.exit(showtext::showtext_opts(dpi = ref), add = TRUE)
  ggplot2::ggsave(filename, plot = plot, device = device,
                  width = width, height = height, dpi = dpi,
                  units = units, bg = bg, ...)
}

# Color palette of the three methods
lc50_method_colors <- c(
  "Traditional linear regression" = "#E69F00",
  "Improved linear regression" = "#56B4E9",
  "Probit analysis" = "#CC79A7"
)

# Internal: fitted mortality curve and its pointwise CI on a grid of
# log10 concentrations. The lm fits probit + 5, the glm probit link
# already is probit; quasibinomial se.fit carries the heterogeneity
# factor, so the band matches the CI of the LC estimate.
lc50_fit_band <- function(r, lg, level = 0.95) {
  nd <- data.frame(x = lg)
  is_glm <- inherits(r$fit, "glm")
  pr <- if (is_glm) {
    stats::predict(r$fit, newdata = nd, se.fit = TRUE, type = "link")
  } else {
    stats::predict(r$fit, newdata = nd, se.fit = TRUE)
  }
  shift <- if (is_glm) 0 else -5
  tcrit <- stats::qt(1 - (1 - level) / 2, df = r$fit$df.residual)
  center <- pr$fit + shift
  data.frame(
    "Conc" = 10^lg,
    "Mortality" = stats::pnorm(center),
    "lo" = stats::pnorm(center - tcrit * pr$se.fit),
    "hi" = stats::pnorm(center + tcrit * pr$se.fit),
    check.names = FALSE
  )
}

# Internal: per concentration, pooled Dead/Tested across replicate rows,
# Abbott-corrected, with a Wilson score CI for the pooled proportion
lc50_pool_ci <- function(prep, level = 0.95) {
  conc   <- prep[["Concentration"]]
  dead   <- prep[["Dead"]]
  tested <- prep[["Tested"]]
  pc <- attr(prep, "pc"); if (is.null(pc)) pc <- 0
  z <- stats::qnorm(1 - (1 - level) / 2)
  do.call(rbind, lapply(sort(unique(conc)), function(c0) {
    idx <- conc == c0
    x <- sum(dead[idx]); m <- sum(tested[idx])
    p_raw <- x / m                       # 合并死亡率（等 n 时 = 重复均值）
    cen <- (x + z^2 / 2) / (m + z^2)
    hw  <- z / (m + z^2) * sqrt(x * (m - x) / m + z^2 / 4)
    lo_raw <- max(cen - hw, 0); hi_raw <- min(cen + hw, 1)
    cor <- function(q) (q - pc) / (1 - pc)
    data.frame("Conc" = c0, "Mean" = cor(p_raw), "k" = sum(idx),
               "lo" = max(cor(lo_raw), 0), "hi" = min(cor(hi_raw), 1),
               check.names = FALSE)
  }))
}

# Internal: classic log10 tick positions (1, 2, 5 per decade; decades only
# when the 1-2-5 set would be too dense) inside [lo, hi], in log10 units
lc50_log_ticks <- function(lo, hi) {
  cand <- function(mult) {
    ks <- floor(lo):ceiling(hi)
    t <- sort(unique(round(as.vector(
      outer(mult, ks, function(m, k) log10(m) + k)), 6)))
    t[t >= lo & t <= hi]
  }
  t5 <- cand(c(1, 2, 5))
  if (length(t5) > 8) cand(1) else t5
}

# Uniform decimals across the axis, mimicking ggplot2's default axis labels
lc50_tick_labels <- function(breaks) {
  breaks <- breaks[is.finite(breaks)]
  if (length(breaks) == 0) return(character(0))
  txt <- vapply(breaks, function(v) {
    format(v, trim = TRUE, scientific = FALSE, digits = 7)
  }, character(1), USE.NAMES = FALSE)
  k <- max(0L, vapply(txt, function(t) {
    p <- regexpr(".", t, fixed = TRUE)
    if (p < 0) 0L else nchar(t) - p
  }, integer(1), USE.NAMES = FALSE))
  sprintf(paste0("%.", k, "f"), breaks)
}

# Whether v already coincides with a regular tick (numerically, or by the
# 3 significant digits shown)
lc50_on_tick <- function(v, breaks) {
  breaks <- breaks[is.finite(breaks)]
  if (length(breaks) == 0) return(FALSE)
  tol <- 1e-9 + 1e-6 * max(abs(c(breaks, v)))
  if (any(abs(breaks - v) < tol)) return(TRUE)
  any(sprintf("%.3g", breaks) == sprintf("%.3g", v))
}

# Whether the LC position v (in axis units) lies so close to a regular
# tick that the extra tick's label next to the axis would overlap the
# neighbouring tick label: thres is the threshold as a fraction of the
# tick spacing (rng = axis range, the fallback spacing when there are
# fewer than two ticks)
lc50_near_tick <- function(v, ticks, thres = 0.5, rng = 1) {
  ticks <- ticks[is.finite(ticks)]
  if (length(ticks) == 0 || !is.finite(v) || !is.finite(thres)) return(FALSE)
  gap <- if (length(ticks) >= 2) min(diff(sort(ticks))) else rng
  if (!is.finite(gap) || gap <= 0) gap <- rng
  min(abs(ticks - v)) <= thres * gap
}

# Plot of a single file (the method whose computation succeeded).
# shape = "sigmoid": log10 concentration axis (symmetric S curve);
# shape = "linear": original linear concentration axis.
lc50_plot_one <- function(nm, one, font, unit = NULL,
                          shape = c("sigmoid", "linear"),
                          ci = TRUE, ci_level = 0.95,
                          error_bar = TRUE, move_thres = 0.5, method = NULL) {

  # Restrict to the requested method(s); NULL keeps everything and the
  # first method that succeeded is plotted (the previous behaviour)
  if (!is.null(method)) {
    all_methods <- c(traditional = "Traditional linear regression",
                     improved = "Improved linear regression",
                     probit = "Probit analysis")
    keys <- lc50_pick_methods(method, all_methods)
    one <- one[intersect(names(one), keys)]
    if (length(one) == 0) {
      warning(sprintf("[%s] no computed result for method \"%s\"; plot skipped",
                      nm, method))
      return(NULL)
    }
  }
  r <- NULL
  for (key in names(one)) {
    if (!is.null(one[[key]]$estimate)) { r <- one[[key]]; break }
  }
  if (is.null(r)) {
    if (!is.null(method))
      warning(sprintf("[%s] method \"%s\" produced no result; plot skipped",
                      nm, method))
    return(NULL)
  }
  shape <- match.arg(shape)
  xfun <- if (shape == "sigmoid") log10 else identity
  col <- unname(lc50_method_colors[r$method])
  if (is.na(col)) col <- "#E69F00"

  prep <- r$prep

  # Observed points: replicate means +- CI (or every raw row)
  if (error_bar) {
    agg <- lc50_pool_ci(prep, level = ci_level)
    pts <- data.frame("Conc" = xfun(agg[["Conc"]]),
                      "Mortality" = agg[["Mean"]],
                      "lo" = agg[["lo"]], "hi" = agg[["hi"]],
                      "k" = agg[["k"]], check.names = FALSE)
  } else {
    pts <- data.frame("Conc" = xfun(prep[["Concentration"]]),
                      "Mortality" = attr(prep, "p"),
                      "lo" = NA_real_, "hi" = NA_real_,
                      "k" = 1L, check.names = FALSE)
  }

  # Fitted curve (+ CI band); the model always works on log10(conc),
  # only the display axis differs between the two shapes
  lg <- seq(min(log10(prep[["Concentration"]])),
            max(log10(prep[["Concentration"]])), length.out = 200)
  band <- lc50_fit_band(r, lg, level = ci_level)
  curve <- data.frame("Conc" = xfun(band[["Conc"]]),
                      "Mortality" = band[["Mortality"]], check.names = FALSE)

  lc_x <- xfun(r$estimate)          # LC position on the display axis
  lc_real <- r$estimate             # LC on the concentration scale
  lc_y <- r$lc
  if (is.null(unit)) unit <- "mg/L"   # NULL -> default unit; "" -> no unit
  lc_label <- if (nzchar(unit)) {
    sprintf('LC[%d] == %.3g~"%s"', round(lc_y * 100), lc_real, unit)
  } else {
    sprintf("LC[%d] == %.3g", round(lc_y * 100), lc_real)
  }

  # Panel range (x extended by 5% on each side, y fixed to 0-1) and the
  # regular ticks; extra ticks are added only when the reference line's
  # landing point is not on a regular tick
  x_rng <- range(xfun(prep[["Concentration"]]))
  x_w <- x_rng[2] - x_rng[1]
  x_lo <- x_rng[1] - 0.05 * x_w
  x_hi <- x_rng[2] + 0.05 * x_w
  if (x_w <= 0) {                 # only one concentration
    x_lo <- x_rng[1] - 0.05
    x_hi <- x_rng[2] + 0.05
  }

  y_percent <- FALSE    # FALSE restores the 0-1 proportion axis
  y_ticks_reg <- seq(0, 1, 0.2)
  y_labels_reg <- lc50_tick_labels(
    if (y_percent) y_ticks_reg * 100 else y_ticks_reg)
  y_extra <- !lc50_on_tick(lc_y, y_ticks_reg)

  if (shape == "sigmoid") {
    x_ticks_reg <- lc50_log_ticks(x_lo, x_hi)
    x_labels_reg <- sprintf("%g", signif(10^x_ticks_reg, 3))
    x_extra <- length(x_ticks_reg) > 0 &&
      !lc50_on_tick(lc_real, 10^x_ticks_reg)
  } else {
    x_ticks_reg <- scales::extended_breaks()(c(x_lo, x_hi))
    x_ticks_reg <- x_ticks_reg[x_ticks_reg >= x_lo & x_ticks_reg <= x_hi]
    x_labels_reg <- lc50_tick_labels(x_ticks_reg)
    x_extra <- length(x_ticks_reg) > 0 && !lc50_on_tick(lc_real, x_ticks_reg)
  }

  # NEW: whether the LC value that missed the regular ticks lies so
  # close to one (distance on the display axis <= move_thres * tick
  # spacing) that its label next to the axis would overlap the tick
  # label; then the value is drawn inside the panel instead: the
  # concentration just above the x axis to the right of the vertical
  # dashed line, the mortality just right of the y axis above the
  # horizontal dashed line
  x_lab_inside <- x_extra &&
    lc50_near_tick(lc_x, x_ticks_reg, move_thres, x_hi - x_lo)
  y_lab_inside <- y_extra &&
    lc50_near_tick(lc_y, y_ticks_reg, move_thres, 1)

  # LC label, anchored to the vertical reference line: LC left of the
  # panel midpoint -> label to the right of the line, BELOW the crossing
  # (the fitted curve is above on that side); LC right of the midpoint ->
  # label to the left of the line, ABOVE the crossing (the curve is
  # below on that side)
  lc_lab_dy <- 0.1       # vertical offset from the crossing (y: 0-1)
  lc_lab_gap <- 0.35     # horizontal gap from the vertical line
  x_mid <- (x_lo + x_hi) / 2
  if (x_w > 0 && lc_x < x_mid) {
    lab_hjust <- -lc_lab_gap                 # text starts right of the line
    lc_lab_y <- max(lc_y - lc_lab_dy, 0.02)  # below, kept inside the panel
  } else {
    lab_hjust <- 1 + lc_lab_gap              # text ends left of the line
    lc_lab_y <- min(lc_y + lc_lab_dy, 0.98)  # above, kept inside the panel
  }

  # NEW: labels moved into the panel hug their dashed line by default
  # and flip to the other side only when they would not fit between the
  # line and the panel edge, or (x) when the LC label already occupies
  # that corner; the width estimate is one digit per character
  x_val_txt <- sprintf("%.3g", lc_real)
  x_val_w <- 0.045 * nchar(x_val_txt) + 0.02
  x_val_left <- (x_hi - lc_x) < x_val_w * (x_hi - x_lo) ||
    (lab_hjust < 0 && lc_lab_y < 0.21)
  y_val_below <- (1 - lc_y) < 0.16

  # Sizes of the hand-drawn axis elements
  base_size <- 45
  tick_len_x <- 0.04
  tick_len_y <- 0.025 * (x_hi - x_lo)
  tick_len_ratio <- 0.6
  tick_lab_gap <- 0.15
  axis_text_col <- "grey10"
  axis_lab_size <- 0.8 * base_size / ggplot2::.pt
  bar_w <- 0.018 * (x_hi - x_lo)   # cap width of the error bars

  # ggplot2 draws layers in the order they are added (later = on top), so
  # the confidence band goes in first and the observed error bars and
  # points last: the data floats above the band and the fitted curve
  gp <- ggplot()
  if (ci) {
    gp <- gp + geom_ribbon(
      data = data.frame("Conc" = curve[["Conc"]],
                        "lo" = band[["lo"]], "hi" = band[["hi"]],
                        check.names = FALSE),
      aes(x = .data[["Conc"]], ymin = .data[["lo"]], ymax = .data[["hi"]]),
      fill = col, alpha = 0.18, color = NA)
  }
  gp <- gp +
    geom_line(data = curve,
              aes(x = .data[["Conc"]], y = .data[["Mortality"]]),
              color = col, linewidth = 0.8)
  if (error_bar) {
    eb <- pts[!is.na(pts[["lo"]]), , drop = FALSE]
    if (nrow(eb) > 0) {
      gp <- gp + geom_errorbar(
        data = eb,
        aes(x = .data[["Conc"]], ymin = .data[["lo"]], ymax = .data[["hi"]]),
        width = bar_w, linewidth = 0.55)
    }
  }
  gp <- gp +
    geom_point(data = pts,
               aes(x = .data[["Conc"]], y = .data[["Mortality"]]),
               size = 1.2, stroke = 0.35) +
    # L-shaped dashed reference lines, from the axes to the LC point
    annotate("segment",
             x = x_lo, xend = lc_x, y = lc_y, yend = lc_y,
             linetype = "dashed", color = col, linewidth = 0.7) +
    annotate("segment",
             x = lc_x, xend = lc_x, y = 0, yend = lc_y,
             linetype = "dashed", color = col, linewidth = 0.7) +
    annotate("text",
             x = lc_x, y = lc_lab_y,
             label = lc_label, parse = TRUE,
             hjust = lab_hjust, vjust = 0.5,
             size = 10.5, fontface = "bold",
             family = font, color = col) +
    coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(0, 1),
                    expand = FALSE, clip = "off")

  # The theme's native ticks are off; all ticks are drawn by hand, the
  # one for the reference line being shorter. The extra tick stays even
  # when its label is moved into the panel - it marks the exact landing
  # point of the dashed line
  if (length(x_ticks_reg) > 0) {
    gp <- gp + annotate("segment",
                        x = x_ticks_reg, xend = x_ticks_reg,
                        y = 0, yend = -tick_len_x,
                        color = "black", linewidth = 0.65)
  }
  if (length(y_ticks_reg) > 0) {
    gp <- gp + annotate("segment",
                        x = x_lo, xend = x_lo - tick_len_y,
                        y = y_ticks_reg, yend = y_ticks_reg,
                        color = "black", linewidth = 0.65)
  }
  if (x_extra) {
    gp <- gp + annotate("segment",
                        x = lc_x, xend = lc_x,
                        y = 0, yend = -tick_len_ratio * tick_len_x,
                        color = "black", linewidth = 0.65)
  }
  if (y_extra) {
    gp <- gp + annotate("segment",
                        x = x_lo, xend = x_lo - tick_len_ratio * tick_len_y,
                        y = lc_y, yend = lc_y,
                        color = "black", linewidth = 0.65)
  }

  # Value labels of the extra ticks; the x label always shows the real
  # concentration, not the (possibly log10) axis position. A value too
  # close to a regular tick is drawn inside the panel (x_lab_inside /
  # y_lab_inside above) so that it cannot overlap the tick label
  if (x_extra) {
    if (x_lab_inside) {
      # above the x axis, to the right of the vertical dashed line
      # (left of it when it would not fit or the LC label is there)
      gp <- gp + annotate("text",
                          x = lc_x, y = 0,
                          label = x_val_txt,
                          hjust = if (x_val_left) 1.1 else -0.1,
                          vjust = -0.3,
                          size = axis_lab_size, family = font,
                          color = axis_text_col)
    } else {
      gp <- gp + annotate("text",
                          x = lc_x, y = -tick_len_ratio * tick_len_x,
                          label = x_val_txt,
                          hjust = 0.5, vjust = 1.2 + tick_lab_gap,
                          size = axis_lab_size, family = font,
                          color = axis_text_col)
    }
  }
  if (y_extra) {
    y_val_txt <- sprintf("%.3g", if (y_percent) lc_y * 100 else lc_y)
    if (y_lab_inside) {
      # right of the y axis, above the horizontal dashed line
      # (below it when it would not fit)
      gp <- gp + annotate("text",
                          x = x_lo, y = lc_y,
                          label = y_val_txt,
                          hjust = -0.1,
                          vjust = if (y_val_below) 1.3 else -0.3,
                          size = axis_lab_size, family = font,
                          color = axis_text_col)
    } else {
      gp <- gp + annotate("text",
                          x = x_lo - tick_len_ratio * tick_len_y, y = lc_y,
                          label = y_val_txt,
                          hjust = 1 + tick_lab_gap, vjust = 0.5,
                          size = axis_lab_size, family = font,
                          color = axis_text_col)
    }
  }

  if (length(x_ticks_reg) > 0) {
    gp <- gp + scale_x_continuous(breaks = x_ticks_reg,
                                  labels = x_labels_reg)
  }
  gp <- gp +
    scale_y_continuous(breaks = y_ticks_reg, labels = y_labels_reg) +
    labs(x = if (nzchar(unit)) {
      if (shape == "sigmoid")
        sprintf("Concentration (%s, log scale)", unit)
      else sprintf("Concentration (%s)", unit)
    } else {
      if (shape == "sigmoid") "Concentration (log scale)"
      else "Concentration"
    },
    y = if (y_percent) "Corrected mortality (%)" else "Corrected mortality") +
    theme_bw(base_size = base_size) +
    theme(
      text = element_text(family = font),
      plot.title = element_blank(),
      plot.margin = margin(0.75, 0.5, 0.2, 0.2, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x.bottom = element_line(color = "black"),
      axis.line.x.top    = element_blank(),
      axis.line.y.left   = element_line(color = "black"),
      axis.line.y.right  = element_blank(),
      axis.line = element_line(linewidth = 0.65),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(10.2, "cm"),
      axis.title = element_text(size = 48),
      axis.text.x = element_text(margin = margin(t = 10)),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 5), hjust = 0.5),
      legend.position = "none",
      panel.border       = element_blank()
    )
  gp
}
