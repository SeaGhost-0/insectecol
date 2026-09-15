#' LC50 Regression Plots
#'
#' Plots every data set: observed points, the fitted curve of the computed
#' method with its pointwise confidence band, and dashed reference lines
#' marking the LC estimate, which is itself marked by a circle where it
#' lies on the fitted curve. By default the concentration axis is on a
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
#' @param lc_ci Logical (default \code{TRUE}): show the 95% confidence
#'   interval of the LC estimate as a second line of the LC reference
#'   label, e.g. \code{(0.98-1.55)} below \code{LC50 = 1.23 mg/L}.
#'   \code{FALSE} omits the line.
#' @param lc_p Logical (default \code{TRUE}): append the chi-square
#'   goodness-of-fit result (\code{chi-square} statistic and \code{P}
#'   value) as an additional line of the LC reference label.
#'   \code{FALSE} omits the line.
#' @param lc_lab_gap Clearance between the vertical reference line and
#'   the near edge of the LC label when the label sits LEFT of the line,
#'   in text widths of the label itself (default 0.35). Larger pushes
#'   the label further away from the line; smaller moves it towards it.
#' @param lc_lab_gap_right The same clearance when the label sits RIGHT
#'   of the line (default 0.1, smaller than \code{lc_lab_gap} because
#'   the label then hangs below the crossing, where a smaller gap keeps
#'   it closer to the reference line).
#' @param lc_lab_dy Clearance between the LC label block and the LC
#'   crossing, in y-axis units (default 0.1): the distance from the
#'   crossing to the edge of the block that faces it. The block is
#'   placed in the diagonal quadrant the fitted curve never enters
#'   (above the crossing when the label sits left of the vertical
#'   reference line, below it when the label sits right), anchored by
#'   that facing edge, so adding lines or changing \code{lc_lab_lh}
#'   grows the block away from the crossing and never onto the dashed
#'   reference line. Larger moves the whole block further from it.
#' @param lc_lab_lh Line spacing of the LC label in multiples of its
#'   font size (1 = single spacing, default 1.05). The lines are spaced
#'   evenly whichever of them \code{lc_ci} / \code{lc_p} switches on.
#' @param method Character scalar, which methods to plot: a subset of
#'   \code{c("traditional", "improved", "probit")}, or \code{"all"}
#'   (default) for every method present in the results object.
#'
#' @details Replicates of the same concentration are pooled and drawn as
#'   the Abbott-corrected pooled mortality with Wilson score intervals.
#'   The LC reference label is centred around the crossing of the two
#'   dashed reference lines. Because the sigmoid only ever passes
#'   through the lower-left and upper-right quadrants around that
#'   crossing, the label is placed in one of the two free ones: above
#'   the crossing when it sits left of the vertical reference line,
#'   below it when it sits right, at a clearance of \code{lc_lab_dy}.
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
                      error_bar = TRUE, move_thres = 0.5, method = NULL,
                      lc_ci = TRUE, lc_p = TRUE,
                      lc_lab_gap = 0.35, lc_lab_gap_right = 0.1,
                      lc_lab_dy = 0.1, lc_lab_lh = 1.05) {
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
                        method = method, lc_ci = lc_ci, lc_p = lc_p,
                        lc_lab_gap = lc_lab_gap,
                        lc_lab_gap_right = lc_lab_gap_right,
                        lc_lab_dy = lc_lab_dy, lc_lab_lh = lc_lab_lh,
                        fig_w = width, fig_h = height)
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

# Width of a label line relative to the width of the widest line of the
# label, used to pull the widest line back to the common centre. Only
# ratios matter and showtext draws without kerning, so a plain table of
# character advances is accurate enough: serif = TRUE gives the Times
# metrics (shared by the default "TNM" font and by the Liberation Serif
# shipped with the package), serif = FALSE the Helvetica metrics used as
# the fallback for sans fonts
lc50_line_width <- function(txt, serif = TRUE) {
  adv <- if (serif) {
    c(" " = 250, "-" = 333, "." = 250, "," = 250, ":" = 278, ";" = 333,
      "(" = 333, ")" = 333, "/" = 278, "+" = 564, "=" = 564, "<" = 564,
      ">" = 564, "%" = 889, "'" = 180, "*" = 389,
      "0" = 500, "1" = 500, "2" = 500, "3" = 500, "4" = 500, "5" = 500,
      "6" = 500, "7" = 500, "8" = 500, "9" = 500,
      "A" = 667, "B" = 667, "C" = 667, "D" = 722, "E" = 611, "F" = 611,
      "G" = 722, "H" = 722, "I" = 278, "J" = 500, "K" = 667, "L" = 556,
      "M" = 833, "N" = 667, "O" = 722, "P" = 556, "Q" = 722, "R" = 667,
      "S" = 556, "T" = 611, "U" = 722, "V" = 611, "W" = 833, "X" = 611,
      "Y" = 556, "Z" = 556,
      "a" = 444, "b" = 500, "c" = 444, "d" = 500, "e" = 444, "f" = 278,
      "g" = 500, "h" = 500, "i" = 278, "j" = 278, "k" = 444, "l" = 278,
      "m" = 778, "n" = 500, "o" = 500, "p" = 500, "q" = 500, "r" = 333,
      "s" = 389, "t" = 278, "u" = 500, "v" = 444, "w" = 667, "x" = 444,
      "y" = 444, "z" = 389)
  } else {
    c(" " = 278, "-" = 333, "." = 278, "," = 278, ":" = 278, ";" = 278,
      "(" = 333, ")" = 333, "/" = 278, "+" = 584, "=" = 584, "<" = 584,
      ">" = 584, "%" = 889, "'" = 191, "*" = 389,
      "0" = 556, "1" = 556, "2" = 556, "3" = 556, "4" = 556, "5" = 556,
      "6" = 556, "7" = 556, "8" = 556, "9" = 556,
      "A" = 667, "B" = 667, "C" = 722, "D" = 722, "E" = 667, "F" = 611,
      "G" = 778, "H" = 722, "I" = 278, "J" = 500, "K" = 667, "L" = 556,
      "M" = 833, "N" = 722, "O" = 778, "P" = 667, "Q" = 778, "R" = 722,
      "S" = 667, "T" = 611, "U" = 722, "V" = 667, "W" = 944, "X" = 667,
      "Y" = 667, "Z" = 611,
      "a" = 556, "b" = 556, "c" = 500, "d" = 556, "e" = 556, "f" = 278,
      "g" = 556, "h" = 556, "i" = 222, "j" = 222, "k" = 500, "l" = 222,
      "m" = 833, "n" = 556, "o" = 556, "p" = 556, "q" = 556, "r" = 333,
      "s" = 500, "t" = 278, "u" = 556, "v" = 500, "w" = 722, "x" = 500,
      "y" = 500, "z" = 500)
  }
  ch <- strsplit(txt, "")[[1]]            # -> vector of single characters
  if (length(ch) == 0) return(0)
  wk <- adv[ch]
  wk[is.na(wk)] <- if (serif) 500 else 556   # characters missing from the table
  sum(wk) / 1000
}

# Height of the plotting panel in inches, for a figure of fig_w x fig_h
# inches. The panel rows of the assembled gtable are labelled "panel", so
# their height can simply be read back once the plot is built; the panel
# is what the y range 0-1 is mapped to, which is exactly what is needed
# to convert a length in inches into y-axis units. The dummy plot only
# needs the same theme and axis text, and is built at the figure size the
# real plot will be drawn at, so it measures the panel of the real plot.
lc50_panel_size <- function(fig_w = 7, fig_h = 6) {
  dummy <- ggplot2::ggplot(data.frame(x = 1, y = 1),
                           ggplot2::aes(.data[["x"]], .data[["y"]])) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(labels = function(v) sprintf("%g", v)) +
    ggplot2::scale_y_continuous(labels = function(v) sprintf("%g", v)) +
    ggplot2::labs(x = "Concentration (mg/L, log scale)",
                  y = "Corrected mortality") +
    lc50_plot_theme(45, "serif")
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(dummy))
  rows <- gt$layout$t[gt$layout$name == "panel"]
  cols <- gt$layout$l[gt$layout$name == "panel"]
  h <- grid::convertHeight(gt$heights[rows[1]], "inches", valueOnly = TRUE)
  w <- grid::convertWidth(gt$widths[cols[1]], "inches", valueOnly = TRUE)
  if (!is.finite(h) || h <= 0) h <- fig_h * 0.7
  if (!is.finite(w) || w <= 0) w <- fig_w * 0.8
  c(width = w, height = h)
}

lc50_panel_height <- function(fig_w = 7, fig_h = 6) {
  unname(lc50_panel_size(fig_w, fig_h)["height"])
}

lc50_panel_width <- function(fig_w = 7, fig_h = 6) {
  unname(lc50_panel_size(fig_w, fig_h)["width"])
}

# Plot of a single file (the method whose computation succeeded).
# shape = "sigmoid": log10 concentration axis (symmetric S curve);
# shape = "linear": original linear concentration axis.
lc50_plot_one <- function(nm, one, font, unit = NULL,
                          shape = c("sigmoid", "linear"),
                          ci = TRUE, ci_level = 0.95,
                          error_bar = TRUE, move_thres = 0.5,
                          method = NULL, lc_ci = TRUE, lc_p = TRUE,
                          lc_lab_gap = 0.35, lc_lab_gap_right = 0.1,
                          lc_lab_dy = 0.1, lc_lab_lh = 1.05,
                          fig_w = 7, fig_h = 6) {

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
  # The LC label as up to three plain-text lines: line 1 the estimate,
  # line 2 (lc_ci = TRUE) the 95% CI of the estimate, line 3
  # (lc_p = TRUE) the chi-square goodness-of-fit result. Plain text
  # (parse = FALSE) keeps the label narrow and ASCII-safe on every R
  # version and platform
  lc_lines <- if (nzchar(unit)) {
    sprintf("LC%d = %.3g %s", round(lc_y * 100), lc_real, unit)
  } else {
    sprintf("LC%d = %.3g", round(lc_y * 100), lc_real)
  }
  if (lc_ci && !is.null(r$lower) && !is.null(r$upper) &&
      is.finite(r$lower) && is.finite(r$upper))
    lc_lines <- c(lc_lines, sprintf("(%.3g-%.3g)", r$lower, r$upper))
  if (lc_p && !is.null(r$chisq) && !is.null(r$p_chi) &&
      is.finite(r$chisq) && is.finite(r$p_chi)) {
    p_txt <- if (r$p_chi < 0.001) "< 0.001" else sprintf("= %.3f", r$p_chi)
    lc_lines <- c(lc_lines, sprintf("chi-square = %.2f, P %s",
                                    r$chisq, p_txt))
  }
  # The lines are drawn one by one further down, each centred on the same
  # vertical axis (see the annotate calls), so they need no padding here
  serif_font <- !grepl("sans|arial|helvet|calibri", font, ignore.case = TRUE)

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
  # below on that side). lc_lab_gap / lc_lab_gap_right / lc_lab_dy
  # fine-tune the position.
  #
  # Each line of the label gets its own text layer, centred on the same
  # vertical axis x_lab: grid justifies the lines of a single multi-line
  # string by each line's own width, which pushes lines of unequal width
  # apart as soon as hjust leaves [0, 1] (it does here, the label being
  # anchored beside the dashed line), and on top of that the block would
  # cover the crossing it is meant to annotate. Drawing the block around
  # the crossing instead costs nothing and keeps lc_lab_dy meaning "offset
  # of the block from the crossing".
  x_mid <- (x_lo + x_hi) / 2
  x_lab_w <- x_hi - x_lo
  lc_lab_pt <- 25                    # LC label font, in points on the device
  lc_lab_size <- lc_lab_pt / ggplot2::.pt
  n_lab <- length(lc_lines)
  # The block is centred on x_lab, so half of the widest line must stay
  # inside the panel; that half width is measured in em from the advance
  # table and converted to x-axis units with the panel width in inches
  lab_w_em <- max(lc50_line_width(lc_lines, serif = serif_font))
  lab_w_panel <- lab_w_em * (lc_lab_pt / 72) / lc50_panel_width(fig_w, fig_h)
  lab_half <- lab_w_panel * x_lab_w / 2
  x_lab <- if (lc_x < x_mid) {
    min(lc_x + lc_lab_gap_right * 2 * lab_half + lab_half, x_hi - lab_half)
  } else {
    max(lc_x - lc_lab_gap * 2 * lab_half - lab_half, x_lo + lab_half)
  }
  # The near edge of the block (the one facing the dashed line) keeps a
  # fixed clearance from the line, in text widths of the label itself:
  # lc_lab_gap when the label sits left of the line, lc_lab_gap_right
  # when it sits right (smaller by default, see the roxygen comments);
  # the panel edge clips the centre when there is no room left

  # The sigmoid rises from the lower left to the upper right, so of the
  # four quadrants around the crossing only two are ever free of it:
  # above-left and below-right. The label is placed in whichever of the
  # two its horizontal placement has already chosen, and anchored by
  # the edge that faces the crossing, so that growing the block (more
  # lines, larger lc_lab_lh) always grows it away from the crossing and
  # never onto the dashed reference line:
  #   label left  of the line -> block above, bottom edge anchored;
  #   label right of the line -> block below, top edge anchored.
  # The clearance lc_lab_dy is the distance from the crossing to that
  # anchored edge either way. If the block does not fit on that side
  # (the crossing sits too close to the panel edge), it falls back to
  # the other side; if it fits nowhere, it is clamped into the panel.
  lab_panel_in <- lc50_panel_height(fig_w, fig_h)
  lab_pitch <- lc_lab_lh * (lc_lab_pt / 72) / lab_panel_in
  lab_h <- (n_lab - 1) * lab_pitch   # top line to bottom line
  lab_gap <- lc_lab_dy               # clearance to the crossing
  lab_right <- lc_x < x_mid          # label sits right of the line
  if (lab_right) {
    lab_top <- lc_y - lab_gap        # below the crossing, top anchored
    lab_bottom <- lab_top - lab_h
    if (lab_bottom < 0.5 * lab_pitch) {
      lab_bottom <- lc_y + lab_gap   # does not fit: above instead
      lab_top <- lab_bottom + lab_h
    }
  } else {
    lab_bottom <- lc_y + lab_gap     # above the crossing, bottom anchored
    lab_top <- lab_bottom + lab_h
    if (lab_top > 1 - 0.5 * lab_pitch) {
      lab_top <- lc_y - lab_gap      # does not fit: below instead
      lab_bottom <- lab_top - lab_h
    }
  }
  # keep the block inside the panel (the crossing can sit near either
  # edge, e.g. a very low or very high LC), shifting it by the smallest
  # amount that brings it back in
  if (lab_bottom < 0.5 * lab_pitch) {
    lab_bottom <- 0.5 * lab_pitch
    lab_top <- lab_bottom + lab_h
  }
  if (lab_top > 1 - 0.5 * lab_pitch) {
    lab_top <- 1 - 0.5 * lab_pitch
    lab_bottom <- lab_top - lab_h
  }

  # NEW: labels moved into the panel hug their dashed line by default
  # and flip to the other side only when they would not fit between the
  # line and the panel edge, or (x) when the LC label already occupies
  # that corner; the width estimate is one digit per character
  x_val_txt <- sprintf("%.3g", lc_real)
  x_val_w <- 0.045 * nchar(x_val_txt) + 0.02
  x_val_left <- (x_hi - lc_x) < x_val_w * (x_hi - x_lo) ||
    (lc_x < x_mid && lab_bottom < 0.21)
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
    # Where both dashed reference lines meet: the LC estimate on the
    # fitted curve. Drawn with a white fill, so the curve passes through
    # the marker instead of vanishing behind it
    annotate("point",
             x = lc_x, y = lc_y,
             shape = 21, size = 2.2, stroke = 0.9,
             fill = "white", color = col) +
    coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(0, 1),
                    expand = FALSE, clip = "off")

  # LC label: one text layer per line, every line centred on the same
  # vertical axis x_lab (all of them hjust = 0.5) and stacked at the
  # pitch computed above, so the lines line up on one another no matter
  # how different their widths are
  for (i in seq_along(lc_lines)) {
    gp <- gp + annotate("text",
                        x = x_lab, y = lab_top - (i - 1) * lab_pitch,
                        label = lc_lines[i],
                        hjust = 0.5, vjust = 0.5,
                        size = lc_lab_size, fontface = "bold",
                        family = font, color = col)
  }

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
    lc50_plot_theme(base_size, font)
  gp
}

# The theme shared by the LC plots and by the dummy plot that measures the
# panel height, so both have exactly the same margins and axis text
lc50_plot_theme <- function(base_size, font) {
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
}
