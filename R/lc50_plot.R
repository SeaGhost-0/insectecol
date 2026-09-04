#' LC50 Regression Plots
#'
#' Plots every data set: observed points, the fitted line of the computed
#' method and dashed reference lines marking the LC estimate. Whenever the
#' lethal proportion or the LC estimate does not coincide with a regular
#' tick, an extra, shorter tick labelled with that value is added, its
#' label sitting closer to the axis than the regular labels.
#'
#' @param results Result list of \code{\link{lc50_calculate}}.
#' @param save_path Folder for the png files; \code{NULL} (default)
#'   displays the plots only.
#' @param font Font family (default \code{"TNM"}).
#' @param width,height Figure size in inches (default 7 x 6).
#' @param dpi Resolution of the saved files (default 300); at any dpi the
#'   figures keep the physical size they have at 300 dpi.
#' @param unit Unit of the concentration (e.g. \code{"mg/L"}), appended to
#'   the LC label; \code{NULL} (default) shows none.
#'
#' @return Named list of ggplot objects (invisibly).
#' @seealso \code{\link{save_lc50}}, \code{\link{save_lc50_plot}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' res <- lc50_calculate(read_lc50(f))
#' plots <- plot_lc50(res, save_path = tempdir())
plot_lc50 <- function(results, save_path = NULL, font = "TNM",
                      width = 7, height = 6, dpi = 300, unit = NULL) {
  showtext::showtext_auto(enable = TRUE)
  font <- pkg_resolve_font(font)

  plot_list <- list()
  for (nm in names(results$results)) {
    gp <- lc50_plot_one(nm, results$results[[nm]], font, unit)
    if (is.null(gp)) next
    attr(gp, "lc50_name") <- nm   # used by save_lc50_plot() for auto-naming
    plot_list[[nm]] <- gp
    if (!is.null(save_path)) {
      lc50_ggsave(file.path(save_path, paste0("LC50_", nm, ".png")),
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

# Plot of a single file (the method whose computation succeeded)
lc50_plot_one <- function(nm, one, font, unit = NULL) {
  # Take the first method whose computation succeeded
  r <- NULL
  for (key in names(one)) {
    if (!is.null(one[[key]]$estimate)) {
      r <- one[[key]]
      break
    }
  }
  if (is.null(r)) return(NULL)

  col <- unname(lc50_method_colors[r$method])
  if (is.na(col)) col <- "#E69F00"

  prep <- r$prep
  pts <- data.frame("Conc" = prep[["Concentration"]],
                    "Mortality" = attr(prep, "p"),
                    check.names = FALSE)
  xs <- seq(min(prep[["Concentration"]]), max(prep[["Concentration"]]),
            length.out = 100)
  # A glm is probit analysis; otherwise linear regression (where probit
  # includes the +5 baseline)
  ys <- if (inherits(r$fit, "glm")) {
    stats::pnorm(stats::predict(r$fit,
                                newdata = data.frame(x = log10(xs)),
                                type = "link"))
  } else {
    stats::pnorm(stats::predict(r$fit,
                                newdata = data.frame(x = log10(xs))) - 5)
  }
  curve <- data.frame("Conc" = xs, "Mortality" = as.numeric(ys),
                      check.names = FALSE)

  lc_x <- r$estimate
  lc_y <- r$lc
  if (is.null(unit)) unit <- ""
  lc_label <- if (nzchar(unit)) {
    sprintf('LC[%d] == %.3g~"%s"', round(lc_y * 100), lc_x, unit)
  } else {
    sprintf("LC[%d] == %.3g", round(lc_y * 100), lc_x)
  }

  # Panel range (x extended by 5% on each side, y fixed to 0-1) and the
  # regular ticks; extra ticks are added only when the reference line's
  # landing point is not on a regular tick
  x_rng <- range(prep[["Concentration"]])
  x_w <- x_rng[2] - x_rng[1]
  x_lo <- x_rng[1] - 0.05 * x_w
  x_hi <- x_rng[2] + 0.05 * x_w
  if (x_w <= 0) {                 # only one concentration
    x_lo <- x_rng[1] - 0.05
    x_hi <- x_rng[2] + 0.05
  }

  y_ticks_reg <- seq(0, 1, 0.2)
  y_labels_reg <- lc50_tick_labels(y_ticks_reg)
  y_extra <- !lc50_on_tick(lc_y, y_ticks_reg)

  x_ticks_reg <- scales::extended_breaks()(c(x_lo, x_hi))
  x_ticks_reg <- x_ticks_reg[x_ticks_reg >= x_lo & x_ticks_reg <= x_hi]
  x_labels_reg <- lc50_tick_labels(x_ticks_reg)
  x_extra <- length(x_ticks_reg) > 0 && !lc50_on_tick(lc_x, x_ticks_reg)

  # LC label: by default at the lower right of the reference-line
  # intersection; flipped to the lower left when the LC is too far right,
  # to prevent it from going out of bounds
  lc_lab_dy <- 0.08
  lc_lab_gap <- 0.1
  flip_at <- 0.7
  lab_hjust <- if (x_w > 0 && (lc_x - x_rng[1]) / x_w > flip_at) {
    1 + lc_lab_gap
  } else {
    -lc_lab_gap
  }
  lc_lab_y <- max(lc_y - lc_lab_dy, 0.02)   # keep inside the panel even at tiny mortality

  # Sizes of the hand-drawn axis elements
  base_size <- 45
  tick_len_x <- 0.04
  tick_len_y <- 0.025 * (x_hi - x_lo)
  tick_len_ratio <- 0.6                      # extra tick / regular tick
  tick_lab_gap <- 0.15
  axis_text_col <- "grey10"
  axis_lab_size <- 0.8 * base_size / ggplot2::.pt

  gp <- ggplot() +
    geom_point(data = pts,
               aes(x = .data[["Conc"]], y = .data[["Mortality"]]),
               size = 0.75) +
    geom_line(data = curve,
              aes(x = .data[["Conc"]], y = .data[["Mortality"]]),
              color = col, linewidth = 0.8) +
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

  # The theme's native ticks are off; all ticks are drawn by hand, the one
  # for the reference line being shorter
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

  # Value labels of the extra ticks, hugging the end of the short tick and
  # offset from the regular labels; styled like the axis text
  if (x_extra) {
    gp <- gp + annotate("text",
                        x = lc_x, y = -tick_len_ratio * tick_len_x,
                        label = sprintf("%.3g", lc_x),
                        hjust = 0.5, vjust = 1.2 + tick_lab_gap,
                        size = axis_lab_size, family = font,
                        color = axis_text_col)
  }
  if (y_extra) {
    gp <- gp + annotate("text",
                        x = x_lo - tick_len_ratio * tick_len_y, y = lc_y,
                        label = sprintf("%.3g", lc_y),
                        hjust = 1 + tick_lab_gap, vjust = 0.5,
                        size = axis_lab_size, family = font,
                        color = axis_text_col)
  }

  # The extra values are not fed into scales (limits would drop hand-drawn
  # ticks outside the panel to NA); the panel is fixed by coord_cartesian()
  if (length(x_ticks_reg) > 0) {
    gp <- gp + scale_x_continuous(breaks = x_ticks_reg,
                                  labels = x_labels_reg)
  }
  gp <- gp +
    scale_y_continuous(breaks = y_ticks_reg, labels = y_labels_reg) +
    labs(x = "Concentration", y = "Mortality") +
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
