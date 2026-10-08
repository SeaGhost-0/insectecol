#' Analyse Bioassay Data for LC Estimation (Main Function)
#'
#' Non-interactive, fully parameter-driven entry point for the LC
#' (lethal concentration) analysis. It (1) assembles the standardised
#' data list from a data frame, a named list of data frames or three
#' parallel vectors, (2) computes the LC estimates with the selected
#' method(s) via \code{\link{lc50_calculate}} and (3) optionally builds
#' the regression plot(s) in the style of \code{\link{lc50_plot}},
#' optionally written to disk as png when \code{plot_file} is supplied.
#' Tabular export is handled separately by \code{\link{lc50_export}}.
#'
#' @param d Optional; the bioassay data: a data frame with the columns
#'   \code{Concentration}, \code{Tested} and \code{Dead} (headers are
#'   matched loosely, as in \code{\link{lc50_read}}, so a header like
#'   \code{"Concentration (mg/L)"} works), a named list of such data
#'   frames (e.g. the return value of \code{\link{lc50_read}}), or
#'   \code{NULL} to build the data from the three vectors below.
#' @param concentration,tested,dead Numeric vectors; the concentration,
#'   the number of insects tested and the number of dead insects, one
#'   entry per concentration group (replicates = repeated values).
#'   Used only when \code{d} is \code{NULL}.
#' @param name Character; the data set name used in the results and the
#'   saved plot file names when \code{d} is a single data frame or the
#'   vectors are used (default \code{"bioassay"}); ignored for a named
#'   list input.
#' @param lc Numeric; the lethal proportion (default 0.5 = LC50,
#'   e.g. 0.9 = LC90), passed to \code{\link{lc50_calculate}}.
#' @param method Character; one or several of \code{"traditional"},
#'   \code{"improved"}, \code{"probit"} or \code{"all"}, passed to
#'   \code{\link{lc50_calculate}}.
#' @param plot Logical; whether to build the regression plot(s)
#'   (default \code{FALSE}). The ggplot objects are also returned.
#' @param plot_file Optional png path, used with \code{plot = TRUE}. A
#'   path with an extension is the file itself (one data set); a path
#'   without one is a folder, created when missing, and the figure(s)
#'   are written inside it as \code{LC50_<name>.png};
#'   \code{NULL} (default) writes them to the working directory.
#' @param plot_width,plot_height,plot_units,plot_res Physical size and
#'   resolution of the exported png (only used when \code{plot_file} is
#'   supplied); defaults 12 x 8 cm at 300 dpi.
#' @param plot_method Character; which of the computed methods to plot
#'   (default \code{NULL} = the first method that succeeded).
#'   Ignored when \code{plot = FALSE}.
#' @param font,unit,shape,ci,ci_level,error_bar,move_thres,lc_ci,lc_p,lc_lab_gap,lc_lab_gap_right,lc_lab_dy,lc_lab_lh
#'   Plot settings, passed to the internal plot engine exactly as in
#'   \code{\link{lc50_plot}} (\code{unit = NULL} means \code{"mg/L"},
#'   \code{unit = ""} shows no unit).
#' @param export Logical; write the results workbook to disk?
#'   Default \code{FALSE}.
#' @param export_path Output directory for the results workbook;
#'   created when missing. \code{NULL} (default) means
#'   \code{\link{getwd}}.
#' @param export_file File name of the results workbook;
#'   \code{NULL} (default) means \code{<name>_results.xlsx}.
#'   Relative paths are resolved against \code{export_path}; absolute
#'   paths are used as-is. The parent directory is created when it
#'   does not exist. The
#'   written path is returned invisibly in the \code{export_file}
#'   component of the result.
#' @param path Path to a csv file (or a folder with a single csv) in
#'   the LC50 input format; read with \code{\link{lc50_read}} and used
#'   instead of \code{d}/\code{concentration}.
#'
#' @return A list with elements \code{data} (the standardised data
#'   list, one data frame per data set), \code{results} (the list
#'   returned by \code{\link{lc50_calculate}}: \code{results},
#'   \code{summary_df}, \code{lc}), \code{plot} (a named list of
#'   ggplot objects when \code{plot = TRUE}, otherwise \code{NULL})
#'   and \code{plot_file} (the written path(s) when \code{plot = TRUE},
#'   otherwise \code{NULL}) and \code{export_file} (the
#'   workbook path when \code{export = TRUE}, otherwise
#'   \code{NULL}).
#'
#' @seealso \code{\link{lc50_read}}, \code{\link{lc50_calculate}},
#'   \code{\link{lc50_plot}}, \code{\link{lc50_export}},
#'   \code{\link{lc50_export_plot}}
#' @export
#' @examples
#' ## way 1: data frame straight from the package example csv
#' f <- system.file("extdata", "lc50_example.csv", package = "insectecol")
#' out1 <- lc50_analyze(lc50_read(f), method = "probit")
#' out1$results$summary_df
#'
#' ## way 2: three parallel vectors, no csv involved; all three methods
#' conc <- c(0, 1.5, 3, 6, 12, 24)
#' n    <- c(120, 60, 60, 60, 60, 60)
#' dead <- c(7, 9, 18, 32, 48, 57)
#' out2 <- lc50_analyze(concentration = conc, tested = n, dead = dead,
#'                      name = "trial1", method = "all")
#' out2$results$summary_df
#'
#' ## way 3: LC90, improved regression, plot on the linear axis
#' out3 <- lc50_analyze(concentration = conc, tested = n, dead = dead,
#'                      name = "trial1", lc = 0.9, method = "improved",
#'                      plot = TRUE, plot_method = "improved",
#'                      shape = "linear",
#'                      plot_file = file.path(tempdir(), "LC50_linear.png"))
#' invisible(out3$plot$trial1)  # ggplot object: print(), customise, export
#' out3$plot_file          # the png that was written
#'
#' ## --- export: figure (png) and results workbook (xlsx) to disk ---
#' ## plot_file writes the regression png; export = TRUE writes the
#' ## workbook with the LC values, CI and the per-group details.
#' ## export_file accepts an absolute path (the parent directory is
#' ## created when missing); a relative path would be resolved against
#' ## export_path (default getwd()). Writing the workbook takes a few
#' ## seconds, so this last part is not run by default.
#' \donttest{
#' out4 <- lc50_analyze(concentration = conc, tested = n, dead = dead,
#'                      name = "trial1", method = "all",
#'                      plot = TRUE, plot_method = "probit",
#'                      plot_file = file.path(tempdir(), "trial1.png"),
#'                      export = TRUE,
#'                      export_file = file.path(tempdir(),
#'                                              "trial1_results.xlsx"))
#' out4$plot_file        # path of the written png
#' out4$export_file      # path of the written workbook (LC table + details)
#' }
lc50_analyze <- function(d = NULL, concentration = NULL, tested = NULL,
                         dead = NULL, name = "bioassay", path = NULL, lc = 0.5,
                         method = "traditional", plot = FALSE,
                         plot_file = NULL, plot_width = 12,
                         plot_height = 8, plot_units = "cm",
                         plot_res = 300, plot_method = NULL,
                         font = "TNM", unit = NULL,
                         shape = c("sigmoid", "linear"), ci = TRUE,
                         ci_level = 0.95, error_bar = TRUE,
                         move_thres = 0.5, lc_ci = TRUE, lc_p = TRUE,
                         lc_lab_gap = 0.35, lc_lab_gap_right = 0.1,
                         lc_lab_dy = 0.1, lc_lab_lh = 1.5,
                         export = FALSE, export_path = NULL,
                         export_file = NULL) {
  ## ---- 1) assemble the standardised data list ----
  if (!is.null(path)) d <- lc50_read(path)
  lcd <- lc50_build(d, concentration, tested, dead, name = name)

  ## ---- 2) compute the LC estimates ----
  results <- lc50_calculate(lcd, lc = lc, method = method)

  ## ---- 3) optional plots (built, not printed) ----
  plots <- NULL
  if (plot) {
    shape <- match.arg(shape)
    font <- pkg_resolve_font(font)
    plots <- list()
    for (nm in names(results$results)) {
      gp <- lc50_plot_one(nm, results$results[[nm]], font, unit,
                          shape = shape, ci = ci, ci_level = ci_level,
                          error_bar = error_bar, move_thres = move_thres,
                          method = plot_method, lc_ci = lc_ci,
                          lc_p = lc_p, lc_lab_gap = lc_lab_gap,
                          lc_lab_gap_right = lc_lab_gap_right,
                          lc_lab_dy = lc_lab_dy,
                          lc_lab_lh = lc_lab_lh)
      if (is.null(gp)) next
      attr(gp, "lc50_name") <-
        if (shape == "sigmoid") nm else paste0(nm, "_linear")
      plots[[nm]] <- gp
    }
  }

  ## ---- 4) optional png export ----
  plot_file_out <- NULL
  ## no plot_file: the working directory (the figures keep their default
  ## names); a path without an extension is a folder, one with an
  ## extension is the file itself
  pf <- if (is.null(plot_file)) getwd() else plot_file
  if (plot && length(plots)) {
    if (length(plots) == 1L && grepl("\\.[[:alnum:]]+$", pf)) {
      ext <- tolower(tools::file_ext(pf))
      if (!ext %in% c("png", "tiff", "tif", "jpeg", "jpg",
                      "pdf", "eps", "ps", "svg")) ext <- "png"
      plot_file_out <- lc50_export_plot(plots[[1]], path = pf,
                                        device = ext, width = plot_width,
                                        height = plot_height,
                                        dpi = plot_res, units = plot_units,
                                        bg = "white")
    } else {
      if (!dir.exists(pf))
        dir.create(pf, recursive = TRUE, showWarnings = FALSE)
      plot_file_out <- lc50_export_plot(plots, path = pf,
                                        device = "png", width = plot_width,
                                        height = plot_height,
                                        dpi = plot_res, units = plot_units,
                                        bg = "white")
    }
    message("Plot saved to: ",
            paste(normalizePath(as.character(plot_file_out), mustWork = FALSE),
                  collapse = ", "))
  }

  export_file_out <- NULL
  if (export) {
    ep <- if (is.null(export_path)) getwd() else export_path
    if (grepl("\\.(csv|xlsx)$", ep, ignore.case = TRUE))
      warning("export_path looks like a file name (ends in .csv or .xlsx); ",
              "it is used as the output FOLDER and the file is written inside ",
              "it - did you mean export_file?", call. = FALSE)
    if (!dir.exists(ep)) dir.create(ep, recursive = TRUE)
    fn <- if (is.null(export_file)) sprintf("%s_results.xlsx", name)
          else export_file
    if (!grepl("\\.[A-Za-z0-9]+$", fn)) fn <- paste0(fn, ".xlsx")
    export_file_out <- lc50_export(results, output_dir = ep, filename = fn)
  }

  list(data = lcd, results = results, plot = plots, plot_file = plot_file_out,
       export_file = export_file_out)
}

# Internal: assemble the standardised LC data list from a data frame,
# a list of data frames, or three parallel vectors; validation and
# sorting are delegated to lc50_clean()
lc50_build <- function(d = NULL, concentration = NULL, tested = NULL,
                       dead = NULL, name = "bioassay") {
  if (!is.null(d) && (!is.null(concentration) || !is.null(tested) ||
                      !is.null(dead)))
    warning("`d` is supplied; concentration/tested/dead are ignored")
  if (is.null(d)) {
    if (is.null(concentration) || is.null(tested) || is.null(dead))
      stop("Pass either `d` (a data frame or a list of data frames) or ",
           "the three vectors `concentration`, `tested` and `dead`")
    if (diff(range(length(concentration), length(tested), length(dead))) != 0)
      stop("`concentration`, `tested` and `dead` must have the same length")
    d <- data.frame("Concentration" = concentration, "Tested" = tested,
                    "Dead" = dead, check.names = FALSE)
  }
  if (is.data.frame(d))
    return(stats::setNames(list(lc50_clean(d, name)), name))
  if (!is.list(d) || length(d) == 0 ||
      !all(vapply(d, is.data.frame, logical(1))))
    stop("`d` must be a data frame or a (named) list of data frames ",
         "with the columns Concentration/Tested/Dead")
  nms <- names(d)
  if (is.null(nms) || any(!nzchar(nms))) nms <- paste0("dataset", seq_along(d))
  stats::setNames(lapply(seq_along(d), function(i) lc50_clean(d[[i]], nms[i])),
                  nms)
}