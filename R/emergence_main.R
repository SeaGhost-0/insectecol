# ============================================================
# insectecol --- Emergence-period module: main entry
# Non-interactive, parameter-driven one-call API, mirroring
# gdd_analyze() (degree days) and lc50_analyze() (bioassay):
# obtain data -> cumulative development -> quantile dates ->
# optional plot, always written to disk.
# ============================================================

#' Predict the Emergence Period from a Stage-Structure Survey
#'
#' Non-interactive, fully parameter-driven entry point for the
#' emergence-period module (the stage-grading method: one field
#' survey of the population stage structure --- e.g. a
#' dissected-sample count of pupal grades --- is turned into the
#' projected eclosion dates of the 16% / 50% / 84% quantiles, i.e.
#' the beginning, peak and end of the adult emergence period). In
#' the same style as \code{\link{gdd_analyze}} and
#' \code{\link{lc50_analyze}}, it (1) obtains the data --- user
#' column vectors (\code{stage = d$stage, count = d$n, days =
#' d$days}), a whole data frame, or a csv/xlsx file / folder read
#' via \code{\link{emergence_read}} ---, (2) computes the
#' cumulative development and the interpolated quantile dates via
#' \code{\link{emergence_calc}} and (3) optionally writes the
#' projection figure to disk (\code{plot_file}, or the working
#' directory under a default name). Larval hatch dates
#' are projected as well when \code{pre_ovip} / \code{egg_days}
#' are supplied. Tabular export is handled separately by
#' \code{\link{emergence_export}}.
#'
#' @param stage,count,days User-supplied column vectors, e.g.
#'   \code{stage = d$stage, count = d$n, days = d$days} after
#'   \code{d <- read.csv("XXX.csv")}: the stage names, the
#'   individuals per stage and the average days from that stage to
#'   adult eclosion. \code{percent} may be used instead of
#'   \code{count} when the survey recorded percentages. When
#'   supplied, these vectors take precedence over \code{data} and
#'   \code{path}.
#' @param percent Alternative to \code{count}: the stage shares in
#'   percent (any scaling works; the shares are normalised).
#' @param data A data.frame with a stage column, a count (or
#'   percent) column and a days column. Used when the vectors are
#'   not supplied; takes precedence over \code{path}.
#' @param path Optional; path to a csv/xlsx file or a folder (batch
#'   mode), read with \code{\link{emergence_read}}. Used only when
#'   neither the column vectors nor \code{data} are supplied.
#' @param stage_col,count_col,percent_col,days_col Column names;
#'   auto-detected by default (ignored when the column vectors are
#'   supplied).
#' @param survey_date The survey date: a \code{Date} or a character
#'   string (\code{"2026-03-20"}, \code{"2026/3/20"}). Required.
#' @param p Numeric vector of emergence quantiles, default
#'   \code{c(0.16, 0.5, 0.84)} (beginning / peak / end).
#' @param labels Optional labels of the quantiles, see
#'   \code{\link{emergence_calc}}.
#' @param pre_ovip Pre-oviposition period in days (default 0 = not
#'   used). Counted from female eclosion to egg deposition.
#' @param egg_days Egg duration in days (default 0 = not used).
#' @param encoding,header,pattern Reading options for
#'   \code{\link{emergence_read}} (only used when \code{path} is
#'   supplied).
#' @param plot Logical; whether to write the projection figure to
#'   disk (default \code{FALSE}). With \code{plot = TRUE} the figure
#'   is always written, to \code{plot_file} when supplied, otherwise
#'   to the working directory under \code{emergence_plot.png}.
#' @param plot_file Optional path of the exported figure, used with
#'   \code{plot = TRUE}: a path with an extension is the file itself
#'   (the format follows the extension --- png, tiff and jpeg are
#'   supported), a path without one is a folder, created when
#'   missing, and the figure is written inside it; \code{NULL}
#'   (default) means the working directory under
#'   \code{emergence_plot.png}. The path written is returned as
#'   \code{plot_file}. The projection can still be drawn on screen
#'   at any time with \code{plot(fit)} on the returned \code{fit}.
#' @param show_hatch Plot option, see \code{\link{plot.emergence}}.
#' @param plot_title,plot_sub,plot_xlab,plot_ylab Plot options
#'   (title, subtitle, axis labels); \code{NULL} keeps the
#'   defaults of \code{\link{plot.emergence}}.
#' @param plot_family Text font family, see \code{\link{plot.emergence}}
#'   (\code{NULL} keeps the default \code{"serif"} --- Times New
#'   Roman on 'Windows'; Chinese characters are rendered through the
#'   device's font fallback, i.e. SimSun on Chinese 'Windows').
#' @param plot_width,plot_height,plot_units,plot_res Physical size
#'   and resolution of the exported figure (only used with
#'   \code{plot = TRUE}), same semantics as in
#'   \code{\link{gdd_analyze}}: the composition is identical at
#'   every resolution, \code{plot_res} only adds pixels.
#' @param ... Further arguments passed to \code{\link{emergence_calc}}
#'   (reserved for future options; keeps user code forward
#'   compatible).
#' @param export Logical; write the results document to disk?
#'   Default \code{FALSE}.
#' @param export_path Output directory for the results document;
#'   created when missing. \code{NULL} (default) means
#'   \code{\link{getwd}}.
#' @param export_file File name of the results document
#'   (\code{.xlsx} or \code{.csv}); \code{NULL} (default) means
#'   \code{emergence_results.xlsx}. Relative paths are resolved
#'   against \code{export_path}; absolute paths are used as-is. The
#'   parent directory is created when it does not exist. The
#'   written path is returned
#'   invisibly in the \code{export_file} component of the result.
#'
#' @return A list with components:
#'   \item{data}{the survey table actually analysed}
#'   \item{fit}{the \code{"emergence"} object returned by
#'     \code{\link{emergence_calc}} --- \code{fit$predictions}
#'     (the quantile dates), \code{fit$table} (cumulative
#'     development), \code{fit$interpolate} (a closure for
#'     arbitrary quantiles); print / summary / plot / predict S3
#'     methods are available}
#'   \item{plot_file}{the path of the written figure when
#'     \code{plot = TRUE}, otherwise \code{NULL}}
#'   \item{export_file}{the results-document path when
#'     \code{export = TRUE}, otherwise \code{NULL}}
#' @seealso \code{\link{emergence_read}},
#'   \code{\link{emergence_calc}}, \code{\link{emergence_export}},
#'   \code{\link{emergence_export_plot}}
#' @examples
#' f <- system.file("extdata", "emergence_example.csv",
#'                  package = "insectecol")
#'
#' ## --- way 1 (recommended): read the file yourself, pass columns in ---
#' d <- read.csv(f)
#' out <- emergence_analyze(stage = d$stage, count = d$count,
#'                          days = d$days, survey_date = "2026-03-20")
#' out$fit                            # three quantile dates
#' out$fit$table                      # cumulative development
#' predict(out$fit, c(0.25, 0.75))    # arbitrary quantiles
#'
#' ## --- way 2: pass the whole data frame ---
#' out2 <- emergence_analyze(data = d, survey_date = "2026-03-20")
#'
#' ## --- way 3: let the function read the file ---
#' out3 <- emergence_analyze(path = f, survey_date = "2026-03-20")
#'
#' ## --- hatch projection + png export + results workbook ---
#' ## plot_file writes the projection png; export = TRUE writes the
#' ## results workbook (quantile dates + cumulative development table).
#' ## export_file accepts an absolute path (the parent directory is
#' ## created when missing), so export_path is not needed here.
#' ## plot_title / plot_xlab / plot_ylab accept custom labels; Chinese
#' ## labels are rendered through the device's font fallback (SimSun
#' ## on Chinese Windows)
#' out4 <- emergence_analyze(path = f, survey_date = "2026-03-20",
#'                           pre_ovip = 3, egg_days = 10,
#'                           plot = TRUE,
#'                           plot_file = file.path(tempdir(), "emergence.png"),
#'                           export = TRUE,
#'                           export_file = file.path(tempdir(),
#'                                                   "emergence_results.xlsx"))
#' out4$fit$predictions
#' out4$plot_file            # path of the written png
#' out4$export_file          # path of the written workbook
#' @export
emergence_analyze <- function(stage = NULL, count = NULL,
                              percent = NULL, days = NULL,
                              data = NULL, path = NULL,
                              stage_col = NULL, count_col = NULL,
                              percent_col = NULL, days_col = NULL,
                              survey_date, p = c(0.16, 0.5, 0.84),
                              labels = NULL, pre_ovip = 0,
                              egg_days = 0,
                              encoding = "UTF-8", header = TRUE,
                              pattern = "\\.(csv|xlsx|xls)$",
                              plot = FALSE, plot_file = NULL,
                              show_hatch = TRUE,
                              plot_title = NULL, plot_sub = NULL,
                              plot_xlab = NULL, plot_ylab = NULL,
                              plot_family = NULL,
                              plot_width = 10.67, plot_height = 6,
                              plot_units = c("in", "cm", "px"),
                              plot_res = 150,
                              export = FALSE, export_path = NULL,
                              export_file = NULL, ...) {
  plot_units <- match.arg(plot_units)

  ## ---- 1) obtain the survey data ----
  ## priority: user-supplied column vectors > data frame > path
  if (!is.null(stage) || !is.null(count) || !is.null(percent) ||
      !is.null(days)) {
    if (!is.null(data))
      warning("Both the column vectors and 'data' are supplied; ",
              "the vectors are used and 'data' is ignored.",
              call. = FALSE)
    if (!is.null(path))
      warning("Both the column vectors and 'path' are supplied; ",
              "the vectors are used and 'path' is ignored.",
              call. = FALSE)
    if (is.null(stage) || is.null(days))
      stop("The stage and days vectors are required.", call. = FALSE)
    if (!xor(is.null(count), is.null(percent)))
      stop("Supply exactly one of the count and percent vectors.",
           call. = FALSE)
    vals <- if (is.null(count)) percent else count
    vals <- suppressWarnings(as.numeric(as.character(vals)))
    days_v <- suppressWarnings(as.numeric(as.character(days)))
    stage <- as.character(stage)
    if (length(stage) != length(vals) || length(stage) != length(days_v))
      stop("stage, count/percent and days must have the same length.",
           call. = FALSE)
    if (!length(stage))
      stop("The survey vectors must be non-empty.", call. = FALSE)
    if (is.null(count)) {
      data <- data.frame(stage = stage, percent = vals, days = days_v,
                         stringsAsFactors = FALSE)
      stage_col <- "stage"; percent_col <- "percent"; count_col <- NULL
    } else {
      data <- data.frame(stage = stage, count = vals, days = days_v,
                         stringsAsFactors = FALSE)
      stage_col <- "stage"; count_col <- "count"; percent_col <- NULL
    }
    days_col <- "days"
  } else if (!is.null(data)) {
    if (!is.null(path))
      warning("Both 'data' and 'path' supplied; 'data' is used and ",
              "'path' is ignored.", call. = FALSE)
    if (!is.data.frame(data)) data <- as.data.frame(data)
  } else if (!is.null(path)) {
    data <- emergence_read(path, encoding = encoding, header = header,
                           pattern = pattern)
  } else {
    stop("Supply the survey via stage/count/days (column vectors), ",
         "via 'data' (a data frame) or via 'path' (a file or folder).",
         call. = FALSE)
  }

  ## ---- 2) cumulative development + quantile dates ----
  fit <- emergence_calc(data, stage_col = stage_col,
                        count_col = count_col,
                        percent_col = percent_col,
                        days_col = days_col,
                        survey_date = survey_date, p = p,
                        labels = labels, pre_ovip = pre_ovip,
                        egg_days = egg_days, ...)

  ## ---- 3) optional plot (always written to disk) ----
  plot_file_out <- NULL
  if (plot) {
    ## plot = TRUE always writes the figure, the same contract as
    ## lifeTable_analyze() and lc50_analyze(): to plot_file when
    ## supplied (a path with an extension is the file itself, one
    ## without an extension is a folder), otherwise to the working
    ## directory under emergence_plot.png. The figure is exported by
    ## emergence_export_plot() --- calling plot() here would hit the
    ## local logical argument `plot`, which shadows graphics::plot()
    pf <- pkg_plot_path(plot_file, "emergence_plot.png")
    plot_file_out <- emergence_export_plot(
      fit, file = pf, show_hatch = show_hatch,
      title = plot_title, sub = plot_sub, xlab = plot_xlab,
      ylab = plot_ylab, family = plot_family,
      width = plot_width, height = plot_height, units = plot_units,
      res = plot_res)
  }

  export_file_out <- NULL
  if (export) {
    ep <- if (is.null(export_path)) getwd() else export_path
    if (grepl("\\.(csv|xlsx)$", ep, ignore.case = TRUE))
      warning("export_path looks like a file name (ends in .csv or .xlsx); ",
              "it is used as the output FOLDER and the file is written inside ",
              "it - did you mean export_file?", call. = FALSE)
    if (!dir.exists(ep)) dir.create(ep, recursive = TRUE)
    fn <- if (is.null(export_file)) "emergence_results.xlsx" else export_file
    if (!grepl("\\.(csv|xlsx)$", fn, ignore.case = TRUE))
      fn <- paste0(fn, ".xlsx")
    fx <- if (.is_abs_path(fn)) fn else file.path(ep, fn)
    if (!dir.exists(dirname(fx)))
      dir.create(dirname(fx), recursive = TRUE, showWarnings = FALSE)
    export_file_out <- emergence_export(fit, file = fx)
  }

  list(data = data, fit = fit, plot_file = plot_file_out,
       export_file = export_file_out)
}
