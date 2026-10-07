# ============================================================
# insectecol --- Degree-day module: main entry
# Non-interactive, parameter-driven one-call API, mirroring
# lifeTable_analyze() (life table) and lc50_auto() (bioassay):
# obtain data -> optional prefit check -> fit -> optional plot.
# Nothing is written to disk unless plot_file is supplied.
# ============================================================

#' Analyse Temperature-Dependent Development (Main Function)
#'
#' Non-interactive, fully parameter-driven entry point for the
#' degree-day module, in the same style as
#' \code{\link{lifeTable_analyze}} and \code{\link{lc50_analyze}}. It
#' (1) obtains the long-format data --- user-supplied column vectors
#' (\code{temp = d$T, duration = d$days, group = d$stage}), a whole
#' data frame, or a csv/xlsx file / folder read via
#' \code{\link{gdd_read}} ---, (2) optionally validates the data with
#' \code{\link{gdd_check}} (including the linear-range check),
#' (3) fits the chosen model --- or selects the best model per group
#' by AICc with \code{model = "auto"} --- via \code{\link{gdd_calc}}
#' and (4) optionally draws the fitted curves with
#' \code{\link{gdd_plot}} --- in the same style as the
#' \code{\link{lc50_analyze}} entry point of the bioassay module.
#' Nothing is written to disk unless \code{plot_file} is supplied;
#' tabular export is handled separately by \code{\link{gdd_export}}.
#'
#' @param temp,duration,group User-supplied column vectors, e.g.
#'   \code{temp = d$T, duration = d$days, group = d$stage} after
#'   \code{d <- read.csv("XXX.csv")}. This is the recommended entry
#'   when many data sets live in one file. \code{group} is optional
#'   (omit it to fit the overall model). When supplied, these vectors
#'   take precedence over \code{data} and \code{path}.
#' @param data A data.frame in the long format required by
#'   \code{\link{gdd_calc}} (one row per observation, with a
#'   temperature and a duration column). Used when \code{temp} /
#'   \code{duration} are not supplied; takes precedence over
#'   \code{path}.
#' @param path Optional; path to a csv/xlsx file or a folder (batch
#'   mode), read with \code{\link{gdd_read}}. Used only when neither
#'   the column vectors nor \code{data} are supplied.
#' @param temp_col,duration_col Column names; auto-detected by default
#'   (ignored when the column vectors are supplied).
#' @param by Grouping variable(s), e.g. \code{"stage"}; \code{NULL}
#'   fits the overall model (ignored when the \code{group} vector is
#'   supplied, which then serves as the grouping).
#' @param model Single model name or \code{"auto"} (best per group by
#'   AICc); default \code{"linear"}. See \code{\link{gdd_calc}} for the
#'   model list and the minimum number of temperature points per model.
#' @param start Optional named list of starting values for a nonlinear
#'   model, e.g. \code{list(a = 1e-4, T0 = 10, Tm = 35)}.
#' @param conf_level Confidence level, default 0.95.
#' @param min_n Minimum rows per group, default 3.
#' @param maxiter Iteration limit passed to the nonlinear fitter.
#' @param check Logical; whether to validate the data with
#'   \code{\link{gdd_check}} before fitting (default \code{TRUE}). The
#'   result is attached to the returned list; a rate decline at high
#'   temperature triggers a targeted warning.
#' @param encoding,header,temp_from_file,pattern Reading options for
#'   \code{\link{gdd_read}} (only used when \code{path} is supplied).
#' @param plot Logical; whether to draw the fitted curves (default
#'   \code{FALSE}).
#' @param plot_file Optional png path: when supplied together with
#'   \code{plot = TRUE} the figure is written to this file (same
#'   machinery as \code{\link{gdd_export_plot}}); when \code{NULL} the
#'   plot is drawn on the current device (fully customisable afterwards
#'   by calling \code{\link{gdd_plot}} on the returned \code{fit}).
#' @param plot_group,show_C,show_Topt Plot options, see
#'   \code{\link{gdd_plot}}.
#' @param plot_title Custom plot title; \code{NULL} = the automatic
#'   per-group caption (group + fitted statistics). A named vector is
#'   matched per group, e.g. \code{c(Egg = "egg", Pupa = "pupa")}.
#' @param plot_sub Custom subtitle; \code{NULL} keeps the automatic
#'   statistics caption (as subtitle when \code{plot_title} is set).
#' @param plot_xlab,plot_ylab Custom axis labels; \code{NULL} keeps
#'   the defaults of \code{\link{gdd_plot}}.
#' @param plot_family Text font family, see \code{\link{gdd_plot}}
#'   (\code{NULL} keeps the default \code{"serif"} --- Times New Roman
#'   on 'Windows'; Chinese characters are rendered through the device's
#'   font fallback, i.e. SimSun on Chinese 'Windows').
#' @param plot_width,plot_height Physical size of the exported figure
#'   in \code{plot_units} (only used when \code{plot_file} is
#'   supplied). \code{NULL} (default, for both) picks a canvas that
#'   gives the axes a panel with a height:width ratio of about 3:4:
#'   12 x 10 cm for a single-panel figure, 15 x 12.2 cm when several
#'   groups are drawn. Because the size is physical, the composition is
#'   identical at every resolution --- \code{plot_res} only adds pixels.
#' @param plot_units Unit of \code{plot_width} / \code{plot_height}:
#'   \code{"cm"} (default), \code{"in"} or \code{"px"}. Use
#'   \code{"cm"} / \code{"in"} for publication figures. With
#'   \code{"px"} the canvas is a fixed pixel count; the text size is
#'   compensated internally so that changing \code{plot_res} keeps the
#'   300-dpi composition (only the recorded dpi metadata changes).
#' @param plot_res Resolution (dpi) of the exported png, default 300.
#'   Higher values add pixels (sharper print) without changing the
#'   layout or the physical size. E.g. an 8 cm-wide figure at journal
#'   quality: \code{plot_units = "cm", plot_width = 8,
#'   plot_res = 300}.
#' @param ... Further arguments passed to \code{\link{gdd_calc}}
#'   (reserved for future model options; keeps user code forward
#'   compatible).
#' @param export Logical; write the results document to disk?
#'   Default \code{FALSE}.
#' @param export_path Output directory for the results document;
#'   created when missing. \code{NULL} (default) means
#'   \code{\link{getwd}}.
#' @param export_file File name of the results document
#'   (\code{.xlsx} or \code{.csv}); \code{NULL} (default) means
#'   \code{gdd_results.xlsx}. Relative paths are resolved
#'   against \code{export_path}; absolute paths are used as-is. The
#'   parent directory is created when it does not exist. The written
#'   path is returned invisibly
#'   in the \code{export_file} component of the result.
#'
#' @return A list with components:
#'   \item{data}{the long-format data actually analysed}
#'   \item{check}{the \code{\link{gdd_check}} result, or \code{NULL}
#'     when \code{check = FALSE}}
#'   \item{fit}{the \code{"gdd"} object returned by
#'     \code{\link{gdd_calc}} --- \code{fit$results} (summary table),
#'     \code{fit$fits} (per-group details incl. coefficient tables),
#'     \code{fit$comparison} (model comparison, \code{"auto"} mode);
#'     print/summary/plot/predict S3 methods are available}
#'   \item{plot_file}{the png path when \code{plot_file} was supplied,
#'     otherwise \code{NULL}}
#'   \item{export_file}{the results-document path when
#'     \code{export = TRUE}, otherwise \code{NULL}}
#' @seealso \code{\link{gdd_read}}, \code{\link{gdd_check}},
#'   \code{\link{gdd_calc}}, \code{\link{gdd_plot}},
#'   \code{\link{gdd_predict}}, \code{\link{gdd_compare}},
#'   \code{\link{gdd_export}}, \code{\link{gdd_export_plot}},
#'   \code{\link{gdd_daily}}
#' @export
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#'
#' ## --- way 1 (recommended): read the file yourself, pass columns in ---
#' ## Typical when many data sets live in one csv: the user reads the
#' ## file and picks the columns with $, exactly like lifeTable_analyze()
#' d <- read.csv(f)
#' out1 <- gdd_analyze(temp = d$temp, duration = d$duration, group = d$stage)
#' out1$fit$results          # C, K, SE and CI per stage
#' summary(out1$fit)         # detailed coefficient tables
#'
#' ## Without a grouping column: one overall model
#' out1b <- gdd_analyze(temp = d$temp, duration = d$duration)
#'
#' ## --- way 2: pass the whole data frame ---
#' out2 <- gdd_analyze(data = d, by = "stage")
#' gdd_predict(out2$fit, temp = c(20, 25), group = "Egg")
#'
#' ## --- way 3: let the function read the file ---
#' out3 <- gdd_analyze(path = f, by = "stage")
#'
#' ## --- AICc model selection + png export + custom labels ---
#' ## plot_title / plot_xlab / plot_ylab accept custom labels; Chinese
#' ## labels are rendered through the device's font fallback
#' ## export = TRUE additionally writes the results workbook
#' ## (C, K, SE, CI and the model comparison table) as xlsx/csv;
#' ## export_file accepts an absolute path (the parent directory is
#' ## created when missing), so export_path is not needed here
#' \donttest{
#' out4 <- gdd_analyze(temp = d$temp, duration = d$duration, group = d$stage,
#'                     model = "auto", plot = TRUE,
#'                     plot_file = file.path(tempdir(), "gdd.png"),
#'                     export = TRUE,
#'                     export_file = file.path(tempdir(), "gdd_results.xlsx"))
#' out4$fit$comparison       # full comparison table, best flag included
#' out4$plot_file            # path of the written png
#' out4$export_file          # path of the written workbook
#' }
gdd_analyze <- function(temp = NULL, duration = NULL, group = NULL,
                        data = NULL, path = NULL,
                        temp_col = NULL, duration_col = NULL, by = NULL,
                        model = c("linear", "logan", "lactin", "briere1",
                                  "briere2", "wang", "auto"),
                        start = NULL, conf_level = 0.95, min_n = 3,
                        maxiter = 1000, check = TRUE,
                        encoding = "UTF-8", header = TRUE,
                        temp_from_file = FALSE,
                        pattern = "\\.(csv|xlsx|xls)$",
                        plot = FALSE, plot_file = NULL,
                        plot_group = NULL, show_C = TRUE, show_Topt = TRUE,
                        plot_title = NULL, plot_sub = NULL,
                        plot_xlab = NULL, plot_ylab = NULL,
                        plot_family = NULL,
                        plot_width = NULL, plot_height = NULL,
                        plot_units = c("cm", "in", "px"),
                        plot_res = 300,
                        export = FALSE, export_path = NULL,
                        export_file = NULL, ...) {
  call <- match.call()
  model <- match.arg(model)
  plot_units <- match.arg(plot_units)

  ## ---- 1) obtain the long-format data ----
  ## priority: user-supplied column vectors > data frame > path
  if (!is.null(temp) || !is.null(duration)) {
    if (!is.null(data))
      warning("Both the temp/duration vectors and 'data' are supplied; ",
              "the vectors are used and 'data' is ignored.", call. = FALSE)
    if (!is.null(path))
      warning("Both the temp/duration vectors and 'path' are supplied; ",
              "the vectors are used and 'path' is ignored.", call. = FALSE)
    temp     <- suppressWarnings(as.numeric(as.character(temp)))
    duration <- suppressWarnings(as.numeric(as.character(duration)))
    if (!length(temp) || !length(duration))
      stop("temp and duration must be non-empty numeric columns.",
           call. = FALSE)
    if (length(temp) != length(duration))
      stop("temp and duration must have the same length.", call. = FALSE)
    if (!is.null(group)) {
      if (length(group) != length(temp))
        stop("group must have the same length as temp.", call. = FALSE)
      group <- as.character(group)
      data  <- data.frame(temp = temp, duration = duration, group = group,
                          stringsAsFactors = FALSE)
      temp_col <- "temp"; duration_col <- "duration"; by <- "group"
    } else {
      if (!is.null(by))
        warning("'by' is ignored because the group vector was not ",
                "supplied; the overall model is fitted.", call. = FALSE)
      data <- data.frame(temp = temp, duration = duration,
                         stringsAsFactors = FALSE)
      temp_col <- "temp"; duration_col <- "duration"; by <- NULL
    }
  } else if (!is.null(data)) {
    if (!is.null(path))
      warning("Both 'data' and 'path' supplied; 'data' is used and ",
              "'path' is ignored.", call. = FALSE)
    if (!is.data.frame(data)) data <- as.data.frame(data)
  } else if (!is.null(path)) {
    data <- gdd_read(path, encoding = encoding, header = header,
                     temp_from_file = temp_from_file, pattern = pattern)
  } else {
    stop("Supply the data via temp/duration (column vectors), via ",
         "'data' (a data frame) or via 'path' (a file or folder).",
         call. = FALSE)
  }

  ## ---- 2) optional prefit validation (does not stop the analysis) ----
  chk <- if (check) {
    gdd_check(data, temp_col = temp_col, duration_col = duration_col,
              by = by)
  } else NULL
  if (!is.null(chk) && !is.null(chk$linear_check) &&
      model %in% c("linear", "auto")) {
    decl <- chk$linear_check[which(chk$linear_check$rate_declines), ,
                             drop = FALSE]
    if (nrow(decl)) {
      msg <- paste(sprintf("%s (max rate at %g deg C, highest tested %g)",
                           decl$group, decl$temp_of_max_rate,
                           decl$highest_temp), collapse = "; ")
      if (model == "linear")
        warning("Developmental rate declines at high temperature in: ",
                msg, ". The linear model must not be fitted over the ",
                "full range; consider model = 'auto' or a nonlinear ",
                "model, or restrict the data to the linear range.",
                call. = FALSE)
      else
        warning("Rate decline at high temperature in: ", msg,
                ". The linear candidate is expected to fit poorly ",
                "there.", call. = FALSE)
    }
  }

  ## ---- 3) fit (single model, or best per group by AICc) ----
  fit <- gdd_calc(data, temp_col = temp_col, duration_col = duration_col,
                  by = by, model = model, start = start,
                  conf_level = conf_level, min_n = min_n,
                  maxiter = maxiter, ...)

  ## ---- 4) optional plot (current device, or exported as png) ----
  plot_file_out <- NULL
  if (plot) {
    pargs <- list(x = fit, group = plot_group, show_C = show_C,
                  show_Topt = show_Topt, title = plot_title, sub = plot_sub)
    if (!is.null(plot_xlab)) pargs$xlab <- plot_xlab
    if (!is.null(plot_ylab)) pargs$ylab <- plot_ylab
    if (!is.null(plot_family)) pargs$family <- plot_family
    if (is.null(plot_file)) {
      do.call(gdd_plot, pargs)
    } else {
      plot_file_out <- gdd_export_plot(
        fit, file = plot_file, group = plot_group, show_C = show_C,
        show_Topt = show_Topt, title = plot_title, sub = plot_sub,
        xlab = plot_xlab, ylab = plot_ylab, family = plot_family,
        width = plot_width, height = plot_height, units = plot_units,
        res = plot_res)
    }
  }

  export_file_out <- NULL
  if (export) {
    ep <- if (is.null(export_path)) getwd() else export_path
    if (grepl("\\.(csv|xlsx)$", ep, ignore.case = TRUE))
      warning("export_path looks like a file name (ends in .csv or .xlsx); ",
              "it is used as the output FOLDER and the file is written inside ",
              "it - did you mean export_file?", call. = FALSE)
    if (!dir.exists(ep)) dir.create(ep, recursive = TRUE)
    fn <- if (is.null(export_file)) "gdd_results.xlsx" else export_file
    if (!grepl("\\.(csv|xlsx)$", fn, ignore.case = TRUE))
      fn <- paste0(fn, ".xlsx")
    fx <- if (.is_abs_path(fn)) fn else file.path(ep, fn)
    if (!dir.exists(dirname(fx)))
      dir.create(dirname(fx), recursive = TRUE, showWarnings = FALSE)
    export_file_out <- gdd_export(fit,
                                  file = fx,
                                  include_data = FALSE,
                                  include_coefs = FALSE,
                                  include_comparison = TRUE)
  }

  list(data = data, check = chk, fit = fit, plot_file = plot_file_out,
       export_file = export_file_out)
}
