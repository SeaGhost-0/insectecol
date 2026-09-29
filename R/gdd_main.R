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
#'   \code{plot = TRUE} the figure is written to this file (ragg when
#'   available, otherwise \code{grDevices::png}); when \code{NULL} the
#'   plot is drawn on the current device (fully customisable afterwards
#'   by calling \code{\link{gdd_plot}} on the returned \code{fit}).
#' @param plot_group,show_C,show_Topt Plot options, see
#'   \code{\link{gdd_plot}}.
#' @param plot_title Custom plot title; \code{NULL} = the automatic
#'   per-group caption (group + fitted statistics). A named vector is
#'   matched per group, e.g. \code{c(Egg = "卵", Pupa = "蛹")}.
#' @param plot_sub Custom subtitle; \code{NULL} keeps the automatic
#'   statistics caption (as subtitle when \code{plot_title} is set).
#' @param plot_xlab,plot_ylab Custom axis labels; \code{NULL} keeps
#'   the defaults of \code{\link{gdd_plot}}.
#' @param plot_family Text font family, see \code{\link{gdd_plot}}
#'   (\code{NULL} keeps the default \code{"serif"} --- Times New Roman
#'   on Windows; Chinese characters are rendered through the device's
#'   font fallback, i.e. SimSun on Chinese Windows).
#' @param plot_width,plot_height Physical size of the exported figure
#'   in \code{plot_units} (only used when \code{plot_file} is
#'   supplied). Defaults \code{10.67} x \code{6} in reproduce the
#'   former 1600 x 900 px canvas at 150 dpi. Because the size is
#'   physical, the composition is identical at every resolution ---
#'   \code{plot_res} only adds pixels.
#' @param plot_units Unit of \code{plot_width} / \code{plot_height}:
#'   \code{"in"} (default), \code{"cm"} or \code{"px"}. Use
#'   \code{"in"} / \code{"cm"} for publication figures. With
#'   \code{"px"} the canvas is a fixed pixel count; the text size is
#'   compensated internally so that changing \code{plot_res} keeps the
#'   150-dpi composition (only the recorded dpi metadata changes).
#' @param plot_res Resolution (dpi) of the exported png, default 150.
#'   Higher values add pixels (sharper print) without changing the
#'   layout or the physical size. E.g. a journal requiring 300 dpi at
#'   8 cm width: \code{plot_units = "cm", plot_width = 8,
#'   plot_res = 300}.
#' @param ... Further arguments passed to \code{\link{gdd_calc}}
#'   (reserved for future model options; keeps user code forward
#'   compatible).
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
#' @seealso \code{\link{gdd_read}}, \code{\link{gdd_check}},
#'   \code{\link{gdd_calc}}, \code{\link{gdd_plot}},
#'   \code{\link{gdd_predict}}, \code{\link{gdd_compare}},
#'   \code{\link{gdd_export}}, \code{\link{gdd_daily}},
#'   \code{\link{gdd_from_lifetable}}
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
#' ## --- AICc model selection + png export + Chinese labels ---
#' \donttest{
#' out4 <- gdd_analyze(temp = d$temp, duration = d$duration, group = d$stage,
#'                     model = "auto", plot = TRUE,
#'                     plot_file = tempfile(fileext = ".png"),
#'                     plot_title = "发育速率与温度的关系",
#'                     plot_xlab = "温度 (°C)", plot_ylab = "发育速率 (1/d)")
#' out4$fit$comparison       # full comparison table, best flag included
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
                        plot_width = 10.67, plot_height = 6,
                        plot_units = c("in", "cm", "px"),
                        plot_res = 150, ...) {
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
      has_ragg <- requireNamespace("ragg", quietly = TRUE)
      ## text sizes scale with res on a fixed-pixel canvas; compensate
      ## for units = "px" so that plot_res keeps the 150-dpi composition
      pps <- if (plot_units == "px") 12 * 150 / plot_res else 12
      if (has_ragg)
        ragg::agg_png(plot_file, width = plot_width,
                      height = plot_height, units = plot_units,
                      res = plot_res, pointsize = pps)
      else
        grDevices::png(plot_file, width = plot_width,
                       height = plot_height, units = plot_units,
                       res = plot_res, pointsize = pps)
      tryCatch(do.call(gdd_plot, pargs),
               finally = while (!is.null(grDevices::dev.list()))
                 grDevices::dev.off())
      plot_file_out <- plot_file
      message("Plot saved to: ", normalizePath(plot_file))
    }
  }

  list(data = data, check = chk, fit = fit, plot_file = plot_file_out)
}
