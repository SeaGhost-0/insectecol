# ============================================================
# insectecol --- Emergence-period module: export
# Quantile predictions always; the cumulative-development table
# is included by default.
# ============================================================

#' Save Emergence-Period Projection Results
#'
#' Saves the quantile predictions (eclosion dates, and hatch dates
#' when present) as CSV (UTF-8) or xlsx. The cumulative-development
#' table behind the projection is written alongside by default.
#'
#' @param x A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param file Output path; the format is chosen by the extension
#'   (.csv / .xlsx).
#' @param include_stages Logical; whether to also write the
#'   cumulative-development table. Default TRUE.
#' @param ... Further arguments passed to \code{write.csv} (CSV mode).
#' @examples
#' \donttest{
#' f <- system.file("extdata", "emergence_example.csv",
#'                  package = "insectecol")
#' fit <- emergence_calc(emergence_read(f), survey_date = "2026-03-20")
#' emergence_export(fit, tempfile(fileext = ".csv"))
#' }
#' @export
emergence_export <- function(x, file = "emergence_results.csv",
                             include_stages = TRUE, ...) {
  if (!inherits(x, "emergence"))
    stop("x must be an 'emergence' object returned by ",
         "emergence_calc().", call. = FALSE)
  ext  <- tolower(tools::file_ext(file))
  base <- sub("\\.[^.]*$", "", file)   # path without the extension

  if (ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE))
      stop("Package 'writexl' is required to write xlsx files. ",
           "Install it via install.packages('writexl').", call. = FALSE)
    sheets <- list(predictions = x$predictions)
    if (include_stages) sheets$stages <- x$table
    writexl::write_xlsx(sheets, file)
  } else {
    if (ext != "csv")
      warning("Unrecognized file extension; writing as CSV (UTF-8).",
              call. = FALSE)
    utils::write.csv(x$predictions, file, row.names = FALSE,
                     fileEncoding = "UTF-8", ...)
    if (include_stages)
      utils::write.csv(x$table, paste0(base, "_stages.csv"),
                       row.names = FALSE, fileEncoding = "UTF-8")
  }
  message("Results saved to: ", normalizePath(file))
  invisible(file)
}

#' Export the Emergence-Period Plot as PNG
#'
#' Draws the emergence-period projection of an \code{"emergence"}
#' object on a png device ('ragg' when available, otherwise
#' \code{\link[grDevices]{png}}) and writes it to disk - the
#' standalone counterpart of \code{plot_file =} in
#' \code{\link{emergence_analyze}}, usable on an existing fit at any
#' time. All plot options of \code{\link{plot.emergence}} are
#' supported.
#'
#' @param x A \code{"emergence"} object returned by
#'   \code{\link{emergence_calc}} or \code{\link{emergence_analyze}}.
#' @param file Output png path.
#' @param show_hatch,title,sub,xlab,ylab,family Plot options, see
#'   \code{\link{plot.emergence}}; \code{NULL} (default) keeps the
#'   function defaults.
#' @param width,height,units,res Physical size and resolution of the
#'   png; the composition is identical at every resolution,
#'   \code{res} only adds pixels (same semantics as in
#'   \code{\link{emergence_analyze}}).
#' @param ... Further arguments passed to
#'   \code{\link{plot.emergence}}.
#' @return Invisibly, \code{file}.
#' @examples
#' \donttest{
#' f <- system.file("extdata", "emergence_example.csv",
#'                  package = "insectecol")
#' fit <- emergence_calc(emergence_read(f), survey_date = "2026-03-20",
#'                       pre_ovip = 3, egg_days = 10)
#' emergence_export_plot(fit, tempfile(fileext = ".png"))
#' }
#' @export
emergence_export_plot <- function(x, file = "emergence_plot.png",
                                  show_hatch = TRUE,
                                  title = NULL, sub = NULL,
                                  xlab = NULL, ylab = NULL,
                                  family = NULL,
                                  width = 10.67, height = 6,
                                  units = c("in", "cm", "px"),
                                  res = 150, ...) {
  if (!inherits(x, "emergence"))
    stop("x must be an 'emergence' object returned by ",
         "emergence_calc().", call. = FALSE)
  units <- match.arg(units)
  pargs <- list(x = x, show_hatch = show_hatch, title = title,
                sub = sub, ...)
  if (!is.null(xlab))   pargs$xlab   <- xlab
  if (!is.null(ylab))   pargs$ylab   <- ylab
  if (!is.null(family)) pargs$family <- family
  ## text sizes scale with res on a fixed-pixel canvas; compensate for
  ## units = "px" so that res keeps the 150-dpi composition
  pps <- if (units == "px") 12 * 150 / res else 12
  if (requireNamespace("ragg", quietly = TRUE))
    ragg::agg_png(file, width = width, height = height, units = units,
                  res = res, pointsize = pps)
  else
    grDevices::png(file, width = width, height = height, units = units,
                   res = res, pointsize = pps)
  tryCatch(do.call(plot, pargs),
           finally = while (!is.null(grDevices::dev.list()))
             grDevices::dev.off())
  message("Plot saved to: ", normalizePath(file))
  invisible(file)
}
