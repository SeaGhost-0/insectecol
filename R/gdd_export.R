# ============================================================
# insectecol --- Degree-day module: export results
# Main results always; the model-comparison table ("auto" mode),
# per-group coefficient tables and the cleaned data are optional.
# ============================================================

#' Save Degree-Day Analysis Results
#'
#' Saves the analysis results as CSV (UTF-8) or xlsx. The summary
#' table (\code{results}, one row per group) is always written; the
#' model-comparison table (available when \code{model = "auto"} was
#' used), per-group coefficient tables with confidence intervals, and
#' the cleaned data can be included optionally.
#'
#' @param x A \code{"gdd"} object returned by [gdd_calc()].
#' @param file Output path; the format is chosen by the extension
#'   (.csv / .xlsx).
#' @param include_data Logical; whether to also write the cleaned
#'   data. Default FALSE.
#' @param include_coefs Logical; whether to write the per-group
#'   coefficient tables (estimate, SE, t, p, CI). Default FALSE.
#' @param include_comparison Logical; whether to write the model
#'   comparison table when it exists (only \code{"auto"} mode).
#'   Default TRUE.
#' @param ... Further arguments passed to \code{write.csv} (CSV mode).
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' fit <- gdd_calc(gdd_read(f), by = "stage")
#' gdd_export(fit, tempfile(fileext = ".csv"))
#' @export
gdd_export <- function(x, file = "gdd_results.csv",
                       include_data = FALSE, include_coefs = FALSE,
                       include_comparison = TRUE, ...) {
  if (!inherits(x, "gdd"))
    stop("x must be a 'gdd' object returned by gdd_calc().", call. = FALSE)
  ext  <- tolower(tools::file_ext(file))
  base <- sub("\\.[^.]*$", "", file)   # file path without the extension

  coefs <- if (include_coefs) {
    do.call(rbind, lapply(names(x$fits), function(g) {
      ct <- x$fits[[g]]$coef_table
      data.frame(group = g, ct, row.names = NULL,
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  } else NULL
  cmp <- if (include_comparison) x$comparison else NULL

  if (ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE))
      stop("Package 'writexl' is required to write xlsx files. ",
           "Install it via install.packages('writexl').", call. = FALSE)
    sheets <- list(results = x$results)
    if (!is.null(cmp))    sheets$comparison   <- cmp
    if (!is.null(coefs))  sheets$coefficients <- coefs
    if (include_data)     sheets$data         <- x$data
    writexl::write_xlsx(sheets, file)
  } else {
    if (ext != "csv")
      warning("Unrecognized file extension; writing as CSV (UTF-8).",
              call. = FALSE)
    utils::write.csv(x$results, file, row.names = FALSE,
                     fileEncoding = "UTF-8", ...)
    if (!is.null(cmp))
      utils::write.csv(cmp, paste0(base, "_comparison.csv"),
                       row.names = FALSE, fileEncoding = "UTF-8")
    if (!is.null(coefs))
      utils::write.csv(coefs, paste0(base, "_coefficients.csv"),
                       row.names = FALSE, fileEncoding = "UTF-8")
    if (include_data)
      utils::write.csv(x$data, paste0(base, "_data.csv"),
                       row.names = FALSE, fileEncoding = "UTF-8")
  }
  message("Results saved to: ", normalizePath(file))
  invisible(file)
}

#' Export the Degree-Day Plot
#'
#' Draws the degree-day figure of a \code{"gdd"} object on a file
#' device ('ragg' when available, otherwise the matching
#' \code{\link[grDevices]{grDevices}} device) and writes it to disk -
#' the standalone counterpart of \code{plot_file =} in
#' \code{\link{gdd_analyze}}, usable on an existing fit at any
#' time. All plot options of \code{\link{gdd_plot}} are supported.
#'
#' @param x A \code{"gdd"} object returned by [gdd_calc()] or
#'   [gdd_analyze()].
#' @param file Output path; the format follows the file extension
#'   (png, tiff and jpeg are supported; any other extension is
#'   written as png).
#' @param group,show_C,show_Topt,title,sub,xlab,ylab,family Plot
#'   options, see \code{\link{gdd_plot}}; \code{NULL} (default) keeps
#'   the function defaults.
#' @param width,height,units,res Physical size and resolution of the
#'   figure. \code{NULL} (default, for both) picks a canvas that gives the
#'   axes a panel with a height:width ratio of about 3:4: 12 x 10 cm for
#'   a single-panel figure, 15 x 12.2 cm when several groups are drawn
#'   (wider so that the statistics caption fits at a larger font). The
#'   composition is identical at every resolution, \code{res} only adds
#'   pixels (same semantics as in \code{\link{gdd_analyze}}).
#' @param ... Further arguments passed to \code{\link{gdd_plot}}.
#' @return Invisibly, \code{file}.
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' fit <- gdd_calc(gdd_read(f), by = "stage")
#' gdd_export_plot(fit, tempfile(fileext = ".png"),
#'                 title = "Developmental rate vs temperature")
#' @export
gdd_export_plot <- function(x, file = "gdd_plot.png", group = NULL,
                            show_C = TRUE, show_Topt = TRUE,
                            title = NULL, sub = NULL,
                            xlab = NULL, ylab = NULL, family = NULL,
                            width = NULL, height = NULL,
                            units = c("cm", "in", "px"), res = 300, ...) {
  if (!inherits(x, "gdd"))
    stop("x must be a 'gdd' object returned by gdd_calc().", call. = FALSE)
  units <- match.arg(units)
  ## default canvas: the panels (the axes region) come out at a
  ## height:width ratio of about 3:4 --- 12 x 10 cm for a single-panel
  ## figure, 15 x 12.2 cm when several groups are drawn (wider panels let
  ## the statistics caption fit at a larger font)
  if (is.null(width) || is.null(height)) {
    ng_ <- length(if (is.null(group)) unique(as.character(x$data$group))
                  else as.character(group))
    wcm <- if (ng_ > 1) 15 else 12
    hcm <- if (ng_ > 1) 12.2 else 10
    if (is.null(width))
      width <- switch(units, cm = wcm, `in` = wcm / 2.54,
                      px = wcm / 2.54 * res)
    if (is.null(height))
      height <- switch(units, cm = hcm, `in` = hcm / 2.54,
                       px = hcm / 2.54 * res)
  }
  pargs <- list(x = x, group = group, show_C = show_C,
                show_Topt = show_Topt, title = title, sub = sub, ...)
  if (!is.null(xlab))   pargs$xlab   <- xlab
  if (!is.null(ylab))   pargs$ylab   <- ylab
  if (!is.null(family)) pargs$family <- family
  ## text sizes scale with res on a fixed-pixel canvas; compensate for
  ## units = "px" so that res keeps the 300-dpi composition
  pps <- if (units == "px") 12 * 300 / res else 12
  ## the device follows the file extension: png/tiff/jpeg are written
  ## by 'ragg' (per-glyph font fallback) when available, otherwise by
  ## the matching grDevices device; any other extension falls back to
  ## the png device (previous behaviour)
  ext <- tolower(tools::file_ext(file))
  dev <- pkg_fallback_device(ext)
  if (is.null(dev))
    dev <- switch(ext,
                  tiff = , tif  = grDevices::tiff,
                  jpeg = , jpg  = grDevices::jpeg,
                  grDevices::png)
  dev(file, width = width, height = height, units = units,
      res = res, pointsize = pps)
  tryCatch(do.call(gdd_plot, pargs),
           finally = while (!is.null(grDevices::dev.list()))
             grDevices::dev.off())
  message("Plot saved to: ", normalizePath(file))
  invisible(file)
}