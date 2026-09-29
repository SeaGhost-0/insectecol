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
#' \donttest{
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' fit <- gdd_calc(gdd_read(f), by = "stage")
#' gdd_export(fit, tempfile(fileext = ".csv"))
#' }
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