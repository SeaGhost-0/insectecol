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
