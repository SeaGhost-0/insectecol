#' Export LC Results to Excel
#'
#' Writes the results of \code{\link{lc50_calculate}} into a multi-sheet
#' Excel workbook: a summary sheet with the LC estimates, confidence
#' intervals and regression parameters of all files and methods, plus
#' one detail sheet per file with the preprocessed data and the
#' parameters of the successfully computed methods.
#'
#' @param results The result list returned by
#'   \code{\link{lc50_calculate}}.
#' @param output_dir Character string; the output folder. If \code{NULL}
#'   (the default), a folder selection dialog is opened.
#' @param filename Character string; the name of the output file
#'   (default \code{"LC50_results.xlsx"}).
#'
#' @details The detail sheet of each file contains the preprocessed data
#'   (concentration, tested and dead insects, raw and corrected
#'   mortality, log concentration and probit) followed by a parameter
#'   block of every method that succeeded for that file. Files for
#'   which no method succeeded get an empty sheet.
#'
#' @return The full path of the exported xlsx file (invisibly).
#'
#' @seealso \code{\link{lc50_calculate}}, \code{\link{plot_lc50}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' save_lc50(lc50_calculate(read_lc50(f)), output_dir = tempdir())
save_lc50 <- function(results, output_dir = NULL, filename = "LC50_results.xlsx") {
  if (is.null(output_dir)) {
    output_dir <- utils::choose.dir()
    if (is.na(output_dir)) stop("No output folder selected")
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Summary")
  openxlsx::writeData(wb, "Summary", results$summary_df)
  for (nm in names(results$results)) {
    sheet <- substr(gsub("[\\/:*?\"<>|]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    ok <- Filter(function(x) !is.null(x$estimate), results$results[[nm]])
    if (length(ok) == 0) next
    openxlsx::writeData(wb, sheet, ok[[1]]$prep, rowNames = FALSE)
    param <- do.call(rbind, lapply(ok, function(r) data.frame(
      "Method" = r$method,
      "Equation" = r$equation,
      "R2" = round(r$r2, 4),
      "Chi_square" = round(r$chisq, 3),
      "P_value" = signif(r$p_chi, 3),
      "LC_estimate" = signif(r$estimate, 4),
      "95%CI_lower" = signif(r$lower, 4),
      "95%CI_upper" = signif(r$upper, 4),
      check.names = FALSE
    )))
    openxlsx::writeData(wb, sheet, param, startRow = nrow(ok[[1]]$prep) + 3)
  }
  out <- file.path(output_dir, filename)
  openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
  invisible(out)
}
