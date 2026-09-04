#' Save the Results of One Life Table Analysis
#'
#' Writes all results of one data set into a multi-sheet Excel workbook
#' (\code{<file name>_out.xlsx}): the population parameters, the
#' age-stage survival rates, the age-specific rates and, optionally, the
#' survival curve plot.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param results The result list returned by \code{\link{lifeTable_calculate_all}}.
#' @param output_path Character; folder the workbook is written to.
#'   Defaults to the current working directory.
#' @param plot A ggplot object (usually from \code{\link{plot_sxj}}); if
#'   \code{NULL} (default) no image is exported.
#' @param keep_tiff Logical; whether to keep the standalone tiff file
#'   next to the workbook in addition to the copy embedded in it. Default
#'   \code{FALSE}, i.e. the tiff is deleted after being embedded.
#' @param dpi Numeric; resolution of the exported image (default 300).
#'
#' @return The path of the exported xlsx file (invisibly).
#'
#' @seealso \code{\link{lifeTable_calculate}}, \code{\link{plot_sxj}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' results <- lifeTable_calculate_all(lt)
#' save_results(lt, results, tempdir())
save_results <- function(lt, results, output_path = getwd(), plot = NULL,
                         keep_tiff = FALSE, dpi = 300) {
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  output_xlsx_path <- sprintf("%s/%s_out.xlsx", output_path, lt$file_name)

  result_df <- data.frame(
    File = lt$file_name, Cohort_size_N = results$N,
    Mean_fecundity_F = results$F, Net_reproductive_rate_R0 = results$R0,
    Finite_rate_of_increase_lambda = results$lambda,
    Intrinsic_rate_of_increase_r = results$r,
    Mean_generation_time_T = results$T
  )

  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Summary")
  addWorksheet(wb, sheetName = "Age-stage survival rate (S_xj)")
  addWorksheet(wb, sheetName = "Age-specific survival (l_x)")
  addWorksheet(wb, sheetName = "Female fecundity (F_xj)")
  addWorksheet(wb, sheetName = "Age-specific fecundity (m_x)")
  addWorksheet(wb, sheetName = "Life expectancy (e_x)")

  sxj_export <- rbind(results$sxj, 0)   # append terminal zero row to S_xj (as in the original program)
  colnames(sxj_export) <- colnames(results$sxj)

  writeData(wb, sheet = "Summary", x = result_df, startRow = 1)
  writeData(wb, sheet = "Age-stage survival rate (S_xj)", x = sxj_export,
            startRow = 1, startCol = 1, rowNames = TRUE, colNames = TRUE)
  writeData(wb, sheet = "Age-specific survival (l_x)", x = results$lx,
            startRow = 1, startCol = 1)
  writeData(wb, sheet = "Female fecundity (F_xj)", x = results$fxj,
            startRow = 1, startCol = 1)
  writeData(wb, sheet = "Age-specific fecundity (m_x)", x = results$mx,
            startRow = 1, startCol = 1)
  writeData(wb, sheet = "Life expectancy (e_x)", x = results$ex,
            startRow = 1, startCol = 1)

  if (!is.null(plot)) {
    output_img_path <- sprintf("%s/%s_out.tiff", output_path, lt$file_name)
    addWorksheet(wb, sheetName = "img")
    ggsave(output_img_path, plot = plot, device = "tiff",
           width = 12, height = 8, dpi = dpi, units = "cm", bg = "white")
    insertImage(wb, sheet = "img", file = output_img_path, startRow = 1,
                startCol = 1, width = 12, height = 8, units = "cm")
   if (keep_tiff == FALSE) unlink(output_img_path)   # delete the standalone tiff unless it should be kept
  }
  saveWorkbook(wb, output_xlsx_path, overwrite = TRUE)
  showtext_auto(enable = FALSE)
  cat("Saved:", output_xlsx_path, "\n")
  invisible(output_xlsx_path)
}
