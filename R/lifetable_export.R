#' Save the Results of One Life Table Analysis
#'
#' Writes all results of one data set into a multi-sheet 'Excel' workbook
#' (\code{<file name>_out.xlsx}): the population parameters, the
#' age-stage survival rates, the age-specific rates and, optionally, the
#' survival curve plot.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{lifeTable_read}}.
#' @param results The result list returned by
#'   \code{\link{lifeTable_calculate_all}}.
#' @param output_path Character; folder the workbook is written to.
#'   Defaults to the current working directory.
#' @param plot A ggplot object (usually from \code{\link{lifeTable_plot}}); if
#'   \code{NULL} (default) no image is exported.
#' @param keep_tiff Logical; whether to keep the standalone tiff file
#'   next to the workbook in addition to the copy embedded in it. Default
#'   \code{FALSE}, i.e. the tiff is deleted after being embedded.
#' @param dpi Numeric; resolution of the exported image (default 300).
#'
#' @details The tiff is written through \code{\link{lifeTable_plot}}'s
#'   device handling, which writes bitmaps with 'ragg' so that Chinese
#'   labels fall back to the system CJK font per glyph while digits and
#'   symbols stay in Times New Roman. The figure therefore looks the same
#'   in every R session, no matter what 'showtext' settings are left over
#'   in the session. If the reproduction-related parameters were skipped
#'   (\code{fecundity = FALSE} in \code{\link{lifeTable_calculate_all}}),
#'   the corresponding values in the Summary sheet are \code{NA} and the
#'   sheets "Female fecundity (F_xj)" and "Age-specific fecundity (m_x)"
#'   are omitted. If bootstrap results are attached
#'   (\code{results$boot <- lifeTable_bootstrap(lt)}; done automatically
#'   by \code{\link{lifeTable_analyze}} and
#'   \code{\link{lifeTable_calculate}} when \code{bootstrap = TRUE}), an
#'   extra worksheet "Bootstrap (SE & CI)" with the parameter table
#'   (original estimate, bootstrap mean, standard error, percentile
#'   confidence interval) is written.
#'
#' @param keep_tiff Keep the standalone tiff file next to the workbook
#'   when a plot is embedded (default \code{FALSE}, i.e. the tiff is
#'   deleted after being inserted).
#' @param dpi Resolution of the embedded plot image.
#' @param filename File name of the workbook; \code{NULL} (default)
#'   means \code{<file_name>_out.xlsx}. A missing \code{.xlsx}
#'   extension is added automatically.
#' @return The path of the exported xlsx file (invisibly).
#'
#' @seealso \code{\link{lifeTable_calculate}}, \code{\link{lifeTable_plot}}
#' @export
#' @examples
#' \donttest{
#' ## writes an xlsx workbook plus a tiff; that takes a few seconds, so it
#' ## is not run by default
#' f <- system.file("extdata", "lifetable_example.csv", package = "insectecol")
#' lt <- lifeTable_read(f)
#' results <- lifeTable_calculate_all(lt)
#' lifeTable_export(lt, results, tempdir())
#' }
lifeTable_export <- function(lt, results, output_path = getwd(), plot = NULL,
                         keep_tiff = FALSE, dpi = 300, filename = NULL) {
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  if (is.null(filename)) filename <- sprintf("%s_out.xlsx", lt$file_name)
  else if (!grepl("\\.xlsx$", filename, ignore.case = TRUE))
    filename <- paste0(filename, ".xlsx")
  output_xlsx_path <- if (!is.null(filename) && .is_abs_path(filename))
    filename else file.path(output_path, filename)
  if (!dir.exists(dirname(output_xlsx_path)))
    dir.create(dirname(output_xlsx_path), recursive = TRUE, showWarnings = FALSE)
  has_fec <- !is.null(results$fxj)              # reproduction parameters computed?
  num <- function(x) if (has_fec) x else NA_real_
  result_df <- data.frame(
    File = lt$file_name, Cohort_size_N = results$N,
    Mean_fecundity_F = num(results$F),
    Net_reproductive_rate_R0 = num(results$R0),
    Finite_rate_of_increase_lambda = num(results$lambda),
    Intrinsic_rate_of_increase_r = num(results$r),
    Mean_generation_time_T = num(results$T))
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Summary")
  addWorksheet(wb, sheetName = "Age-stage survival rate (S_xj)")
  addWorksheet(wb, sheetName = "Age-specific survival (l_x)")
  addWorksheet(wb, sheetName = "Life expectancy (e_x)")
  if (!is.null(results$exj))
    addWorksheet(wb, sheetName = "Life expectancy (e_xj)")
  sxj_export <- rbind(results$sxj, 0)
  colnames(sxj_export) <- colnames(results$sxj)
  writeData(wb, sheet = "Summary", x = result_df, startRow = 1)
  writeData(wb, sheet = "Age-stage survival rate (S_xj)", x = sxj_export,
            startRow = 1, startCol = 1, rowNames = TRUE, colNames = TRUE)
  writeData(wb, sheet = "Age-specific survival (l_x)", x = results$lx,
            startRow = 1, startCol = 1)
  writeData(wb, sheet = "Life expectancy (e_x)", x = results$ex,
            startRow = 1, startCol = 1)
  if (!is.null(results$exj))
    writeData(wb, sheet = "Life expectancy (e_xj)",
              x = results$exj, startRow = 1, startCol = 1)
  if (has_fec) {                                # only written when computed
    addWorksheet(wb, sheetName = "Female fecundity (F_xj)")
    addWorksheet(wb, sheetName = "Age-specific fecundity (m_x)")
    writeData(wb, sheet = "Female fecundity (F_xj)", x = results$fxj,
              startRow = 1, startCol = 1)
    writeData(wb, sheet = "Age-specific fecundity (m_x)", x = results$mx,
              startRow = 1, startCol = 1)
  }

  if (!is.null(results$boot)) {                 # bootstrap SE & CI attached
    addWorksheet(wb, sheetName = "Bootstrap (SE & CI)")
    writeData(wb, sheet = "Bootstrap (SE & CI)", x = results$boot$summary,
              startRow = 1, startCol = 1)
  }

  if (!is.null(plot)) {
    output_img_path <- sprintf("%s/%s_out.tiff", output_path, lt$file_name)
    addWorksheet(wb, sheetName = "img")
    lt_ggsave(output_img_path, plot, width = 12, height = 8, dpi = dpi)
    insertImage(wb, sheet = "img", file = output_img_path, startRow = 1,
                startCol = 1, width = 12, height = 8, units = "cm")
    if (keep_tiff == FALSE) unlink(output_img_path)   # delete the standalone tiff unless it should be kept
  }
  saveWorkbook(wb, output_xlsx_path, overwrite = TRUE)
  message("Saved: ", output_xlsx_path)
  invisible(output_xlsx_path)
}

# Save a life table plot, like ggsave(path, plot, device = "tiff",
# width = 12, height = 8, dpi = 300, units = "cm", bg = "white") but with
# the font handling of pkg_ggsave(): bitmaps are written by 'ragg', which
# falls back per glyph so that Chinese labels are drawn with the system
# CJK font while digits and symbols keep Times New Roman, and text sizes,
# which are true typographic points, come out the same at every dpi.
lt_ggsave <- function(filename, plot, width, height, dpi,
                      device = "tiff", units = "cm", bg = "white", ...) {
  pkg_ggsave(filename, plot, device = device, width = width, height = height,
             units = units, dpi = dpi, bg = bg, ...)
}
