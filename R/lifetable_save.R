#' Save the Results of One Life Table Analysis
#'
#' Writes all results of one data set into a multi-sheet Excel workbook
#' (\code{<file name>_out.xlsx}): the population parameters, the
#' age-stage survival rates, the age-specific rates and, optionally, the
#' survival curve plot.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param results The result list returned by
#'   \code{\link{lifeTable_calculate_all}}.
#' @param output_path Character; folder the workbook is written to.
#'   Defaults to the current working directory.
#' @param plot A ggplot object (usually from \code{\link{plot_sxj}}); if
#'   \code{NULL} (default) no image is exported.
#' @param keep_tiff Logical; whether to keep the standalone tiff file
#'   next to the workbook in addition to the copy embedded in it. Default
#'   \code{FALSE}, i.e. the tiff is deleted after being embedded.
#' @param dpi Numeric; resolution of the exported image (default 300).
#'
#' @details The tiff is written through the internal \code{lt_ggsave()},
#'   which enables showtext for the export device and pins showtext's
#'   internal dpi to the value the text sizes of \code{\link{plot_sxj}}
#'   are calibrated for. The exported figure therefore looks the same in
#'   every R session, no matter what showtext settings are left over in
#'   the session. If the reproduction-related parameters were skipped
#'   (\code{fecundity = FALSE} in \code{\link{lifeTable_calculate_all}}),
#'   the corresponding values in the Summary sheet are \code{NA} and the
#'   sheets "Female fecundity (F_xj)" and "Age-specific fecundity (m_x)"
#'   are omitted.
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
  sxj_export <- rbind(results$sxj, 0)
  colnames(sxj_export) <- colnames(results$sxj)
  writeData(wb, sheet = "Summary", x = result_df, startRow = 1)
  writeData(wb, sheet = "Age-stage survival rate (S_xj)", x = sxj_export,
            startRow = 1, startCol = 1, rowNames = TRUE, colNames = TRUE)
  writeData(wb, sheet = "Age-specific survival (l_x)", x = results$lx,
            startRow = 1, startCol = 1)
  writeData(wb, sheet = "Life expectancy (e_x)", x = results$ex,
            startRow = 1, startCol = 1)
  if (has_fec) {                                # only written when computed
    addWorksheet(wb, sheetName = "Female fecundity (F_xj)")
    addWorksheet(wb, sheetName = "Age-specific fecundity (m_x)")
    writeData(wb, sheet = "Female fecundity (F_xj)", x = results$fxj,
              startRow = 1, startCol = 1)
    writeData(wb, sheet = "Age-specific fecundity (m_x)", x = results$mx,
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

# Current internal dpi of showtext (default 96)
lt_showtext_dpi <- function() {
  opts <- tryCatch(showtext::showtext_opts(), error = function(e) NULL)
  if (is.list(opts) && is.numeric(opts$dpi) &&
      length(opts$dpi) == 1 && is.finite(opts$dpi)) opts$dpi else 96
}

# Save a life table plot, like ggsave(path, plot, device = "tiff",
# width = 12, height = 8, dpi = 300, units = "cm", bg = "white") but with
# showtext enabled for the export device.
#
# showtext renders/measures text only at its own fixed internal resolution
# (default 96), ignoring the device dpi. plot_sxj() is calibrated for this:
# its text sizes grow with dpi/300, and the showtext shrink factor of
# 96/dpi cancels that growth, so the exported text keeps the physical size
# it has at 300 dpi - at every dpi and for every device type (vector
# devices have no pixels and are converted at 72 pt/in).
#
# The internal dpi is therefore pinned to 96 * eff / dpi rather than to
# whatever value happens to be active in the session, so the export is
# identical in every new R session (this differs from lc50_ggsave(),
# whose formula is ref * eff / 300, because lc50 plots use fixed font
# sizes while plot_sxj() already scales its fonts with dpi/300; for a
# 300 dpi tiff both formulas give 96). The previous session setting is
# restored on exit (also when ggsave() fails), and showtext is switched
# on only while the file is being written.
lt_ggsave <- function(filename, plot, width, height, dpi,
                      device = "tiff", units = "cm", bg = "white", ...) {
  ref <- lt_showtext_dpi()   # session setting, restored on exit
  eff <- if (is.character(device) &&
               tolower(device) %in% c("pdf", "cairo_pdf", "eps", "ps",
                                      "postscript", "cairo_ps")) 72 else dpi
  showtext::showtext_auto(enable = TRUE)
  on.exit({
    showtext::showtext_auto(enable = FALSE)
    showtext::showtext_opts(dpi = ref)
  }, add = TRUE)
  showtext::showtext_opts(dpi = 96 * eff / dpi)
  ggplot2::ggsave(filename, plot = plot, device = device,
                  width = width, height = height, dpi = dpi,
                  units = units, bg = bg, ...)
}
