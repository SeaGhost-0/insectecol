#' Calculate All Parameters of One Life Table
#'
#' Convenience wrapper that computes all life table parameters of one
#' data set in a single call: cohort size, mean fecundity, the age-stage
#' survival rates, the age-specific rates and the derived population
#' parameters (R0, r, lambda, T).
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @details The intermediate results are passed on internally, so nothing
#'   is computed twice: s_xj first, then l_x, F_xj and m_x, then r and
#'   R0, and finally \code{lambda = exp(r)} and \code{T = log(R0) / r}.
#'
#' @return A named list with elements \code{N}, \code{F}, \code{sxj},
#'   \code{lx}, \code{fxj}, \code{mx}, \code{ex}, \code{R0}, \code{r},
#'   \code{lambda} and \code{T}.
#'
#' @seealso The individual \code{calc_*} functions;
#'   \code{\link{lifeTable_calculate}} for the batch workflow.
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' results <- lifeTable_calculate_all(read_life_table(f))
#' results$R0
lifeTable_calculate_all <- function(lt) {
  sxj <- calc_sxj(lt)
  lx  <- calc_lx(lt, sxj)
  fxj <- calc_fxj(lt, sxj)
  mx  <- calc_mx(lt, sxj, fxj, lx)
  r   <- calc_r(lt, lx, mx)
  R0  <- calc_R0(lt, sxj, fxj)
  list(N = calc_N(lt), F = calc_F(lt), sxj = sxj, lx = lx, fxj = fxj,
       mx = mx, ex = calc_ex(lt, lx), R0 = R0, r = r,
       lambda = exp(r), T = log(R0) / r)
}

#' Batch Analysis of Life Table Data
#'
#' Runs the complete workflow (reading, validation, calculation, plotting
#' and exporting) for every csv file in a folder, or for a single csv
#' file. Each data set gets its own Excel workbook; in addition an
#' \code{all.xlsx} with the summary of all files is created. Files that
#' fail (e.g. because of data errors) are skipped and reported at the end
#' without interrupting the remaining files.
#'
#' @param path Character; the data path: a folder (all csv files inside
#'   are analysed) or a single csv file. The type of the path is
#'   determined by \code{check_path_type()}.
#' @param output_path Character; the export folder. Defaults to the
#'   parent folder of the csv file (single-file mode) or the data folder
#'   itself (folder mode).
#' @param plot Logical; whether the age-stage survival curves are
#'   generated and embedded into the workbooks (default \code{TRUE}).
#' @param keep_tiff Logical; whether to keep the standalone tiff files
#'   (default \code{FALSE}).
#' @param dpi Numeric; resolution of the exported images (default 300).
#'
#' @return A summary data frame with one row per successfully analysed
#'   file (population parameters as columns); the attribute
#'   \code{error_files} contains the names of the files that failed.
#'
#' @seealso \code{\link{read_life_table}}, \code{\link{lifeTable_calculate_all}},
#'   \code{\link{plot_sxj}}, \code{\link{save_results}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lifeTable_calculate(f, output_path = file.path(tempdir(), "insectecol-demo"))
#' \dontrun{
#' lifeTable_calculate("D:/life_table/data")                       # whole folder
#' lifeTable_calculate("D:/data", output_path = "D:/results", dpi = 600)
#' }
lifeTable_calculate <- function(path, output_path = NULL, plot = TRUE,
                      keep_tiff = FALSE, dpi = 300) {
  path_type <- check_path_type(path)
  if (path_type == "folder") {
    file_path <- list.files(path, pattern = "\\.(csv)$", full.names = TRUE,
                            ignore.case = TRUE)
  } else if (path_type == "csv file") {
    file_path <- path
  } else stop("The path is neither a csv file nor a folder: ", path)

  number <- length(file_path)
  if (number == 0) stop("No csv files found in the folder")
  if (is.null(output_path)) {
    output_path <- if (path_type == "csv file") dirname(path) else path
  }
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  summary_df <- data.frame(); error_files <- c()
  cat("------------ Calculation started; please wait a few seconds for large data sets ------------\n")
  flush.console()

  for (p in 1:number) {
    tryCatch({
      lt <- read_life_table(file_path[p])                 # read and validate
      results <- lifeTable_calculate_all(lt)                        # all indicators
      plt <- if (plot) plot_sxj(lt, results$sxj, dpi = dpi) else NULL
      save_results(lt, results, output_path, plot = plt,
                   keep_tiff = keep_tiff, dpi = dpi)
      summary_df <- rbind(summary_df, data.frame(
        File = lt$file_name, Cohort_size_N = results$N,
        Mean_fecundity_F = results$F, Net_reproductive_rate_R0 = results$R0,
        Finite_rate_of_increase_lambda = results$lambda,
        Intrinsic_rate_of_increase_r = results$r,
        Mean_generation_time_T = results$T))
      cat(sprintf("[%d/%d] File [%s] completed\n", p, number, lt$file_name))
    }, error = function(e) {
      fname <- tools::file_path_sans_ext(basename(file_path[p]))
      cat(sprintf("File [%d/%d] [%s] failed and was skipped:\n%s\n",
                  p, number, fname, conditionMessage(e)))
      error_files <<- c(error_files, fname)
    })
    while (!is.null(grDevices::dev.list())) grDevices::dev.off()
    gc()
  }

  # Summary workbook all.xlsx
  all_wb <- createWorkbook()
  addWorksheet(all_wb, sheetName = "all")
  writeData(all_wb, sheet = "all", x = summary_df, startRow = 1)
  saveWorkbook(all_wb, sprintf("%s/all.xlsx", output_path), overwrite = TRUE)

  cat(sprintf("\nFinished: total %d | succeeded %d | failed %d\n",
              number, number - length(error_files), length(error_files)))
  if (length(error_files) > 0) cat("Failed files:", paste(error_files, collapse = ", "), "\n")
  attr(summary_df, "error_files") <- error_files
  summary_df
}
