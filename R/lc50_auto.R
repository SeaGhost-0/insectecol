# Internal: classify the input path (file or folder) and collect the csv
# files to process; stops for anything that is not a csv file or a folder
lc50_auto_files <- function(path) {
  if (is.null(path)) {
    path <- utils::choose.dir()
    if (is.na(path)) stop("No input path selected")
  }
  path <- clean_path(path)
  ptype <- check_path_type(path)
  if (ptype == "invalid path") stop("Invalid path, please check: ", path)
  if (ptype == "other file")
    stop("The path must be a folder or a csv file: ", path)
  if (ptype == "csv file")
    return(list(files = path, folder = FALSE, path = path))
  fs <- list.files(path, pattern = "\\.csv$",
                   full.names = TRUE, ignore.case = TRUE)
  if (length(fs) == 0) stop("No csv files found in the folder: ", path)
  list(files = fs, folder = TRUE, path = path)
}

# Internal: output name stem "<file>[_LCxx][_<method>]<suffix>"
lc50_auto_stem <- function(nm, lc, method, suffix) {
  if (is.null(suffix)) suffix <- ""
  all_methods <- c(traditional = "Traditional linear regression",
                   improved = "Improved linear regression",
                   probit = "Probit analysis")
  keys <- lc50_pick_methods(method, all_methods)
  lc_tag <- if (isTRUE(all.equal(lc, 0.5))) ""
            else sprintf("_LC%d", round(lc * 100))
  m_tag <- if (identical(keys, "traditional")) ""
           else if (identical(keys, names(all_methods))) "_all"
           else paste0("_", paste(keys, collapse = "_"))
  paste0(nm, lc_tag, m_tag, suffix)
}

#' Save the LC Results of Every Data File (One xlsx per csv)
#'
#' Reads the csv file(s) at \code{path}, computes the LC values with
#' \code{\link{lc50_calculate}} and writes one Excel workbook per csv
#' file with \code{\link{save_lc50}} - named after the data file and
#' placed next to the raw data.
#'
#' @param path Character string; a csv file or a folder with csv files,
#'   classified with \code{\link{check_path_type}}. \code{NULL} (the
#'   default) opens a folder selection dialog.
#' @param lc Numeric; the lethal proportion, passed on to
#'   \code{\link{lc50_calculate}} (default 0.5 = LC50).
#' @param method Character string; the estimation method, passed on to
#'   \code{\link{lc50_calculate}} (default \code{"traditional"}).
#' @param suffix Optional character string appended to the output file
#'   names, e.g. \code{"_v2"}; the default \code{NULL} adds nothing.
#'
#' @details Each csv file is processed completely on its own (read,
#'   calculate, export) in its own loop pass, so the outputs of
#'   different files can never mix. \code{LB_48.csv} produces
#'   \code{LB_48.xlsx} in the same folder; for a folder every csv file
#'   inside is processed the same way. A file that cannot be read is
#'   skipped with a message (a single-file input that cannot be read is
#'   an error); a file whose computation fails still gets its workbook
#'   with the error recorded in the summary sheet. Non-default
#'   settings are appended to the names so repeated runs do not
#'   overwrite each other: \code{lc = 0.9} gives
#'   \code{LB_48_LC90.xlsx}, \code{method = "probit"} gives
#'   \code{LB_48_probit.xlsx}.
#'
#' @return The paths of the written xlsx files, invisibly.
#' @seealso \code{\link{save_lc50_plot_auto}} for the matching figure
#'   export, \code{\link{save_lc50}} for a custom output location
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' tmp <- file.path(tempdir(), "bioassay.csv")
#' file.copy(f, tmp, overwrite = TRUE)
#' save_lc50_auto(tmp)                    # -> <tempdir>/bioassay.xlsx
save_lc50_auto <- function(path = NULL, lc = 0.5, method = "traditional",
                          suffix = NULL) {
  info <- lc50_auto_files(path)
  message(sprintf("Input: %s (%s, %d csv file(s))", info$path,
              if (info$folder) "folder" else "file", length(info$files)))
  written <- character(0)
  skipped <- 0L
  for (f in info$files) {
    nm <- tools::file_path_sans_ext(basename(f))
    lcd <- tryCatch(read_lc50(f), error = function(e) e)
    if (inherits(lcd, "error")) {
      if (!info$folder)
        stop("The input file could not be read: ", conditionMessage(lcd))
      message(sprintf("Skipped %s.csv: %s", nm, conditionMessage(lcd)))
      skipped <- skipped + 1L
      next
    }
    # 每个文件独立完成 读取 -> 计算 -> 导出，输出之间不可能串
    res <- lc50_calculate(lcd, lc = lc, method = method)
    out <- save_lc50(res, output_dir = dirname(f),
                     filename = paste0(lc50_auto_stem(nm, lc, method, suffix),
                                       ".xlsx"))
    est <- res$summary_df[["Estimate"]]
    est <- est[!is.na(est)][1]
    if (!is.na(est)) {
      message(sprintf("  %s: LC%d = %s -> %s", nm, round(lc * 100),
                  format(signif(est, 4), trim = TRUE), basename(out)))
    } else {
      message(sprintf("  %s: computation FAILED (see Summary sheet) -> %s",
                  nm, basename(out)))
    }
    written <- c(written, out)
  }
  message(sprintf("Done: %d xlsx file(s)%s in %s", length(written),
              if (skipped > 0) sprintf(", %d csv skipped", skipped) else "",
              dirname(info$files[1])))
  invisible(written)
}

#' Save the LC Figure of Every Data File (One image per csv)
#'
#' Reads the csv file(s) at \code{path}, computes the LC values, draws
#' the regression plot of \code{\link{plot_lc50}} for every file and
#' saves it with \code{\link{save_lc50_plot}} - named after the data
#' file and placed next to the raw data.
#'
#' @param path,lc,method,suffix Same as \code{\link{save_lc50_auto}}.
#' @param device,width,height,dpi,units,bg Figure settings, passed on
#'   to \code{\link{save_lc50_plot}} (defaults \code{"tiff"}, 12 x 8
#'   cm, 600 dpi, white background).
#' @param font,unit,shape,ci,ci_level,error_bar,move_thres,lc_ci,lc_p,lc_lab_gap,lc_lab_gap_right,lc_lab_dy,lc_lab_lh
#'   Plot settings, passed on to \code{\link{plot_lc50}} unchanged.
#' @param preview Logical (default \code{FALSE}); also print every
#'   figure on the screen.
#'
#' @details The pipeline and the file-naming rules are the same as in
#'   \code{\link{save_lc50_auto}}, plus \code{_linear} for
#'   \code{shape = "linear"}: \code{LB_48.csv} gives \code{LB_48.tiff},
#'   \code{lc = 0.9, method = "probit"} gives
#'   \code{LB_48_LC90_probit.tiff}. The two functions are fully
#'   independent: each reads and computes the data on its own, so they
#'   can be called alone or in any order. A file whose computation
#'   fails gets no figure. With \code{method = "all"} the figure shows
#'   the first method that succeeded.
#'
#' @return Invisibly a list with elements \code{plots} (named list of
#'   the ggplot objects, e.g. for \code{print()} or
#'   \code{save_lc50_plot(plots, ...)}) and \code{files} (paths of the
#'   written images).
#' @seealso \code{\link{save_lc50_auto}}, \code{\link{plot_lc50}},
#'   \code{\link{save_lc50_plot}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' tmp <- file.path(tempdir(), "bioassay.csv")
#' file.copy(f, tmp, overwrite = TRUE)
#' save_lc50_plot_auto(tmp)               # -> <tempdir>/bioassay.tiff
save_lc50_plot_auto <- function(path = NULL, lc = 0.5,
                                method = "traditional", suffix = NULL,
                                device = "tiff", dpi = 600, width = 12,
                                height = 8, units = "cm", bg = "white",
                                font = "TNM", unit = NULL,
                                shape = c("sigmoid", "linear"), ci = TRUE,
                                ci_level = 0.95, error_bar = TRUE,
                                move_thres = 0.5, lc_ci = TRUE,
                                lc_p = TRUE, lc_lab_gap = 0.35,
                                lc_lab_gap_right = 0.1,
                                lc_lab_dy = 0.1, lc_lab_lh = 1.05,
                                preview = FALSE) {
  shape <- match.arg(shape)
  info <- lc50_auto_files(path)
  font <- pkg_resolve_font(font)
  ext <- if (is.character(device)) tolower(device) else "tiff"
  img_tag <- if (shape == "linear") "_linear" else ""
  message(sprintf("Input: %s (%s, %d csv file(s))", info$path,
              if (info$folder) "folder" else "file", length(info$files)))

  plots <- list()
  written <- character(0)
  skipped <- 0L
  for (f in info$files) {
    nm <- tools::file_path_sans_ext(basename(f))
    lcd <- tryCatch(read_lc50(f), error = function(e) e)
    if (inherits(lcd, "error")) {
      if (!info$folder)
        stop("The input file could not be read: ", conditionMessage(lcd))
      message(sprintf("Skipped %s.csv: %s", nm, conditionMessage(lcd)))
      skipped <- skipped + 1L
      next
    }
    res <- lc50_calculate(lcd, lc = lc, method = method)
    gp <- lc50_plot_one(nm, res$results[[1]], font, unit, shape = shape,
                        ci = ci, ci_level = ci_level, error_bar = error_bar,
                        move_thres = move_thres, lc_ci = lc_ci,
                        lc_p = lc_p, lc_lab_gap = lc_lab_gap,
                        lc_lab_gap_right = lc_lab_gap_right,
                        lc_lab_dy = lc_lab_dy,
                        lc_lab_lh = lc_lab_lh)
    if (is.null(gp)) {
      message(sprintf("  %s: computation FAILED, figure skipped", nm))
      next
    }
    stem <- lc50_auto_stem(nm, lc, method, suffix)
    attr(gp, "lc50_name") <- paste0(stem, img_tag)
    out <- save_lc50_plot(
      gp, file.path(dirname(f), paste0(stem, img_tag, ".", ext)),
      device = device, width = width, height = height, dpi = dpi,
      units = units, bg = bg)
    est <- res$summary_df[["Estimate"]]
    est <- est[!is.na(est)][1]
    message(sprintf("  %s: LC%d = %s -> %s", nm, round(lc * 100),
                format(signif(est, 4), trim = TRUE), basename(out)))
    plots[[nm]] <- gp
    written <- c(written, out)
    if (preview) {
      showtext::showtext_auto(enable = TRUE)
      print(gp)
      showtext::showtext_auto(enable = FALSE)
    }
  }
  message(sprintf("Done: %d figure(s)%s in %s", length(written),
              if (skipped > 0) sprintf(", %d csv skipped", skipped) else "",
              dirname(info$files[1])))
  invisible(list(plots = plots, files = written))
}
