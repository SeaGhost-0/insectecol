#' Read Bioassay Data for LC Estimation
#'
#' Reads the raw csv files of a dose-response bioassay (one file per
#' insecticide, population or similar) and returns a list of standardised
#' data frames that the LC functions of the package work with.
#'
#' @param path Character string; the data path: a folder (all csv files
#'   inside are read) or a single csv file. If \code{NULL} (the default),
#'   a folder selection dialog is opened.
#'
#' @details Each csv file must contain one row per concentration with
#'   three required columns: the concentration, the number of insects
#'   tested and the number of dead insects. The column names are matched
#'   loosely against the fixed keywords of the csv template, so headers
#'   with additional text such as units (e.g. a concentration header
#'   with "(mg/L)" appended) are recognised as well. Rows with a
#'   concentration of zero are treated as the control group and are used
#'   for the Abbott correction during the analysis.
#'
#'   The data are standardised and validated while reading: rows with
#'   non-numeric or missing entries are dropped, the number of tested
#'   insects must be positive, the number of dead insects must lie
#'   between zero and the number of tested insects, at least one
#'   concentration greater than zero must be present, and the rows are
#'   sorted by increasing concentration. The file encoding is detected
#'   automatically (UTF-8 with BOM and GBK are tried), so files written
#'   by both English and Chinese versions of Excel can be read.
#'
#' @return A named list with one data frame per csv file; the list
#'   elements are named after the files (without extension) and each
#'   data frame has the columns \code{Concentration}, \code{Tested} and
#'   \code{Dead}.
#'
#' @references Abbott, W. S. (1925) A method of computing the
#'   effectiveness of an insecticide. \emph{Journal of Economic
#'   Entomology} 18(2), 265-267.
#'
#' @seealso \code{\link{lc50_calculate}} for the analysis workflow,
#'   \code{\link{check_path_type}} for the path handling.
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' lcd <- read_lc50(f)
#' lcd$bioassay
#' \dontrun{
#' lcd <- read_lc50()                        # interactive folder dialog
#' }
read_lc50 <- function(path = NULL) {
  if (is.null(path)) {
    path <- utils::choose.dir()
    if (is.na(path)) stop("No folder selected")
  }

  path <- clean_path(path)
  ptype <- check_path_type(path)

  if (ptype == "invalid path") {
    stop("Invalid path, please check: ", path)
  }
  if (ptype == "other file") {
    stop("The path must be a folder or a csv file: ", path)
  }

  files <- if (ptype == "folder") {
    fs <- list.files(path, pattern = "\\.csv$",
                     full.names = TRUE, ignore.case = TRUE)
    if (length(fs) == 0) stop("No csv files found in the folder: ", path)
    fs
  } else {
    path
  }

  lcd <- list()
  for (f in files) {
    nm <- tools::file_path_sans_ext(basename(f))
    lcd[[nm]] <- lc50_clean(lc50_read_csv(f), nm)
    cat(sprintf("Read: %s (%d concentration groups)\n", nm, nrow(lcd[[nm]])))
  }
  lcd
}

# Internal: try UTF-8-BOM and GBK encodings
lc50_read_csv <- function(f) {
  for (enc in c("UTF-8-BOM", "GBK")) {
    d <- tryCatch(read.csv(f, check.names = FALSE, fileEncoding = enc),
                  error = function(e) NULL)
    if (!is.null(d) && any(grepl("concentration|Concentration|Conc", names(d)))) return(d)   # anchor: concentration column of the csv template
  }
  stop(sprintf("Cannot read the file (unknown encoding) or the concentration column is missing: %s", basename(f)))
}

# Internal: fuzzy column-name matching + data validation
lc50_clean <- function(raw, nm) {
    nm_conc <- names(raw)[grepl("concentration|Concentration|Conc", names(raw))]               # anchor: csv template header
  nm_dead <- names(raw)[grepl("death|Death|Dead|dead", names(raw))]               # anchor: csv template header
  nm_n <- setdiff(names(raw)[grepl("Number of insects|Number of heads|Tested", names(raw))], nm_dead)  # anchor: csv template header
  if (length(nm_conc) < 1 || length(nm_n) < 1 || length(nm_dead) < 1)
    stop(sprintf("File [%s] is missing the \"Concentration\"/\"Tested\"/\"Dead\" columns", nm))
  d <- data.frame(
    "Concentration" = suppressWarnings(as.numeric(raw[[nm_conc[1]]])),
    "Tested" = suppressWarnings(as.numeric(raw[[nm_n[1]]])),
    "Dead" = suppressWarnings(as.numeric(raw[[nm_dead[1]]])),
    check.names = FALSE
  )
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) == 0) stop(sprintf("File [%s] contains no valid data rows", nm))
  if (any(d[["Tested"]] <= 0)) stop(sprintf("File [%s] contains rows with Tested <= 0", nm))
  bad <- d[["Dead"]] < 0 | d[["Dead"]] > d[["Tested"]]
  if (any(bad))
    stop(sprintf("File [%s], row %d: the number of dead insects is outside [0, Tested]", nm, which(bad)[1]))
  if (!any(d[["Concentration"]] > 0)) stop(sprintf("File [%s] contains no treatment group with Concentration > 0", nm))
  d <- d[order(d[["Concentration"]]), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Internal: Abbott correction, drop 0%/100% groups, probit transformation
lc50_prepare <- function(d) {
  ctrl <- d[["Concentration"]] == 0
  pc <- 0
  if (any(ctrl)) {
    pc <- sum(d[["Dead"]][ctrl]) / sum(d[["Tested"]][ctrl])
    if (pc >= 1) stop("Control mortality is 100%; please check the data")
  }
  d1 <- d[!ctrl, , drop = FALSE]
  p_raw <- d1[["Dead"]] / d1[["Tested"]]
  p <- if (pc > 0) (p_raw - pc) / (1 - pc) else p_raw
  keep <- p > 0 & p < 1
  dropped <- sum(!keep)
  if (dropped > 0) {
    message(sprintf("Dropped %d concentration group(s) with a corrected mortality of 0%% or 100%%", dropped))
  }
  d1 <- d1[keep, , drop = FALSE]
  p_raw <- p_raw[keep]
  p <- p[keep]
  if (length(p) < 3) stop("Fewer than 3 valid concentration groups; the regression cannot be fitted")
  prep <- data.frame(
    "Concentration" = d1[["Concentration"]],
    "Tested" = d1[["Tested"]],
    "Dead" = d1[["Dead"]],
    "Mortality" = round(p_raw, 4),
    "Corrected_mortality" = round(p, 4),
    "log10_concentration" = round(log10(d1[["Concentration"]]), 4),
    "y" = round(stats::qnorm(p) + 5, 4),
    check.names = FALSE
  )
  attr(prep, "p") <- p
  attr(prep, "dropped") <- dropped
  prep
}
