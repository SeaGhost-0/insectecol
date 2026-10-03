# ============================================================
# insectecol --- Degree-day module: data reading (long format)
# Path handling is delegated to the existing package utilities
# check_path_type() and clean_path() (R/check_path_type.R) ---
# no duplicated path logic here.
# ============================================================

#' Read Insect Developmental Data from a Folder or a File
#'
#' Reads constant-temperature development data in long format (one row
#' per observation, with a temperature column and a duration column;
#' optional grouping columns such as life stage). The input may be:
#' \itemize{
#'   \item a single csv file (delimiter auto-detected),
#'   \item a single xlsx/xls file (via the 'readxl' package),
#'   \item a folder containing csv/xlsx files (batch mode), e.g. one
#'     file per temperature or per stage; the files are combined and
#'     the source file name is kept in a \code{source_file} column.
#' }
#'
#' @param path Path to a csv/xlsx file or to a folder (batch mode).
#' @param encoding Text encoding of csv files, default \code{"UTF-8"}.
#'   Use \code{"GBK"} for csv files saved from Chinese 'Excel' on 'Windows'.
#' @param header Logical; whether the file(s) contain a header row.
#'   Default TRUE.
#' @param temp_from_file Logical (batch mode only); if TRUE, the file
#'   name (extension stripped) is converted to a number and written to
#'   the \code{temp} column --- convenient when one file per temperature
#'   is named e.g. "25.csv". Default FALSE.
#' @param pattern Regular expression selecting the files in batch mode;
#'   default: csv / xlsx / xls.
#' @return A data.frame (single file), or the combined data.frame with
#'   an extra \code{source_file} column (folder input).
#' @seealso \code{\link{check_path_type}} (path handling),
#'   \code{\link{gdd_check}}, \code{\link{gdd_calc}}
#' @examples
#' # Single csv file shipped with the package (inst/extdata)
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' df <- gdd_read(f)
#' head(df)
#'
#' # Batch mode: one csv per temperature, temperature from the file names
#' d <- system.file("extdata", "gdd_batch", package = "insectecol")
#' df2 <- gdd_read(d, temp_from_file = TRUE)
#' head(df2)
#' @export
gdd_read <- function(path, encoding = "UTF-8", header = TRUE,
                     temp_from_file = FALSE,
                     pattern = "\\.(csv|xlsx|xls)$") {
  path  <- clean_path(path)          # internal utility (R/check_path_type.R)
  ptype <- check_path_type(path)

  out <- switch(ptype,
    "folder" = {
      files <- list.files(path, pattern = pattern,
                          ignore.case = TRUE, full.names = TRUE)
      if (!length(files))
        stop("No csv/xlsx files found in folder: ", path, call. = FALSE)
      combined <- NULL
      first_names <- NULL
      for (f in files) {
        d <- gdd_read_any(f, encoding, header)
        if (!is.null(first_names) && !identical(names(d), first_names))
          warning("File '", basename(f), "' has different column name(s) ",
                  "than the first file; the combined rows may not align.",
                  call. = FALSE)
        first_names <- names(d)
        d$source_file <- basename(f)
        if (temp_from_file) {
          tv <- suppressWarnings(
            as.numeric(tools::file_path_sans_ext(basename(f))))
          if (is.na(tv))
            stop("temp_from_file = TRUE but the file name is not a ",
                 "number: ", basename(f), call. = FALSE)
          d$temp <- tv
        }
        combined <- rbind(combined, d)
      }
      message("Batch mode: read ", length(files), " file(s) from: ", path)
      combined
    },
    "csv file"   = ,
    "other file" = gdd_read_any(path, encoding, header),
    stop("Invalid path (folder/file not found): ", path, call. = FALSE)
  )

  gdd_note_cols(out)
  out
}

## Internal: dispatch one file by extension (csv / xlsx / xls)
gdd_read_any <- function(file, encoding, header) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "csv") {
    gdd_read_one(file, encoding, header)
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read Excel files. ",
           "Install it via install.packages('readxl').", call. = FALSE)
    as.data.frame(readxl::read_excel(file, col_names = header))
  } else {
    stop("Unsupported file type '", ext,
         "': use csv, xlsx, xls or a folder path.", call. = FALSE)
  }
}

## Internal: read one csv file with delimiter auto-detection
gdd_read_one <- function(file, encoding, header) {
  ## "UTF-8-BOM" also reads plain UTF-8 files and additionally strips a
  ## BOM that Windows Excel prepends; otherwise the first column name
  ## keeps a BOM prefix and auto-detection of the columns fails
  enc <- if (identical(toupper(encoding), "UTF-8")) "UTF-8-BOM" else encoding
  line1 <- readLines(file, n = 1, warn = FALSE, encoding = enc)
  cnt <- vapply(c(",", ";", "\t"),
                function(s) sum(gregexpr(s, line1, fixed = TRUE)[[1]] > 0),
                integer(1))
  sep <- names(cnt)[which.max(cnt)]
  args <- list(file = file, header = header, fileEncoding = enc,
               check.names = FALSE, stringsAsFactors = FALSE)
  if (cnt[sep] > 0) args$sep <- sep
  do.call(utils::read.csv, args)
}

## Internal: report which temperature/duration columns were detected
gdd_note_cols <- function(df) {
  hit <- try(gdd_detect_cols(df), silent = TRUE)
  if (inherits(hit, "try-error")) {
    message("Read ", nrow(df), " rows; could not auto-detect the ",
            "temperature/duration columns --- please specify them via ",
            "temp_col / duration_col in gdd_calc().")
  } else {
    message("Read ", nrow(df), " rows; detected: temperature column = '",
            hit$temp, "', duration column = '", hit$duration, "'.")
  }
  invisible(NULL)
}

## Internal: auto-detect the temperature and duration columns
gdd_detect_cols <- function(data, temp_col = NULL, duration_col = NULL) {
  nm <- names(data)
  pick <- function(user, aliases, regex, label) {
    if (!is.null(user)) {
      if (!(user %in% nm))
        stop(label, " column \"", user, "\" not found.", call. = FALSE)
      return(user)
    }
    hit <- nm[nm %in% aliases]
    if (!length(hit)) hit <- grep(regex, nm, ignore.case = TRUE, value = TRUE)
    if (!length(hit))
      stop("No ", label, " column found; please specify it explicitly.",
           call. = FALSE)
    if (length(hit) > 1)
      stop("Multiple candidate ", label, " columns detected: ",
           paste(hit, collapse = ", "),
           "; please specify one explicitly.", call. = FALSE)
    hit
  }
  # English and Chinese aliases are both supported
  # (Chinese written as \u escapes to keep this file pure ASCII)
  list(
    temp     = pick(temp_col,
                    c("temp", "temperature", "T", "\u6e29\u5ea6",
                      "\u9972\u517b\u6e29\u5ea6"),
                    "temp|\u6e29\u5ea6", "temperature"),
    duration = pick(duration_col,
                    c("duration", "days", "D", "\u5386\u671f",
                      "\u53d1\u80b2\u5386\u671f", "\u53d1\u80b2\u5929\u6570"),
                    "dur|days|\u5386\u671f|\u5929\u6570", "duration")
  )
}