# ============================================================
# insectecol --- Emergence-period module: data reading
# Reuses the file machinery of the degree-day module (delimiter
# auto-detection, xlsx via readxl, folder batch mode).
# ============================================================

#' Read a Stage-Structure Survey from a File or a Folder
#'
#' Reads one field survey of the population stage structure for
#' \code{\link{emergence_calc}}: one row per stage, with a stage
#' name column, a count (or percent) column and a days-to-eclosion
#' column. The input may be a single csv file (delimiter
#' auto-detected), a single xlsx/xls file (via the 'readxl' package)
#' or a folder containing csv/xlsx files (batch mode; the files are
#' combined and the source file name kept in a \code{source_file}
#' column).
#'
#' @param path Path to a csv/xlsx file or to a folder (batch mode).
#' @param encoding Text encoding of csv files, default \code{"UTF-8"}.
#'   Use \code{"GBK"} for csv files saved from Chinese 'Excel' on 'Windows'.
#' @param header Logical; whether the file(s) contain a header row.
#'   Default TRUE.
#' @param pattern Regular expression selecting the files in batch mode;
#'   default: csv / xlsx / xls.
#' @return A data.frame (single file), or the combined data.frame with
#'   an extra \code{source_file} column (folder input).
#' @seealso \code{\link{emergence_calc}},
#'   \code{\link{emergence_analyze}}
#' @examples
#' f <- system.file("extdata", "emergence_example.csv",
#'                  package = "insectecol")
#' d <- emergence_read(f)
#' head(d)
#' @export
emergence_read <- function(path, encoding = "UTF-8", header = TRUE,
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
        d <- gdd_read_any(f, encoding, header)   # shared file machinery
        if (!is.null(first_names) && !identical(names(d), first_names))
          warning("File '", basename(f), "' has different column name(s) ",
                  "than the first file; the combined rows may not align.",
                  call. = FALSE)
        first_names <- names(d)
        d$source_file <- basename(f)
        combined <- rbind(combined, d)
      }
      message("Batch mode: read ", length(files), " file(s) from: ", path)
      combined
    },
    "csv file"   = ,
    "other file" = gdd_read_any(path, encoding, header),
    stop("Invalid path (folder/file not found): ", path, call. = FALSE)
  )

  emergence_note_cols(out)
  out
}

## Internal: report the detected columns
emergence_note_cols <- function(df) {
  hit <- try(emergence_detect_cols(df), silent = TRUE)
  if (inherits(hit, "try-error")) {
    message("Read ", nrow(df), " rows; could not auto-detect the ",
            "stage / count (percent) / days columns --- please specify ",
            "them via stage_col / count_col / percent_col / days_col ",
            "in emergence_calc().")
  } else {
    message("Read ", nrow(df), " rows; detected: stage column = '",
            hit$stage, "', ",
            if (is.null(hit$count)) paste0("percent column = '",
                                           hit$percent, "'")
            else paste0("count column = '", hit$count, "'"),
            ", days column = '", hit$days, "'.")
  }
  invisible(NULL)
}

## Internal: auto-detect the stage / count / percent / days columns.
## Chinese aliases are written as \u escapes to keep this file pure
## ASCII (R CMD check requirement).
emergence_detect_cols <- function(data, stage_col = NULL,
                                  count_col = NULL, percent_col = NULL,
                                  days_col = NULL) {
  nm <- names(data)
  pick <- function(user, aliases, regex, label, none_ok = FALSE) {
    if (!is.null(user)) {
      if (!(user %in% nm))
        stop(label, " column \"", user, "\" not found.", call. = FALSE)
      return(user)
    }
    hit <- nm[nm %in% aliases]
    if (!length(hit)) hit <- grep(regex, nm, ignore.case = TRUE,
                                  value = TRUE)
    if (!length(hit)) {
      if (none_ok) return(NULL)
      stop("No ", label, " column found; please specify it explicitly.",
           call. = FALSE)
    }
    if (length(hit) > 1)
      stop("Multiple candidate ", label, " columns detected: ",
           paste(hit, collapse = ", "),
           "; please specify one explicitly.", call. = FALSE)
    hit
  }
  stage <- pick(stage_col,
                c("stage", "grade", "instar", "\u866b\u6001",
                  "\u53d1\u80b2\u9636\u6bb5", "\u866b\u6001/\u7ea7\u522b"),
                "stage|grade|instar|\u866b\u6001|\u9636\u6bb5",
                "stage")
  days <- pick(days_col,
               c("days", "days_to_eclosion", "t",
                 "\u5929\u6570", "\u5386\u671f",
                 "\u8ddd\u7fbd\u5316\u5929\u6570",
                 "\u7fbd\u5316\u5929\u6570"),
               "^days?$|days_to|^t$|\u5929\u6570|\u5386\u671f|\u7fbd\u5316",
               "days")
  count <- pick(count_col,
                c("count", "n", "number", "\u6570\u91cf",
                  "\u5934\u6570", "\u866b\u6570"),
                "count|^n$|number|\u6570\u91cf|\u5934\u6570|\u866b\u6570",
                "count", none_ok = TRUE)
  percent <- pick(percent_col,
                  c("percent", "pct", "proportion", "prop",
                    "\u5360\u6bd4", "\u767e\u5206\u7387",
                    "\u767e\u5206\u6bd4"),
                  "percent|pct|prop|\u5360\u6bd4|\u767e\u5206",
                  "percent", none_ok = TRUE)
  if (is.null(count) && is.null(percent))
    stop("Neither a count nor a percent column was found; supply one ",
         "of them explicitly.", call. = FALSE)
  if (!is.null(count) && !is.null(percent)) {
    message("Both a count and a percent column detected; the count ",
            "column '", count, "' is used.")
    percent <- NULL
  }
  list(stage = stage, count = count, percent = percent, days = days)
}
