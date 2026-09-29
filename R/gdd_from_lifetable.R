# ============================================================
# insectecol --- Degree-day module: bridge from raw life-table files
# Reuses the existing package utilities:
#   check_path_type() / clean_path() --- path handling (batch input)
#   check_data()                    --- raw-data validation
# ============================================================

#' Build Degree-Day Input from Raw Life-Table Files
#'
#' Converts individual-level life-table files (the wide layout validated
#' by \code{check_data()}) into the long-format data frame required
#' by \code{\link{gdd_calc}}: for every input file, the mean (or median)
#' duration of each stage is computed over the individuals that entered
#' that stage (the partial durations of individuals that died in the
#' stage are counted; trailing blanks of individuals that died earlier
#' are ignored), giving one row per stage and temperature.
#'
#' @details Expected raw layout (as checked by \code{check_data}): column
#' 1 = individual ID, columns 2 .. n-1 = stage durations (the last of
#' them, column n-1, is the adult survival time), column n = sex, the
#' columns after n = daily oviposition records. One file per constant
#' temperature is expected; the temperature is taken from the file name
#' (\code{temp_from_file = TRUE}, e.g. "25.csv") or from a \code{temp}
#' column inside the files. Rows flagged invalid by \code{check_data()}
#' can be excluded from the aggregation. Reported oviposition/survival
#' mismatches do not affect stage durations and are only warned about.
#' Stage means are computed over non-NA entries, i.e. over the
#' individuals that entered the stage (including those that died in it).
#'
#' @param path Folder containing one file per temperature (batch mode)
#'   or a single file.
#' @param n Column index of the sex column, as in \code{check_data()}.
#' @param stage_names Character vector of stage names for the duration
#'   columns 2 .. n-1 (length n-2). If NULL, generic names "stage_1",
#'   "stage_2", ... are used.
#' @param stages Optional character vector: subset of \code{stage_names}
#'   to keep in the output (e.g. drop the adult column).
#' @param temp_from_file Logical, default TRUE: the temperature is the
#'   numeric file name (extension stripped). If FALSE, a \code{temp}
#'   column must exist inside every file.
#' @param agg Aggregation function for the durations, default
#'   \code{mean}; \code{median} is a robust alternative.
#' @param exclude_invalid Logical, default TRUE: rows flagged by
#'   \code{check_data()} are excluded from the aggregation.
#' @param encoding,header,pattern Reading options, see \code{\link{gdd_read}}.
#' @return A data.frame with columns \code{stage}, \code{temp},
#'   \code{duration} (and \code{source_file}), ready for
#'   \code{gdd_calc(data, by = "stage")}.
#' @examples
#' \dontrun{
#' df <- gdd_from_lifetable("D:/exp/rearing", n = 6,
#'        stage_names = c("egg", "larva", "pupa", "adult"),
#'        stages = c("egg", "larva", "pupa"))
#' fit <- gdd_calc(df, by = "stage")
#' }
#' @seealso \code{check_data()}, \code{\link{check_path_type}},
#'   \code{\link{gdd_read}}, \code{\link{gdd_calc}}
#' @export
gdd_from_lifetable <- function(path, n, stage_names = NULL, stages = NULL,
                               temp_from_file = TRUE, agg = mean,
                               exclude_invalid = TRUE, encoding = "UTF-8",
                               header = TRUE,
                               pattern = "\\.(csv|xlsx|xls)$") {
  if (!is.function(agg))
    stop("agg must be a function (e.g. mean or median).", call. = FALSE)
  if (length(n) != 1 || is.na(n) || n < 4 || n != floor(n))
    stop("n must be a single integer >= 4 (sex column index).",
         call. = FALSE)

  path  <- clean_path(path)
  ptype <- check_path_type(path)
  files <- switch(ptype,
    "folder"     = list.files(path, pattern = pattern,
                               ignore.case = TRUE, full.names = TRUE),
    "csv file"   = ,
    "other file" = path,
    stop("Invalid path (folder/file not found): ", path, call. = FALSE))
  if (!length(files))
    stop("No csv/xlsx files found in: ", path, call. = FALSE)

  if (is.null(stage_names)) {
    stage_names <- paste0("stage_", seq_len(n - 2))
    message("stage_names not supplied: using generic names (",
            paste(stage_names, collapse = ", "),
            "); pass real stage names for a meaningful output.")
  }
  stage_names <- as.character(stage_names)
  if (length(stage_names) != n - 2)
    stop("stage_names must have length n - 2 = ", n - 2,
         " (one name per duration column 2 .. n-1).", call. = FALSE)

  out <- NULL
  for (f in files) {
    d <- gdd_read_any(f, encoding, header)
    if (ncol(d) < n)
      stop("File '", basename(f), "' has ", ncol(d), " columns; ",
           "n = ", n, " does not fit this file.", call. = FALSE)

    # --- validation via the existing check_data() ---
    chk <- tryCatch(check_data(d, n),
                    error = function(e)
                      stop("check_data() failed on file '", basename(f),
                           "': ", conditionMessage(e), call. = FALSE))
    if (any(!chk$valid)) {
      bad_rows <- which(!chk$valid)
      if (exclude_invalid)
        warning("File '", basename(f), "': excluding ", length(bad_rows),
                " invalid row(s) flagged by check_data(): ",
                paste(bad_rows, collapse = ", "), call. = FALSE)
      else
        warning("File '", basename(f), "': ", length(bad_rows),
                " invalid row(s) flagged by check_data() are kept ",
                "(exclude_invalid = FALSE).", call. = FALSE)
    }
    if (length(chk$oviposition))
      warning("File '", basename(f), "': oviposition records do not ",
              "match the adult survival time in row(s) ",
              paste(chk$oviposition, collapse = ", "),
              " (does not affect stage durations).", call. = FALSE)

    # --- temperature of this file ---
    temp <- if (temp_from_file) {
      tv <- suppressWarnings(
        as.numeric(tools::file_path_sans_ext(basename(f))))
      if (is.na(tv))
        stop("temp_from_file = TRUE but the file name is not a number: ",
             basename(f), call. = FALSE)
      tv
    } else {
      if (!"temp" %in% names(d))
        stop("File '", basename(f), "' has no 'temp' column and ",
             "temp_from_file = FALSE.", call. = FALSE)
      suppressWarnings(as.numeric(as.character(d$temp)))
    }
    if (length(unique(temp)) != 1L)
      stop("File '", basename(f), "': the temperature must be a single ",
           "value (one file per temperature).", call. = FALSE)

    # --- aggregate the duration of each stage column (2 .. n-1) ---
    keep <- if (exclude_invalid) chk$valid else rep(TRUE, nrow(d))
    keep_idx <- which(keep)
    for (j in seq_len(n - 2) + 1L) {
      v <- suppressWarnings(as.numeric(as.character(d[keep_idx, j])))
      v <- v[is.finite(v) & v > 0]        # completed individuals only
      if (!length(v)) next
      out <- rbind(out, data.frame(stage = stage_names[j - 1L],
                                   temp = temp[1], duration = agg(v),
                                   source_file = basename(f),
                                   stringsAsFactors = FALSE))
    }
  }

  if (is.null(out))
    stop("No stage durations could be extracted from the input.",
         call. = FALSE)
  if (!is.null(stages)) {
    stages <- as.character(stages)
    unknown <- setdiff(stages, unique(out$stage))
    if (length(unknown))
      stop("stages contains unknown stage name(s): ",
           paste(unknown, collapse = ", "), call. = FALSE)
    out <- out[out$stage %in% stages, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}