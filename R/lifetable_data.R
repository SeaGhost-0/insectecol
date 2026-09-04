#' Read an Age-Stage, Two-Sex Life Table from a csv File
#'
#' Reads a raw csv file containing the individual daily records of an
#' age-stage, two-sex life table experiment and
#' returns a \code{life_table} object, the central data structure that all
#' other functions of the package work with.
#'
#' @param path Character string; path to the csv file.
#' @param check Logical; if \code{TRUE} (the default) the data are
#'   validated by \code{\link{check_life_table}} immediately after
#'   reading. Set to \code{FALSE} to skip validation.
#'
#' @details The csv file contains one row per individual. If the sex
#'   column is located at column \code{n}, the layout must be:
#' \itemize{
#'   \item column 1: individual ID (header \code{ID}),
#'   \item columns 2 to \code{n - 2}: number of days spent in each
#'     developmental stage (egg, larva, ..., pupa),
#'   \item column \code{n - 1}: adult survival days,
#'   \item column \code{n}: sex, coded \code{F} (female), \code{M} (male)
#'     or \code{N} (died before the adult stage),
#'   \item columns \code{n + 1} onward: daily oviposition of the females
#'     (one column per day; cells after the death of a female stay empty).
#' }
#' The first line of the file must contain the stage names as column
#' headers, including the ID column (\code{ID}) and the sex column
#' (\code{gender}). The sex column is located automatically by scanning the
#' first data row for the markers \code{F}/\code{M}/\code{N}. The file
#' encoding is detected automatically, so UTF-8 and GBK files are both
#' supported.
#'
#' @return A \code{life_table} object; a named list with components
#'   \item{data}{data frame with the raw csv content}
#'   \item{file_name}{file name without extension}
#'   \item{n}{column index of the sex column}
#'   \item{n_1}{\code{n + 1}, the first column of the oviposition data}
#'   \item{n_2}{\code{n - 2}, the column index of the pupal stage}
#'   \item{header}{the first row of the file (stage names), as character}
#'   \item{encoding}{the detected file encoding}
#'   \item{path}{the full path of the csv file}
#'
#' @references
#' Chi, H. and Liu, H. (1985) Two new methods for study of insect
#' population ecology. \emph{Bull. Inst. Zool. Acad. Sin} 24(2), 225-240.
#'
#' Chi, H. (1988) Life-table analysis incorporating both sexes and
#' variable development rates among individuals. \emph{Environmental
#' Entomology} 17(1), 26-34.
#'
#' @seealso \code{\link{check_life_table}} for data validation,
#'   \code{\link{lifeTable_calculate}} for the complete analysis workflow.
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(lt$data[, 1:5])
read_life_table <- function(path, check = TRUE) {
  if (!file.exists(path)) stop("File does not exist: ", path)

  # Detect the file encoding
  enc <- readr::guess_encoding(path)
  encoding_type <- if (nrow(enc) > 0) enc$encoding[1] else "UTF-8"

  # Read the first row (used to extract the stage names)
  header <- read.csv(file = path, header = FALSE, nrows = 1,
                     fileEncoding = encoding_type) %>%
    unlist() %>% as.character()

  data_df <- read.csv(path, header=TRUE, fileEncoding = encoding_type)

  # Locate the sex column
  first_row_char <- as.character(unlist(data_df[1, ]))
  n <- which(first_row_char == "F" | first_row_char == "M" | first_row_char == "N")
  if (length(n) == 0) {
    stop("No sex marker (F/M/N) found in the first data row. Please check that the sex column of the csv file is filled in correctly.")
  }
  n <- n[1]

  lt <- list(
    data      = data_df,
    file_name = tools::file_path_sans_ext(basename(path)),
    n   = n,        # column index of the sex column
    n_1 = n + 1,    # first column of the oviposition data
    n_2 = n - 2,    # column index of the pupal stage
    header = header, encoding = encoding_type, path = path
  )
  class(lt) <- "life_table"
  if (check) check_life_table(lt)
  lt
}

#' Extract the Developmental Stage Names from the Header
#'
#' Extracts the names of the developmental stages from the header (first
#' row) of a life table csv file. The header is truncated at the sex
#' column, the ID column is dropped, and the two adult labels before the
#' sex column are replaced by \code{"Female"} and \code{"Male"}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @details Stage names are taken from the header exactly as they are
#'   spelled in the csv file, so English headers produce English stage
#'   names and Chinese headers produce Chinese stage names. Only the two
#'   adult labels are always converted to \code{"Female"} and
#'   \code{"Male"}. Internally the sex column is located by matching the
#'   header entry \code{gender} and the ID column by the header entry
#'   \code{ID} (both are fixed parts of the csv template).
#'
#' @return A character vector of stage names, e.g.
#'   \code{c("Egg", "Larva", "Pupa", "Female", "Male")}; used as column
#'   names by \code{\link{calc_sxj}} and as legend labels by
#'   \code{\link{plot_sxj}}.
#'
#' @seealso \code{\link{read_life_table}}, \code{\link{calc_sxj}},
#'   \code{\link{plot_sxj}}
#' @export
#' @examples
#' \dontrun{
#' lt <- read_life_table("D:/life_table/cohort.csv")
#' get_stage_names(lt)
#' }
get_stage_names <- function(lt) {
  gp_text <- lt$header
  gp_text <- gp_text[!is.na(gp_text)]           # drop NA entries
  gender_pos <- which(grepl("gender", gp_text))   # sex column header in the csv template
  gp_text <- gp_text[1:gender_pos]              # drop entries after the sex column
  gp_text <- gp_text[gp_text != "ID"]         # drop the ID column
  last <- length(gp_text)
  gp_text[(last - 1):last] <- c("Female", "Male")
  gp_text
}

#' Validate a Life Table Data Set
#'
#' Checks the age data (illegal characters, negative values, illegal gaps)
#' and verifies for every female that the number of her oviposition
#' records matches her recorded survival days. If a problem is found, the
#' function stops and reports the exact row and column positions of all
#' offending cells.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @details Two kinds of problems are detected:
#' \itemize{
#'   \item \strong{age data errors}: cells in the stage-duration and
#'     survival-day columns that contain illegal characters or negative
#'     values, or illegal gaps within a sequence of values;
#'   \item \strong{oviposition errors}: for every female row, the number
#'     of non-empty daily fecundity cells must equal the adult survival
#'     days recorded in column \code{n - 1}.
#' }
#' The check runs automatically inside \code{\link{read_life_table}}
#' unless \code{check = FALSE} is used there.
#'
#' @return \code{invisible(TRUE)} if no error is found; otherwise the
#'   function stops with a detailed error message.
#'
#' @seealso \code{\link{read_life_table}}
#' @export
check_life_table <- function(lt) {
  check <- check_data(lt$data, lt$n)
  if (is.null(check$positions) == FALSE || !is.null(check$oviposition)) {
    err_msg <- ""
    if (is.null(check$positions) == FALSE) {
      detail_lines <- c()
      for (row_idx in seq_len(nrow(check$positions))) {
        r_i <- check$positions$row[row_idx]
        c_i <- check$positions$column[row_idx]
        cell_val <- as.character(lt$data[r_i, c_i])
        num_val <- suppressWarnings(as.numeric(cell_val))
        if (is.na(num_val)) {
          detail_lines <- c(detail_lines,
                            sprintf('  -> row %d, column %d: illegal character "%s"\n', r_i, c_i, cell_val))
        } else if (num_val < 0) {
          detail_lines <- c(detail_lines,
                            sprintf('  -> row %d, column %d: negative value "%s"\n', r_i, c_i, cell_val))
        }
      }
      err_msg <- paste0(err_msg, "[Age data errors]\n", paste(detail_lines, collapse = ""))
    }
    if (!is.null(check$oviposition)) {
      ovi_lines <- c()
      for (ovi_row in check$oviposition) {
        days_data <- lt$data[ovi_row, (lt$n + 1):ncol(lt$data)]
        actual_days <- sum(!is.na(days_data))
        expected_days <- as.numeric(lt$data[ovi_row, lt$n - 1])
        ovi_lines <- c(ovi_lines,
                       sprintf("  -> row %d: number of oviposition records (%d) does not match the survival days (%d)\n",
                               ovi_row, actual_days, expected_days))
      }
      err_msg <- paste0(err_msg, "[Oviposition data errors]\n", paste(ovi_lines, collapse = ""))
    }
    stop(err_msg)
  }
  invisible(TRUE)
}
