#' Check the Type of an Input Path
#'
#' Checks whether the input path points to a folder or to a file. Before
#' the check, the path is cleaned automatically: backslashes are
#' converted to forward slashes, invisible characters that are often
#' copied along with paths from Windows dialogs (U+202A) are removed, and
#' a redundant trailing "/" is dropped.
#'
#' @param path Character string; the path to check. Anything that is not
#'   a single, non-missing, non-empty character string is treated as
#'   invalid.
#'
#' @details The function never stops: it always returns one of the four
#'   type labels below, so callers can branch directly on the result.
#'   The cleaned path is used for the existence checks, which makes the
#'   function robust against paths copied out of the Windows Explorer
#'   address bar. It is used internally by the reading functions of the
#'   package to support both folder input (batch mode) and single-file
#'   input.
#'
#' @return A character string, one of
#'   \item{"folder"}{the path exists and is a folder}
#'   \item{"csv file"}{the path exists, is a file and has the extension csv}
#'   \item{"other file"}{the path exists and is a file with another extension}
#'   \item{"invalid path"}{the path does not exist or is not a valid path string}
#'
#' @seealso \code{\link{read_lc50}}
#' @export
#' @examples
#' check_path_type(tempdir())
#' check_path_type(system.file("extdata", "bioassay.csv", package = "insectecol"))
check_path_type <- function(path) {
  # Input protection: non-character, length != 1, NA or empty string -> invalid
  if (!is.character(path) || length(path) != 1 ||
      is.na(path) || trimws(path) == "") {
    return("invalid path")
  }

  # ===== Path cleaning (formerly done in user_choice, now inside the function) =====
  path <- clean_path(path)  # remove invisible characters copied along with the path

  # ===== Type decision (original logic unchanged) =====
  if (dir.exists(path)) {                     # folder path
    return("folder")
  } else if (file.exists(path)) {             # file path
    if (tolower(tools::file_ext(path)) == "csv") {
      return("csv file")
    } else {
      return("other file")
    }
  } else {
    return("invalid path")
  }
}

#' @noRd
# Path cleaning: backslashes to forward slashes, removal of invisible
# characters such as U+202A, dropping of a trailing "/"
clean_path <- function(path) {
  path <- gsub("\\", "/", path, fixed = TRUE)
  path <- gsub("\u202A", "", path, fixed = TRUE)
  if (nchar(path) > 1 && substr(path, nchar(path), nchar(path)) == "/") {
    path <- substr(path, 1, nchar(path) - 1)
  }
  path
}
