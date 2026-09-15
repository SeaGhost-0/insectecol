# ==================== lifetable_main.R ====================
# Non-interactive, parameter-driven API for building an age-stage,
# two-sex life table directly from user-supplied columns of an
# already loaded data frame (e.g. data <- read.csv("XXX.csv")),
# and for running the complete analysis on it.

#' @noRd
.ord_suffix <- function(i) {
  if (i %% 100 %in% 11:13 || i %% 10 == 0) return("th")
  switch(i %% 10, "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
}

#' Default Developmental Stage Names
#'
#' Generates the default stage names used throughout the package:
#' \code{"Egg"} followed by \code{"1st instar"}, \code{"2nd instar"},
#' ... one entry per immature stage. The adult labels
#' \code{"Female"}/\code{"Male"} are appended automatically by
#' \code{\link{get_stage_names}} and need not be included here.
#'
#' @param k Integer; number of immature stages (>= 1).
#'
#' @return A character vector of length \code{k}.
#' @export
#' @examples
#' default_stage_names(4)   # "Egg", "1st instar", "2nd instar", "3rd instar"
default_stage_names <- function(k) {
  k <- as.integer(k)
  if (length(k) != 1 || is.na(k) || k < 1) stop("k must be a single integer >= 1")
  if (k == 1) return("Egg")
  instars <- seq_len(k - 1)
  c("Egg", paste0(instars, vapply(instars, .ord_suffix, character(1)), " instar"))
}

#' Build a Life Table Object from User-Supplied Columns
#'
#' Assembles a \code{life_table} object (the same structure returned by
#' \code{\link{read_life_table}}) from individual column vectors already
#' loaded into the R session, e.g. after
#' \code{data <- read.csv("XXX.csv")}. This is the entry point for
#' analysing data that do not come from a package-conform csv file.
#'
#' @param stages Stage-duration columns, one column per immature stage:
#'   a data frame, a matrix or a list of equal-length vectors (column j =
#'   days spent in immature stage j; blank/NA for stages not reached).
#'   If it is a named data frame/list, the names are used as stage names.
#' @param adult_days Numeric vector; adult survival days (NA for
#'   individuals that died before the adult stage).
#' @param sex Character/factor vector; \code{F}, \code{M} or \code{N}
#'   (died before adult).
#' @param oviposition Optional; daily oviposition records, one column per
#'   day: a data frame, a matrix (rows = individuals in the same order as
#'   \code{sex}) or a single vector (one column). \code{NULL} if the
#'   reproduction-related parameters should not be computed (see
#'   \code{fecundity} in \code{\link{lifeTable_calculate_all}}).
#' @param stage_names Character vector of stage names (length =
#'   number of columns of \code{stages}). \code{NULL} (default) uses the
#'   names of \code{stages} if it has non-empty names, otherwise
#'   \code{\link{default_stage_names}}.
#' @param file_name Character; data set name (default plot title, base
#'   name of the exported xlsx).
#' @param check Logical; validate the data with
#'   \code{\link{check_life_table}} (default \code{TRUE}).
#'
#' @return A \code{life_table} object, ready for all \code{calc_*},
#'   \code{plot_sxj} and \code{save_results} functions.
#' @export
#' @examples
#' ## The raw example data shipped with the package
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' ## ^^ change to the actual package name
#' d <- read.csv(f)
#'
#' ## 1) Standard build: column-range subset of stage columns + adult days
#' ##    + sex + oviposition columns (positional indexing is robust to
#' ##    the space-containing headers like "1st instar")
#' lt1 <- build_life_table(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17], file_name = "Example")
#' names(lt1)     # components of the life_table object
#' head(lt1$df)   # wide table: ID + stages + Adult + gender + oviposition
#'
#' ## 2) Survival analysis only: omit oviposition entirely. Legal since
#' ##    the data checker skips the oviposition check when the table ends
#' ##    at the sex column (use fecundity = FALSE in the analysis).
#' lt2 <- build_life_table(d[2:8], adult_days = d$Adult, sex = d$gender)
#'
#' ## 3) Named list: the list names become the stage names
#' lt3 <- build_life_table(list(Egg = d[[2]], "1st instar" = d[[3]],
#'                              "2nd instar" = d[[4]], "3rd instar" = d[[5]],
#'                              "4th instar" = d[[6]], Prepupa = d[[7]],
#'                              Pupa = d[[8]]),
#'                         adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17])
#'
#' ## 4) Friendly stage names via stage_names: exactly one per IMMATURE
#' ##    stage. The adult labels "Female" and "Male" are appended
#' ##    automatically and must NOT be included.
#' lt4 <- build_life_table(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17],
#'                         stage_names = c("Egg", "L1", "L2", "L3", "L4",
#'                                         "Prepupa", "Pupa"))
#'
#' ## 5) A common mistake, handled gracefully: stage_names wrongly
#' ##    including the adult labels. The extra two entries are dropped
#' ##    with a warning (only a WARNING - the build still succeeds).
#' lt5 <- build_life_table(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17],
#'                         stage_names = c("Egg", "L1", "L2", "L3", "L4",
#'                                         "Prepupa", "Pupa",
#'                                         "Female", "Male"))
#'
#' ## 6) Skip the consistency check (e.g. oviposition columns
#' ##    deliberately shorter than the adult life span)
#' lt6 <- build_life_table(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17], check = FALSE)
build_life_table <- function(stages, adult_days, sex, oviposition = NULL,
                             stage_names = NULL, file_name = "life_table",
                             check = TRUE) {
  ## ---- normalise stages into a list of numeric columns ----
  if (is.data.frame(stages) || is.matrix(stages)) {
    stage_nm <- colnames(stages)
    stages <- lapply(seq_len(ncol(stages)),
                     function(j) suppressWarnings(as.numeric(as.character(stages[, j]))))
  } else if (is.list(stages)) {
    stage_nm <- names(stages)
    stages <- lapply(stages, function(s) suppressWarnings(as.numeric(as.character(s))))
  } else stop("stages must be a data.frame, matrix or list of columns")
  k <- length(stages)
  if (k < 1) stop("stages contains no columns")

  ## ---- stage names: user > column names > default (Egg, 1st instar, ...) ----
  if (is.null(stage_names)) {
    stage_names <- if (!is.null(stage_nm) && length(stage_nm) == k &&
                       all(nzchar(stage_nm))) stage_nm else default_stage_names(k)
  }
  ## user wrongly included the adult labels -> drop them with a warning
  if (length(stage_names) == k + 2) {
    warning(sprintf(
      "stage_names has %d entries; the last two (\"%s\", \"%s\") were dropped - the adult labels Female/Male are appended automatically",
      length(stage_names), stage_names[k + 1], stage_names[k + 2]))
    stage_names <- stage_names[seq_len(k)]
  }
  if (length(stage_names) != k)
    stop(sprintf(paste("length(stage_names) is %d but stages has %d columns.",
                       "Pass exactly one name per IMMATURE stage; the adult labels",
                       "\"Female\" and \"Male\" are appended automatically and must",
                       "NOT be included."),
                 length(stage_names), k))
  ## duplicated labels would break column assignment and the plot;
  if (any(duplicated(stage_names))) {
    warning("stage_names contains duplicated labels; they were made unique (suffixes .1, .2, ...): ",
            paste(stage_names, collapse = ", "))
    stage_names <- make.unique(stage_names)
  }
  ## ---- validate the vectors ----
  sex <- as.character(sex)
  adult_days <- suppressWarnings(as.numeric(as.character(adult_days)))
  N <- length(sex)
  if (N == 0) stop("sex is empty")
  if (anyNA(sex) || !all(sex %in% c("F", "M", "N")))
    stop("sex may only contain F, M or N (no NAs)")
  if (length(adult_days) != N) stop("length(adult_days) must equal length(sex)")
  if (any(lengths(stages) != N)) stop("every stage column must have the same length as sex")

  ## ---- assemble the data frame (identical layout to the csv template) ----
  df <- data.frame(ID = seq_len(N))
  for (i in seq_len(k)) df[, i + 1] <- stages[[i]]      # index-based, duplicate-safe
  names(df)[2:(k + 1)] <- stage_names
  df[["Adult_days"]] <- adult_days
  df[["gender"]] <- sex
  header <- c("ID", stage_names, "Adult_days", "gender")
  n <- k + 3                                    # column index of gender

  ## ---- optional oviposition columns (one per day) ----
  if (!is.null(oviposition)) {
    ov <- if (is.data.frame(oviposition) || is.matrix(oviposition))
      as.data.frame(oviposition)
    else as.data.frame(matrix(suppressWarnings(as.numeric(oviposition)), ncol = 1))
    if (nrow(ov) != N)
      stop(sprintf("oviposition has %d rows but the cohort has %d individuals", nrow(ov), N))
    for (j in seq_len(ncol(ov))) {
      df[[paste0("Day", j)]] <- suppressWarnings(as.numeric(as.character(ov[[j]])))
      header <- c(header, paste0("Day", j))
    }
  }

  lt <- list(data = df, file_name = file_name, n = n, n_1 = n + 1, n_2 = n - 2,
             header = header, encoding = "user input", path = file_name)
  class(lt) <- "life_table"
  if (check) check_life_table(lt)
  lt
}

#' Analyse a Life Table from User-Supplied Columns (Main Function)
#'
#' Non-interactive, fully parameter-driven entry point for the
#' age-stage, two-sex life table analysis. It (1) builds a
#' \code{life_table} object from column vectors of an already loaded
#' data frame (e.g. after \code{data <- read.csv("XXX.csv")}, or accepts
#' a ready \code{life_table} object), (2) computes the life table
#' parameters and (3) optionally draws the age-stage survival curves
#' with customisable title, axis titles and legend labels. Nothing is
#' written to disk; export is handled separately by
#' \code{\link{save_results}}.
#'
#' @param lt Optional; an existing \code{life_table} object (from
#'   \code{\link{read_life_table}} or \code{\link{build_life_table}}).
#'   If \code{NULL} (default), the object is built from \code{stages},
#'   \code{adult_days}, \code{sex} and \code{oviposition}.
#' @param stages,adult_days,sex,oviposition,stage_names,file_name,check
#'   Passed to \code{\link{build_life_table}} (ignored when \code{lt} is
#'   supplied).
#' @param fecundity Logical; whether to compute the reproduction-related
#'   parameters (F, F_xj, m_x, R0, r, lambda, T). \code{FALSE} skips them
#'   entirely - \code{oviposition} is then not required at all and may
#'   be left \code{NULL}.
#' @param plot Logical; whether to draw the age-stage survival curves
#'   (default \code{FALSE}). The returned ggplot object can be printed,
#'   customised further or passed to \code{\link{save_results}}.
#' @param title Character; plot title. \code{NULL} = \code{file_name}.
#' @param x_title,y_title Character; axis titles. Defaults
#'   \code{"Age(days)"} and \code{"Age-Stage Survival Rate(Sxj)"}.
#' @param legend_labels Character vector; legend labels, one per stage
#'   (immature stages + Female + Male), e.g.
#'   \code{c("Egg", "1st instar", "Pupa", "Female", "Male")}.
#'   \code{NULL} (default) = the stage names of the data
#'   (\code{Egg, 1st instar, 2nd instar, ..., Female, Male}).
#' @param dpi Numeric; resolution used for scaling the text of the plot
#'   (default 300).
#'
#' @return A list with components \code{lt} (the \code{life_table}
#'   object), \code{results} (the list returned by
#'   \code{\link{lifeTable_calculate_all}}) and \code{plot} (the ggplot
#'   object when \code{plot = TRUE}, otherwise \code{NULL}).
#'
#' @seealso \code{\link{build_life_table}},
#'   \code{\link{lifeTable_calculate_all}}, \code{\link{plot_sxj}},
#'   \code{\link{save_results}}
#' @export
#' @examples
#' ## The example raw data shipped with the package (the same layout as
#' ## the csv template: ID + immature stage columns + Adult + gender +
#' ## one column per oviposition day of the females)
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' ## ^^ change "lifeTable" to the actual package name
#' d  <- read.csv(f)
#' names(d)   # with check.names = TRUE (default) the names become
#'           # ID, Egg, X1st.instar, X2nd.instar, ..., Prepupa, Pupa,
#'           # Adult, gender, ...
#'
#' ## --- way 1: pass a column-range subset of the data frame
#' ## (positional indexing: works regardless of how the names were mangled)
#' out1 <- lifeTable_analyze(stages = d[2:8], adult_days = d$Adult,
#'                           sex = d$gender, oviposition = d[, 11:17],
#'                           file_name = "Example - way 1")
#' out1$results$N          # number of individuals
#' out1$results$Summary   # all life table parameters
#'
#' ## --- way 2: pass a named list of single columns
#' ## (the list names become the stage names in plots and results)
#' out2 <- lifeTable_analyze(stages = list(Egg = d[[2]], "1st instar" = d[[3]],
#'                                         "2nd instar" = d[[4]], "3rd instar" = d[[5]],
#'                                         "4th instar" = d[[6]], Prepupa = d[[7]],
#'                                         Pupa = d[[8]]),
#'                          adult_days = d$Adult, sex = d$gender,
#'                          fecundity = FALSE)   # survival analysis only,
#'                                               # oviposition not supplied
#' out2$results$N
#'
#' ## --- way 3: select the stage columns by their original names
#' ## (re-read with check.names = FALSE to keep "1st instar", "2nd instar", ...)
#' d3 <- read.csv(f, check.names = FALSE)
#' out3 <- lifeTable_analyze(stages = d3[, c("Egg", "1st instar", "2nd instar",
#'                                           "3rd instar", "4th instar",
#'                                           "Prepupa", "Pupa")],
#'                          adult_days = d3$Adult, sex = d3$gender,
#'                          oviposition = d3[, 11:17],
#'                          stage_names = c("Egg", "L1", "L2", "L3", "L4",
#'                                          "Prepupa", "Pupa"),
#'                          plot = TRUE,
#'                          legend_labels = c("Egg", "L1", "L2", "L3", "L4",
#'                                            "Prepupa", "Pupa",
#'                                            "Female", "Male"))
#' out3$plot               # print or further customise the ggplot object
lifeTable_analyze <- function(lt = NULL, stages = NULL, adult_days = NULL,
                              sex = NULL, oviposition = NULL, stage_names = NULL,
                              file_name = "life_table", check = TRUE,
                              fecundity = TRUE, plot = FALSE, title = NULL,
                              x_title = "Age(days)",
                              y_title = "Age-Stage Survival Rate(Sxj)",
                              legend_labels = NULL, dpi = 300) {
  ## ---- 1) build the life_table object (or use the supplied one) ----
  if (is.null(lt))
    lt <- build_life_table(stages, adult_days, sex, oviposition,
                           stage_names = stage_names, file_name = file_name,
                           check = check)

  ## ---- 2) compute all parameters (fecundity-related skippable) ----
  results <- lifeTable_calculate_all(lt, fecundity = fecundity)

  ## ---- 3) optional plot (no device interaction, no file output) ----
  p <- if (plot) plot_sxj(lt, results$sxj, title = title, x_title = x_title,
                          y_title = y_title, legend_labels = legend_labels,
                          dpi = dpi) else NULL

  list(lt = lt, results = results, plot = p)
}
