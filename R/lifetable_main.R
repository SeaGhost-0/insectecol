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
#' \code{\link{lifeTable_read}}) from individual column vectors already
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
#'   \code{\link{lifeTable_check}} (default \code{TRUE}).
#'
#' @return A \code{life_table} object, ready for all \code{calc_*},
#'   \code{lifeTable_plot} and \code{lifeTable_export} functions.
#' @export
#' @examples
#' ## The raw example data shipped with the package
#' f <- system.file("extdata", "lifetable_example.csv", package = "insectecol")
#' ## ^^ change to the actual package name
#' d <- read.csv(f)
#'
#' ## 1) Standard build: column-range subset of stage columns + adult days
#' ##    + sex + oviposition columns (positional indexing is robust to
#' ##    the space-containing headers like "1st instar")
#' lt1 <- lifeTable_build(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17], file_name = "Example")
#' names(lt1)     # components of the life_table object
#' head(lt1$df)   # wide table: ID + stages + Adult + gender + oviposition
#'
#' ## 2) Survival analysis only: omit oviposition entirely. Legal since
#' ##    the data checker skips the oviposition check when the table ends
#' ##    at the sex column (use fecundity = FALSE in the analysis).
#' lt2 <- lifeTable_build(d[2:8], adult_days = d$Adult, sex = d$gender)
#'
#' ## 3) Named list: the list names become the stage names
#' lt3 <- lifeTable_build(list(Egg = d[[2]], "1st instar" = d[[3]],
#'                              "2nd instar" = d[[4]], "3rd instar" = d[[5]],
#'                              "4th instar" = d[[6]], Prepupa = d[[7]],
#'                              Pupa = d[[8]]),
#'                         adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17])
#'
#' ## 4) Friendly stage names via stage_names: exactly one per IMMATURE
#' ##    stage. The adult labels "Female" and "Male" are appended
#' ##    automatically and must NOT be included.
#' lt4 <- lifeTable_build(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17],
#'                         stage_names = c("Egg", "L1", "L2", "L3", "L4",
#'                                         "Prepupa", "Pupa"))
#'
#' ## 5) A common mistake, handled gracefully: stage_names wrongly
#' ##    including the adult labels. The extra two entries are dropped
#' ##    with a warning (only a WARNING - the build still succeeds).
#' lt5 <- lifeTable_build(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17],
#'                         stage_names = c("Egg", "L1", "L2", "L3", "L4",
#'                                         "Prepupa", "Pupa",
#'                                         "Female", "Male"))
#'
#' ## 6) Skip the consistency check (e.g. oviposition columns
#' ##    deliberately shorter than the adult life span)
#' lt6 <- lifeTable_build(d[2:8], adult_days = d$Adult, sex = d$gender,
#'                         oviposition = d[, 11:17], check = FALSE)
lifeTable_build <- function(stages, adult_days, sex, oviposition = NULL,
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
  if (check) lifeTable_check(lt)
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
#' with customisable title, axis titles and legend labels, written to
#' disk as png (to the working directory when \code{plot_file} is not
#' given). Tabular
#' export is handled separately by \code{\link{lifeTable_export}}.
#'
#' @param lt Optional; an existing \code{life_table} object (from
#'   \code{\link{lifeTable_read}} or \code{\link{lifeTable_build}}).
#'   If \code{NULL} (default), the object is built from \code{stages},
#'   \code{adult_days}, \code{sex} and \code{oviposition}.
#' @param stages,adult_days,sex,oviposition,stage_names,check
#'   Passed to \code{\link{lifeTable_build}} (ignored when \code{lt} is
#'   supplied).
#' @param file_name Character; data set name, used as the default plot
#'   title and as the base name of the exported png and xlsx.
#'   \code{NULL} (default) keeps the name stored in the life table
#'   object (for a csv input, the file name without extension); an
#'   explicit value overrides it in every input mode.
#' @param fecundity Logical; whether to compute the reproduction-related
#'   parameters (F, F_xj, m_x, R0, r, lambda, T). \code{FALSE} skips them
#'   entirely - \code{oviposition} is then not required at all and may
#'   be left \code{NULL}.
#' @param bootstrap Logical; whether to estimate the standard errors and
#'   percentile confidence intervals of all scalar parameters with the
#'   bootstrap technique of 'TWOSEX-MSChart' via
#'   \code{\link{lifeTable_bootstrap}} (default \code{FALSE}). The
#'   result is attached as \code{results$boot} and is exported by
#'   \code{\link{lifeTable_export}} as an extra worksheet.
#' @param B Integer; number of bootstrap replicates (only used when
#'   \code{bootstrap = TRUE}). The 'TWOSEX-MSChart' standard is
#'   \code{100000} (the default).
#' @param seed Integer; seed of the bootstrap random number generator
#'   (only used when \code{bootstrap = TRUE}); \code{NULL} uses the
#'   current R session state.
#' @param plot Logical; whether to draw the age-stage survival curves
#'   (default \code{FALSE}). The returned ggplot object can be printed,
#'   customised further or passed to \code{\link{lifeTable_export}}.
#' @param plot_file Optional png path, used with \code{plot = TRUE}. A
#'   path with an extension is the file itself; a path without one is a
#'   folder, created when missing, and the figure is written inside it;
#'   \code{NULL} (default) writes the figure to the working directory
#'   under \code{<file_name>_plot.png}. The path written is returned as
#'   \code{plot_file}.
#' @param plot_width,plot_height,plot_units,plot_res Physical size and
#'   resolution of the exported png (only used when \code{plot_file} is
#'   supplied); defaults 12 x 8 cm at 300 dpi.
#' @param title Character; plot title. \code{NULL} = \code{file_name}.
#' @param x_title,y_title Character or plotmath expression; axis
#'   titles. Defaults \code{"Age(days)"} and
#'   \code{expression("Age-Stage Survival Rate ("*italic(S)[italic(xj)]*")")}
#'   (S with an italic xj subscript).
#' @param legend_labels Character vector; legend labels, one per stage
#'   (immature stages + Female + Male), e.g.
#'   \code{c("Egg", "1st instar", "Pupa", "Female", "Male")}.
#'   \code{NULL} (default) = the stage names of the data
#'   (\code{Egg, 1st instar, 2nd instar, ..., Female, Male}).
#' @param dpi Numeric; resolution used for scaling the text of the plot
#'   (default 300).
#' @param export Logical; write the results workbook to disk?
#'   Default \code{FALSE}.
#' @param export_path Output directory for the results workbook;
#'   created when missing. \code{NULL} (default) means
#'   \code{\link{getwd}}.
#' @param export_file File name of the results workbook;
#'   \code{NULL} (default) means \code{<file_name>_results.xlsx}.
#'   Relative paths are resolved against \code{export_path}; absolute
#'   paths are used as-is. The parent directory is created when it
#'   does not exist. The
#'   written path is returned invisibly in the \code{export_file}
#'   component of the result.
#' @param path Path to a csv file; read with \code{\link{lifeTable_read}}
#'   and used instead of \code{lt}/\code{stages}.
#'
#' @return A list with components \code{lt} (the \code{life_table}
#'   object), \code{results} (the list returned by
#'   \code{\link{lifeTable_calculate_all}}; additionally containing
#'   \code{boot}, the \code{\link{lifeTable_bootstrap}} result, when
#'   \code{bootstrap = TRUE}), \code{plot} (the ggplot object when
#'   \code{plot = TRUE}, otherwise \code{NULL}), \code{plot_file}
#'   (the png path when \code{plot = TRUE}, otherwise
#'   \code{NULL}) and \code{export_file} (the workbook path when
#'   \code{export = TRUE}, otherwise \code{NULL}).
#'
#' @seealso \code{\link{lifeTable_build}},
#'   \code{\link{lifeTable_calculate_all}}, \code{\link{lifeTable_bootstrap}},
#'   \code{\link{lifeTable_plot}}, \code{\link{lifeTable_export}}
#' @export
#' @examples
#' ## The example raw data shipped with the package (the same layout as
#' ## the csv template: ID + immature stage columns + Adult + gender +
#' ## one column per oviposition day of the females)
#' f <- system.file("extdata", "lifetable_example.csv", package = "insectecol")
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
#' out1$results$R0         # net reproductive rate
#'
#' ## --- with bootstrap standard errors (small B for a fast example;
#' ## use the default B = 100000 for publications)
#' out1b <- lifeTable_analyze(stages = d[2:8], adult_days = d$Adult,
#'                            sex = d$gender, oviposition = d[, 11:17],
#'                            file_name = "Example - way 1",
#'                            bootstrap = TRUE, B = 2000, seed = 1)
#' out1b$results$boot$summary
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
#'                          plot_file = file.path(tempdir(), "sxj.png"),
#'                          legend_labels = c("Egg", "L1", "L2", "L3", "L4",
#'                                            "Prepupa", "Pupa",
#'                                            "Female", "Male"))
#' invisible(out3$plot)     # print or further customise the ggplot object
#' out3$plot_file          # the png that was written
#'
#' ## --- way 4: export the figure (png) and the results workbook (xlsx) ---
#' ## plot_file writes the survival-curve png; export = TRUE writes the
#' ## workbook with all result tables (Summary, s_xj, l_x, e_x, e_xj, ...).
#' ## export_file accepts an absolute path (the parent directory is
#' ## created when missing); a relative path would be resolved against
#' ## export_path (default getwd()). Writing the workbook takes well over
#' ## 5 seconds, so this last part is not run by default.
#' \donttest{
#' out4 <- lifeTable_analyze(stages = d[2:8], adult_days = d$Adult,
#'                           sex = d$gender, oviposition = d[, 11:17],
#'                           file_name = "Example - export",
#'                           plot = TRUE,
#'                           plot_file = file.path(tempdir(), "sxj.png"),
#'                           export = TRUE,
#'                           export_file = file.path(tempdir(),
#'                                                   "life_table_results.xlsx"))
#' out4$plot_file        # path of the written png
#' out4$export_file      # path of the written workbook (all result tables)
#' }
lifeTable_analyze <- function(lt = NULL, path = NULL, stages = NULL,
                              adult_days = NULL,
                              sex = NULL, oviposition = NULL, stage_names = NULL,
                              file_name = NULL, check = TRUE,
                              fecundity = TRUE, bootstrap = FALSE,
                              B = 100000, seed = NULL, plot = FALSE, title = NULL,
                              x_title = "Age(days)",
                              y_title = expression("Age-Stage Survival Rate ("*italic(S)[italic(xj)]*")"),
                              legend_labels = NULL, dpi = 300,
                              plot_file = NULL, plot_width = 12,
                              plot_height = 8, plot_units = "cm",
                              plot_res = 300,
                              export = FALSE, export_path = NULL,
                              export_file = NULL) {
  ## ---- 1) build the life_table object (or use the supplied one) ----
  if (!is.null(path)) {
    lt <- lifeTable_read(path)
  } else if (is.null(lt)) {
    lt <- lifeTable_build(stages, adult_days, sex, oviposition,
                           stage_names = stage_names,
                           file_name = if (is.null(file_name)) "life_table"
                                       else file_name,
                           check = check)
  }
  ## an explicitly given file_name wins over the name derived from the
  ## input file or stored in a supplied life_table object
  if (!is.null(file_name)) lt$file_name <- file_name

  ## ---- 2) compute all parameters (fecundity-related skippable) ----
  results <- lifeTable_calculate_all(lt, fecundity = fecundity)

  ## ---- 3) optional bootstrap standard errors ----
  if (bootstrap) results$boot <- lifeTable_bootstrap(lt, B = B, seed = seed)

  ## ---- 4) optional plot (current device or exported as png) ----
  p <- if (plot) lifeTable_plot(lt, results$sxj, title = title, x_title = x_title,
                          y_title = y_title, legend_labels = legend_labels,
                          dpi = dpi) else NULL
  plot_file_out <- NULL
  if (plot && !is.null(p)) {
    ## no plot_file: the working directory, under <file_name>_plot.png;
    ## a plot_file without an extension is a folder (created when
    ## missing), one with an extension is the file itself - ggsave()
    ## would stop on a folder path
    pf <- pkg_plot_path(plot_file, sprintf("%s_plot.png", lt$file_name))
    ## the device follows the file extension: bitmaps are written by
    ## 'ragg' (per-glyph fallback for the mixed Latin/Chinese labels),
    ## vector formats by the classic device with 'showtext' switched on;
    ## a plain ggsave() would land on the platform png device and look
    ## the families up in its PostScript font database, which fails on
    ## systems without Times New Roman (Linux servers)
    ext <- tolower(tools::file_ext(pf))
    if (!ext %in% c("png", "tiff", "tif", "jpeg", "jpg",
                    "pdf", "eps", "ps", "svg")) ext <- "png"
    pkg_ggsave(pf, p, device = ext, width = plot_width,
               height = plot_height, units = plot_units,
               dpi = plot_res, bg = "white")
    plot_file_out <- pf
    message("Plot saved to: ", normalizePath(pf))
  }

  export_file_out <- NULL
  if (export) {
    ep <- if (is.null(export_path)) getwd() else export_path
    if (grepl("\\.(csv|xlsx)$", ep, ignore.case = TRUE))
      warning("export_path looks like a file name (ends in .csv or .xlsx); ",
              "it is used as the output FOLDER and the file is written inside ",
              "it - did you mean export_file?", call. = FALSE)
    fn <- if (is.null(export_file)) sprintf("%s_results.xlsx", lt$file_name)
          else export_file
    export_file_out <- lifeTable_export(lt, results, output_path = ep,
                                        filename = fn)
  }

  list(lt = lt, results = results, plot = p, plot_file = plot_file_out,
       export_file = export_file_out)
}
