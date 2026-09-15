#' Batch Calculation of LC Values
#'
#' Computes the LC estimates for every data set read by
#' \code{\link{read_lc50}}, using the selected estimation method(s), and
#' returns both the detailed per-file results and a summary data frame.
#'
#' @param lcd The named list returned by \code{\link{read_lc50}}.
#' @param lc Numeric; the lethal proportion for which the concentration
#'   is estimated. The default 0.5 gives the LC50, 0.9 the LC90.
#' @param method Character string or vector; the estimation method(s) to
#'   use: one or several of \code{"traditional"} (traditional linear
#'   regression, the default), \code{"improved"} (improved linear
#'   regression) and \code{"probit"} (probit analysis),
#'   case-insensitive, or \code{"all"} for all three in a single call.
#'   The full English method names are accepted as well.
#'
#' @details The selected method(s) are applied to every data set; the
#'   default is the traditional linear regression. The summary data
#'   frame gets one row per file and method, and the progress log
#'   reports the status of every method for every file. A file that
#'   fails (e.g. because too few valid concentrations remain after the
#'   Abbott correction) does not interrupt the batch: the error
#'   message is recorded in the \code{Equation} column of the summary
#'   data frame instead.
#'
#' @return A list with elements
#'   \item{results}{nested list: one element per file, each holding one
#'     element per selected method with its result list (or the error
#'     message)}
#'   \item{summary_df}{data frame with one row per file and method:
#'     estimate, 95% confidence interval, regression parameters and
#'     goodness-of-fit}
#'   \item{lc}{the lethal proportion used}
#'
#' @references
#' Finney, D. J. (1971) \emph{Probit Analysis}, 3rd edition. Cambridge
#' University Press, Cambridge.
#'
#' @seealso \code{\link{read_lc50}}, \code{\link{lc50_traditional}},
#'   \code{\link{lc50_improved}}, \code{\link{lc50_probit}},
#'   \code{\link{plot_lc50}}, \code{\link{save_lc50}}
#' @export
#' @examples
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' res <- lc50_calculate(read_lc50(f), lc = 0.7)     # LC70
#' res$summary_df
#' lc50_calculate(read_lc50(f), method = "all")$summary_df   # all methods
lc50_calculate <- function(lcd, lc = 0.5, method = "traditional") {
  all_methods <- c(traditional = "Traditional linear regression",
                   improved = "Improved linear regression",
                   probit = "Probit analysis")
  keys <- lc50_pick_methods(method, all_methods)
  message(sprintf("Method: %s", paste(all_methods[keys], collapse = ", ")))

  results <- list()
  i <- 0
  for (nm in names(lcd)) {
    i <- i + 1
    one <- list()
    for (key in keys) {
      one[[key]] <- tryCatch(
        switch(key,
               traditional = lc50_traditional(lcd[[nm]], lc),
               improved = lc50_improved(lcd[[nm]], lc),
               probit = lc50_probit(lcd[[nm]], lc)),
        error = function(e) list(method = all_methods[key],
                                 error = conditionMessage(e))
      )
    }
    results[[nm]] <- one
    # status of every method; single-method calls keep the old log format
    stat <- vapply(one, function(r)
      if (is.null(r$estimate)) paste("FAILED -", r$error) else "success",
      character(1))
    message(sprintf("[%d/%d] %s: %s", i, length(lcd), nm,
                if (length(stat) == 1) stat
                else paste(sprintf("%s: %s", names(one), stat),
                           collapse = "; ")))
  }

  rows <- list()
  for (nm in names(results)) {
    for (key in names(results[[nm]])) {
      r <- results[[nm]][[key]]
      if (is.null(r$estimate)) {
        rows[[paste(nm, key)]] <- data.frame(
          "File" = nm, "Method" = all_methods[key], "LC" = lc,
          "Estimate" = NA_real_, "95%CI_lower" = NA_real_, "95%CI_upper" = NA_real_,
          "Slope_b" = NA_real_, "Slope_SE" = NA_real_,
          "Equation" = r$error, "R2" = NA_real_,
          "Chi_square" = NA_real_, "P_value" = NA_real_,
          "Valid_groups" = NA_integer_, "Dropped_groups" = NA_integer_,
          check.names = FALSE
        )
      } else {
        rows[[paste(nm, key)]] <- data.frame(
          "File" = nm, "Method" = r$method, "LC" = r$lc,
          "Estimate" = signif(r$estimate, 4),
          "95%CI_lower" = signif(r$lower, 4),
          "95%CI_upper" = signif(r$upper, 4),
          "Slope_b" = signif(r$slope, 4),
          "Slope_SE" = signif(r$se_slope, 4),
          "Equation" = r$equation,
          "R2" = round(r$r2, 4),
          "Chi_square" = round(r$chisq, 3),
          "P_value" = signif(r$p_chi, 3),
          "Valid_groups" = r$n_groups, "Dropped_groups" = r$dropped,
          check.names = FALSE
        )
      }
    }
  }
  summary_df <- do.call(rbind, rows)
  rownames(summary_df) <- NULL
  list(results = results, summary_df = summary_df, lc = lc)
}

# Internal: parse the user-supplied method into standard method keys;
# accepts one method, a vector of methods, or "all"
lc50_pick_methods <- function(method, all_methods) {
  if (is.null(method)) return("traditional")
  if (!is.character(method) || length(method) == 0)
    stop("method must be \"traditional\", \"improved\", \"probit\", \"all\" ",
         "or a character vector of them")
  if (length(method) == 1 && tolower(method) == "all")
    return(names(all_methods))
  keys <- unique(unlist(lapply(method, function(m) {
    hit <- names(all_methods)[tolower(names(all_methods)) == tolower(m)]
    if (length(hit) == 0)
      hit <- names(all_methods)[tolower(all_methods) == tolower(m)]
    hit
  })))
  if (length(keys) == 0)
    stop("Unknown method \"", paste(method, collapse = ", "),
         "\"; available options: ", paste(names(all_methods), collapse = "/"),
         ", \"all\", or the full method names")
  keys
}
