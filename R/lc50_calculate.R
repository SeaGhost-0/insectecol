#' Batch Calculation of LC Values
#'
#' Computes the LC estimate for every data set read by
#' \code{\link{read_lc50}}, using the selected estimation method, and
#' returns both the detailed per-file results and a summary data frame.
#'
#' @param lcd The named list returned by \code{\link{read_lc50}}.
#' @param lc Numeric; the lethal proportion for which the concentration
#'   is estimated. The default 0.5 gives the LC50, 0.9 the LC90.
#' @param method Character string; the estimation method to use:
#'   \code{"traditional"} (traditional linear regression, the default),
#'   \code{"improved"} (improved linear regression) or \code{"probit"}
#'   (probit analysis), case-insensitive. The full English method name
#'   is accepted as well. Only one method is computed per call; to use
#'   a different method pass it explicitly, e.g.
#'   \code{method = "probit"}.
#'
#' @details The selected method is applied to every data set. The
#'   default is the traditional linear regression; use e.g.
#'   \code{method = "probit"} to compute a different single method.
#'   Only one method can be selected per call. A file that fails
#'   (e.g. because too few valid concentrations remain after the
#'   Abbott correction) does not interrupt the batch: the error
#'   message is recorded in the \code{Equation} column of the summary
#'   data frame instead.
#'
#' @return A list with elements
#'   \item{results}{nested list: one element per file, each holding the
#'     result list of the selected method (or the error message)}
#'   \item{summary_df}{data frame with one row per file: estimate, 95%
#'     confidence interval, regression parameters and goodness-of-fit}
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
lc50_calculate <- function(lcd, lc = 0.5, method = "traditional") {
  all_methods <- c(traditional = "Traditional linear regression",
                   improved = "Improved linear regression",
                   probit = "Probit analysis")
  keys <- lc50_pick_methods(method, all_methods)
  cat(sprintf("Method: %s\n", all_methods[keys]))

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
    r <- one[[keys]]
    cat(sprintf("[%d/%d] %s: %s\n", i, length(lcd), nm,
                if (is.null(r$estimate)) paste("FAILED -", r$error)
                else "success"))
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

# Internal: parse the user-supplied method into one standard method key
lc50_pick_methods <- function(method, all_methods) {
  default <- "traditional"
  if (is.null(method)) return(default)
  if (!is.character(method)) stop("method must be a character string or NULL")
  if (length(method) == 0)
    stop("method is empty; please select one method: traditional/improved/probit")
  if (length(method) > 1)
    stop("Only one method can be computed at a time; please pass a single method, e.g. method = \"probit\"")
  if (tolower(method) == "all")
    stop("Computing all methods at once is not supported; please pass a single method, e.g. method = \"probit\"")
  hit <- names(all_methods)[tolower(names(all_methods)) == tolower(method)]
  if (length(hit) == 0) hit <- names(all_methods)[tolower(all_methods) == tolower(method)]
  if (length(hit) == 0) {
    stop("Unknown method \"", method, "\"; available options: ",
         paste(names(all_methods), collapse = "/"),
         " or the full method names")
  }
  hit
}
