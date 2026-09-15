#' Analyse Bioassay Data for LC Estimation (Main Function)
#'
#' Non-interactive, fully parameter-driven entry point for the LC
#' (lethal concentration) analysis. It (1) assembles the standardised
#' data list from a data frame, a named list of data frames or three
#' parallel vectors, (2) computes the LC estimates with the selected
#' method(s) via \code{\link{lc50_calculate}} and (3) optionally builds
#' the regression plot(s) in the style of \code{\link{plot_lc50}}.
#' Nothing is written to disk and no dialog is opened; export is
#' handled separately by \code{\link{save_lc50}} /
#' \code{\link{save_lc50_plot}}.
#'
#' @param d Optional; the bioassay data: a data frame with the columns
#'   \code{Concentration}, \code{Tested} and \code{Dead} (headers are
#'   matched loosely, as in \code{\link{read_lc50}}, so a header like
#'   \code{"Concentration (mg/L)"} works), a named list of such data
#'   frames (e.g. the return value of \code{\link{read_lc50}}), or
#'   \code{NULL} to build the data from the three vectors below.
#' @param concentration,tested,dead Numeric vectors; the concentration,
#'   the number of insects tested and the number of dead insects, one
#'   entry per concentration group (replicates = repeated values).
#'   Used only when \code{d} is \code{NULL}.
#' @param name Character; the data set name used in the results and the
#'   saved plot file names when \code{d} is a single data frame or the
#'   vectors are used (default \code{"bioassay"}); ignored for a named
#'   list input.
#' @param lc Numeric; the lethal proportion (default 0.5 = LC50,
#'   e.g. 0.9 = LC90), passed to \code{\link{lc50_calculate}}.
#' @param method Character; one or several of \code{"traditional"},
#'   \code{"improved"}, \code{"probit"} or \code{"all"}, passed to
#'   \code{\link{lc50_calculate}}.
#' @param plot Logical; whether to build the regression plot(s)
#'   (default \code{FALSE}). The ggplot objects are only returned -
#'   not printed, not saved.
#' @param plot_method Character; which of the computed methods to plot
#'   (default \code{NULL} = the first method that succeeded).
#'   Ignored when \code{plot = FALSE}.
#' @param font,unit,shape,ci,ci_level,error_bar,move_thres Plot
#'   settings, passed to the internal plot engine exactly as in
#'   \code{\link{plot_lc50}} (\code{unit = NULL} means \code{"mg/L"},
#'   \code{unit = ""} shows no unit).
#'
#' @return A list with elements \code{data} (the standardised data
#'   list, one data frame per data set), \code{results} (the list
#'   returned by \code{\link{lc50_calculate}}: \code{results},
#'   \code{summary_df}, \code{lc}) and \code{plot} (a named list of
#'   ggplot objects when \code{plot = TRUE}, otherwise \code{NULL}).
#'
#' @seealso \code{\link{read_lc50}}, \code{\link{lc50_calculate}},
#'   \code{\link{plot_lc50}}, \code{\link{save_lc50}}
#' @export
#' @examples
#' ## way 1: data frame straight from the package example csv
#' f <- system.file("extdata", "bioassay.csv", package = "insectecol")
#' out1 <- lc50_analyze(read_lc50(f), method = "probit")
#' out1$results$summary_df
#'
#' ## way 2: three parallel vectors, no csv involved; all three methods
#' conc <- c(0, 1.5, 3, 6, 12, 24)
#' n    <- c(120, 60, 60, 60, 60, 60)
#' dead <- c(7, 9, 18, 32, 48, 57)
#' out2 <- lc50_analyze(concentration = conc, tested = n, dead = dead,
#'                      name = "trial1", method = "all")
#' out2$results$summary_df
#'
#' ## way 3: LC90, improved regression, plot on the linear axis
#' out3 <- lc50_analyze(concentration = conc, tested = n, dead = dead,
#'                      name = "trial1", lc = 0.9, method = "improved",
#'                      plot = TRUE, plot_method = "improved",
#'                      shape = "linear")
#' out3$plot$trial1        # ggplot object: print(), customise or export
lc50_analyze <- function(d = NULL, concentration = NULL, tested = NULL,
                         dead = NULL, name = "bioassay", lc = 0.5,
                         method = "traditional", plot = FALSE,
                         plot_method = NULL, font = "TNM", unit = NULL,
                         shape = c("sigmoid", "linear"), ci = TRUE,
                         ci_level = 0.95, error_bar = TRUE,
                         move_thres = 0.5) {
  ## ---- 1) assemble the standardised data list ----
  lcd <- lc50_build(d, concentration, tested, dead, name = name)

  ## ---- 2) compute the LC estimates ----
  results <- lc50_calculate(lcd, lc = lc, method = method)

  ## ---- 3) optional plots (built, not printed, not saved) ----
  plots <- NULL
  if (plot) {
    shape <- match.arg(shape)
    font <- pkg_resolve_font(font)
    plots <- list()
    for (nm in names(results$results)) {
      gp <- lc50_plot_one(nm, results$results[[nm]], font, unit,
                          shape = shape, ci = ci, ci_level = ci_level,
                          error_bar = error_bar, move_thres = move_thres,
                          method = plot_method)
      if (is.null(gp)) next
      attr(gp, "lc50_name") <-
        if (shape == "sigmoid") nm else paste0(nm, "_linear")
      plots[[nm]] <- gp
    }
  }

  list(data = lcd, results = results, plot = plots)
}

# Internal: assemble the standardised LC data list from a data frame,
# a list of data frames, or three parallel vectors; validation and
# sorting are delegated to lc50_clean()
lc50_build <- function(d = NULL, concentration = NULL, tested = NULL,
                       dead = NULL, name = "bioassay") {
  if (!is.null(d) && (!is.null(concentration) || !is.null(tested) ||
                      !is.null(dead)))
    warning("`d` is supplied; concentration/tested/dead are ignored")
  if (is.null(d)) {
    if (is.null(concentration) || is.null(tested) || is.null(dead))
      stop("Pass either `d` (a data frame or a list of data frames) or ",
           "the three vectors `concentration`, `tested` and `dead`")
    if (diff(range(length(concentration), length(tested), length(dead))) != 0)
      stop("`concentration`, `tested` and `dead` must have the same length")
    d <- data.frame("Concentration" = concentration, "Tested" = tested,
                    "Dead" = dead, check.names = FALSE)
  }
  if (is.data.frame(d))
    return(stats::setNames(list(lc50_clean(d, name)), name))
  if (!is.list(d) || length(d) == 0 ||
      !all(vapply(d, is.data.frame, logical(1))))
    stop("`d` must be a data frame or a (named) list of data frames ",
         "with the columns Concentration/Tested/Dead")
  nms <- names(d)
  if (is.null(nms) || any(!nzchar(nms))) nms <- paste0("dataset", seq_along(d))
  stats::setNames(lapply(seq_along(d), function(i) lc50_clean(d[[i]], nms[i])),
                  nms)
}