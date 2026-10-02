# ============================================================
# insectecol --- Emergence-period module: core calculation
# Stage-grading projection (fen ling fen ji tui suan fa): one field
# survey of the stage structure -> cumulative development curve ->
# interpolated eclosion dates of the 16% / 50% / 84% quantiles
# (beginning / peak / end of the emergence period), optionally
# shifted by the pre-oviposition period and the egg duration to
# project larval hatching.
# ============================================================

#' Core Emergence-Period Calculation (Stage-Grading Method)
#'
#' Computes the emergence-period projection from one field survey of
#' the population stage structure (the classic Chinese
#' stage-grading method, \emph{fen ling fen ji tui suan fa}). The
#' stages are ordered by their days to eclosion (most developed
#' first), the cumulative development share is built top-down and the
#' eclosion dates of the requested quantiles --- by default 16%
#' (beginning), 50% (peak) and 84% (end of the emergence period,
#' i.e. the mean +/- 1 SD of a normal emergence curve) --- are
#' obtained by linear interpolation of the days-to-eclosion axis,
#' then anchored to the survey date. When \code{pre_ovip} /
#' \code{egg_days} are supplied, the larval hatch dates are projected
#' as well (eclosion + pre-oviposition period + egg duration).
#'
#' @param data A data.frame with a stage column (character), a count
#'   or percent column (numeric) and a days column: the average days
#'   from that stage to adult eclosion at the current temperature.
#' @param stage_col,count_col,percent_col,days_col Column names;
#'   auto-detected by default (English and Chinese aliases, e.g.
#'   stage / count / days or their Chinese equivalents). Exactly one
#'   of count / percent is required.
#' @param survey_date The survey date: a \code{Date} or a character
#'   string (\code{"2026-03-20"}, \code{"2026/3/20"}).
#' @param p Numeric vector of emergence quantiles, default
#'   \code{c(0.16, 0.5, 0.84)}.
#' @param labels Optional labels of the quantiles (same length as
#'   \code{p}); \code{NULL} (default) uses \samp{Beginning (16\%)}
#'   etc. for the default \code{p}, or \samp{16\%}-style labels
#'   otherwise.
#' @param pre_ovip Pre-oviposition period in days (default 0 = not
#'   used).
#' @param egg_days Egg duration in days (default 0 = not used).
#'
#' @details The quantiles that fall outside the surveyed cumulative
#'   range are linearly extrapolated from the outermost segment and
#'   flagged with a warning: a quantile below the share of the most
#'   developed stage has partially eclosed before the survey, a
#'   quantile above the share of the least developed stage indicates
#'   that younger stages were missed. Stages are sorted by days to
#'   eclosion, so the row order of the input is irrelevant; a stage
#'   sharing its days value with another stage is allowed but flagged
#'   (check the stage durations).
#'
#' @return A list of class \code{"emergence"}:
#'   \item{survey_date}{the survey \code{Date}}
#'   \item{table}{data.frame: stage, count (or percent), proportion,
#'     cumulative share, days to eclosion, projected eclosion date}
#'   \item{predictions}{data.frame: label, p, interpolated days
#'     (fractional, measured from the survey date), projected
#'     calendar date, and \code{hatch_date} when applicable}
#'   \item{interpolate}{closure \code{function(p)} returning the
#'     interpolated days for arbitrary quantiles (for further
#'     programming)}
#'   \item{n, n_stages, pre_ovip, egg_days, p, labels}{the inputs}
#' @seealso \code{\link{emergence_analyze}} (the main entry point),
#'   \code{\link{emergence_read}}, \code{\link{emergence_export}}
#' @examples
#' ## Overwintering-generation survey, 40 individuals (Tianyang case):
#' d <- data.frame(
#'   stage = c("Pupal exuviae", paste("Pupa", 7:1), "Prepupa", "Larva 5"),
#'   count = c(2, 3, 5, 7, 7, 5, 3, 4, 2, 2),
#'   days  = seq(0, 18, 2))          # days from that stage to eclosion
#' fit <- emergence_calc(d, survey_date = "2026-03-20")
#' fit                                  # three quantile dates
#' fit$table                            # cumulative development
#' predict(fit, c(0.25, 0.75))          # arbitrary quantiles
#'
#' ## With the hatch projection (pre-oviposition 3 d + egg 10 d):
#' fit2 <- emergence_calc(d, survey_date = "2026-03-20",
#'                        pre_ovip = 3, egg_days = 10)
#' fit2$predictions
#' @export
emergence_calc <- function(data, stage_col = NULL, count_col = NULL,
                           percent_col = NULL, days_col = NULL,
                           survey_date, p = c(0.16, 0.5, 0.84),
                           labels = NULL, pre_ovip = 0, egg_days = 0) {
  call <- match.call()
  if (!is.data.frame(data)) data <- as.data.frame(data)
  cols <- emergence_detect_cols(data, stage_col = stage_col,
                                count_col = count_col,
                                percent_col = percent_col,
                                days_col = days_col)
  stage <- as.character(data[[cols$stage]])
  days  <- suppressWarnings(as.numeric(as.character(data[[cols$days]])))
  is_count <- !is.null(cols$count)
  vcol  <- if (is_count) cols$count else cols$percent
  value <- suppressWarnings(as.numeric(as.character(data[[vcol]])))

  ## ---- clean rows ----
  bad <- !is.finite(days) | !is.finite(value) | !nzchar(stage)
  if (any(bad)) {
    message("Dropped ", sum(bad), " row(s) with a missing stage, ",
            "count/percent or days entry.")
    stage <- stage[!bad]; days <- days[!bad]; value <- value[!bad]
  }
  if (any(value < 0))
    stop("Counts / percentages must be non-negative.", call. = FALSE)
  if (any(days < 0))
    stop("Days to eclosion must be non-negative (no stage can eclose ",
         "before the survey date).", call. = FALSE)
  zero <- value == 0
  if (any(zero)) {
    message("Dropped ", sum(zero), " stage(s) with a zero count: ",
            paste(stage[zero], collapse = ", "))
    stage <- stage[!zero]; days <- days[!zero]; value <- value[!zero]
  }
  if (length(days) < 2)
    stop("At least two stages are required for the interpolation.",
         call. = FALSE)
  if (any(duplicated(days)))
    warning("Stages sharing the same days-to-eclosion value: ",
            paste(stage[duplicated(days)], collapse = ", "),
            " - please check the stage durations.", call. = FALSE)

  survey_date <- emergence_as_date(survey_date)

  ## ---- order most-developed first (ascending days) ----
  ord <- order(days)
  if (!identical(ord, seq_along(ord)))
    message("Rows sorted by days to eclosion (most developed first).")
  stage <- stage[ord]; days <- days[ord]; value <- value[ord]

  prop <- value / sum(value)
  cum  <- cumsum(prop)
  cum[length(cum)] <- 1

  ## ---- quantiles ----
  p <- suppressWarnings(as.numeric(p))
  if (length(p) == 0 || any(!is.finite(p)) || any(p <= 0 | p >= 1))
    stop("p must contain probabilities strictly between 0 and 1.",
         call. = FALSE)
  if (is.null(labels)) {
    labels <- if (isTRUE(all.equal(p, c(0.16, 0.5, 0.84))))
      c("Beginning (16%)", "Peak (50%)", "End (84%)")
    else sprintf("%g%%", p * 100)
  } else if (length(labels) != length(p)) {
    stop("labels must have the same length as p.", call. = FALSE)
  }

  pre_ovip <- as.numeric(pre_ovip)[1]
  egg_days <- as.numeric(egg_days)[1]
  if (!is.finite(pre_ovip) || pre_ovip < 0 ||
      !is.finite(egg_days) || egg_days < 0)
    stop("pre_ovip and egg_days must be non-negative numbers of days.",
         call. = FALSE)
  has_hatch <- pre_ovip > 0 || egg_days > 0

  res <- emergence_interp(days, cum, p)
  pred <- data.frame(label = labels, p = p, days = res$days,
                     date = as.Date(survey_date + floor(res$days)),
                     stringsAsFactors = FALSE, check.names = FALSE)
  if (has_hatch)
    pred$hatch_date <- as.Date(survey_date + floor(res$days +
                                pre_ovip + egg_days))

  table <- data.frame(stage = stage, days = days, value = value,
                      proportion = prop, cumulative = cum,
                      eclosion_date = as.Date(survey_date + floor(days)),
                      stringsAsFactors = FALSE, check.names = FALSE)
  names(table)[match("value", names(table))] <-
    if (is_count) "count" else "percent"

  structure(
    list(call = call, survey_date = survey_date, table = table,
         p = p, labels = labels, pre_ovip = pre_ovip,
         egg_days = egg_days, has_hatch = has_hatch,
         n = if (is_count) sum(value) else NA_real_,
         n_stages = nrow(table),
         predictions = pred,
         interpolate = res$fun),
    class = "emergence")
}

## Internal: parse the survey date (Date, numeric or character)
emergence_as_date <- function(x) {
  d <- if (inherits(x, "Date")) {
    x[1]
  } else if (is.numeric(x)) {
    as.Date(x[1], origin = "1970-01-01")
  } else {
    ## as.Date() errors (rather than returning NA) when the string
    ## matches none of the tryFormats
    d <- suppressWarnings(tryCatch(
      as.Date(as.character(x[1]),
              tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d")),
      error = function(e) NA))
  }
  if (length(d) != 1 || is.na(d))
    stop("Cannot parse survey_date: ", x[1],
         " (use e.g. \"2026-03-20\").", call. = FALSE)
  d
}

## Internal: interpolate the days-to-eclosion axis at quantiles q.
## The surveyed points (days_i, cum_i) define a piecewise linear
## eclosion curve: at survey date + days_i the cumulative eclosed
## share is cum_i. Inside the surveyed range the quantile days are
## interpolated; outside they are linearly extrapolated from the
## outermost segment with a positive day width (with a warning).
emergence_interp <- function(days, cum, q) {
  n <- length(days)
  wid <- which(diff(days) > 0)
  left  <- if (length(wid)) wid[1] else NA_integer_
  right <- if (length(wid)) wid[length(wid)] else NA_integer_
  extrap <- integer(length(q))
  out <- numeric(length(q))
  for (i in seq_along(q)) {
    qi <- q[i]
    if (qi >= cum[n]) {
      if (is.na(right)) {
        out[i] <- days[n]        # all days equal: clamp
      } else {
        out[i] <- days[right] + (qi - cum[right]) /
          (cum[right + 1] - cum[right]) *
          (days[right + 1] - days[right])
      }
      extrap[i] <- 1
    } else if (qi <= cum[1]) {
      if (is.na(left)) {
        out[i] <- days[1]
      } else {
        out[i] <- days[left] - (cum[left] - qi) /
          (cum[left + 1] - cum[left]) *
          (days[left + 1] - days[left])
      }
      extrap[i] <- -1
    } else {
      k <- findInterval(qi, cum)
      out[i] <- days[k] + (qi - cum[k]) /
        (cum[k + 1] - cum[k]) * (days[k + 1] - days[k])
    }
  }
  if (any(extrap == -1))
    warning(sprintf(
      paste0("Quantile(s) %s lie at or below the cumulative share of ",
             "the most developed stage (%.1f%%): the date is ",
             "extrapolated backwards from the first segment."),
      paste(sprintf("%g%%", q[extrap == -1] * 100), collapse = ", "),
      cum[1] * 100), call. = FALSE)
  if (any(extrap == 1))
    warning(sprintf(
      paste0("Quantile(s) %s lie at or above the cumulative share of ",
             "the least developed stage (%.1f%%): the date is ",
             "extrapolated from the last segment - the survey may have ",
             "missed younger stages."),
      paste(sprintf("%g%%", q[extrap == 1] * 100), collapse = ", "),
      cum[n] * 100), call. = FALSE)
  list(days = out,
       fun = function(qq) emergence_interp(days, cum, qq)$days)
}
