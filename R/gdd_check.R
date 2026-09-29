# ============================================================
# insectecol --- Degree-day module: data checking (long format)
# Long-format counterpart of check_data() (which validates the wide
# life-table layout); check_data() itself stays untouched.
# ============================================================

#' Check Developmental Data (Long Format) Before Fitting
#'
#' Non-stopping validation of a long-format development data frame (one
#' row per observation with temperature and duration columns), i.e. the
#' layout required by \code{\link{gdd_calc}}. Reports per-row problems
#' (missing / non-numeric / non-positive values), a per-group summary
#' (sample sizes, temperature coverage) and a linear-range check: if
#' the highest mean developmental rate is not reached at the highest
#' temperature, the data contain a high-temperature decline and the
#' linear model must not be fitted to the full range.
#'
#' @param data A data.frame with temperature and duration columns.
#' @param temp_col,duration_col Column names (auto-detected by default).
#' @param by Optional grouping variable(s), as in \code{\link{gdd_calc}}.
#' @return A list:
#'   \item{valid}{logical vector, one entry per row of \code{data}}
#'   \item{problems}{data.frame (row, column, reason); empty if none}
#'   \item{group_summary}{data.frame per group: n, n_temp, temperature
#'     and duration ranges}
#'   \item{linear_check}{data.frame per group: temperature of the
#'     maximal mean rate, highest temperature, \code{rate_declines} flag}
#'   \item{data}{the valid rows, standardised to columns temp /
#'     duration / rate / group (NULL if no valid row remains)}
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' chk <- gdd_check(gdd_read(f), by = "stage")
#' chk$group_summary   # per-group sample sizes and ranges
#' chk$linear_check    # is the maximal mean rate at the highest temperature?
#' @seealso \code{\link{gdd_read}},
#'   \code{\link{gdd_calc}}
#' @export
gdd_check <- function(data, temp_col = NULL, duration_col = NULL,
                      by = NULL) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  cols <- gdd_detect_cols(data, temp_col, duration_col)
  n <- nrow(data)

  tv <- suppressWarnings(as.numeric(as.character(data[[cols$temp]])))
  dv <- suppressWarnings(as.numeric(as.character(data[[cols$duration]])))

  scan_col <- function(raw, num, colname, positive = FALSE) {
    miss   <- is.na(raw) | trimws(as.character(raw)) == ""
    nonnum <- !miss & is.na(num)
    nonpos <- positive & !miss & !nonnum & is.finite(num) & num <= 0
    idx <- which(miss | nonnum | nonpos)
    if (!length(idx)) return(NULL)
    reason <- ifelse(miss[idx], "missing",
              ifelse(nonnum[idx], "non-numeric", "value <= 0"))
    data.frame(row = idx, column = colname, reason = reason,
               row.names = NULL)
  }
  problems <- rbind(scan_col(data[[cols$temp]],     tv, cols$temp),
                    scan_col(data[[cols$duration]], dv, cols$duration,
                             positive = TRUE))
  if (is.null(problems))
    problems <- data.frame(row = integer(), column = character(),
                           reason = character())

  valid <- rep(TRUE, n)
  if (nrow(problems)) valid[problems$row] <- FALSE

  grp <- if (is.null(by)) {
    rep("Overall", n)
  } else {
    if (!all(by %in% names(data)))
      stop("Grouping variable(s) not found: ",
           paste(setdiff(by, names(data)), collapse = ", "), call. = FALSE)
    apply(data[by], 1, paste, collapse = "-")
  }

  ok <- if (any(valid))
    data.frame(temp = tv[valid], duration = dv[valid],
               rate = 1 / dv[valid], group = grp[valid]) else NULL

  if (!is.null(ok)) {
    group_summary <- do.call(rbind, lapply(split(ok, ok$group), function(g)
      data.frame(group = g$group[1], n = nrow(g),
                 n_temp = length(unique(g$temp)),
                 temp_min = min(g$temp), temp_max = max(g$temp),
                 dur_min = min(g$duration), dur_max = max(g$duration),
                 row.names = NULL)))
    linear_check <- do.call(rbind, lapply(split(ok, ok$group), function(g) {
      mr <- tapply(g$rate, g$temp, mean)   # replicates are averaged
      tt <- as.numeric(names(mr)); o <- order(tt)
      mr <- mr[o]; tt <- tt[o]
      data.frame(group = g$group[1], n_temp = length(tt),
                 temp_of_max_rate = tt[which.max(mr)],
                 highest_temp = max(tt),
                 rate_declines = which.max(mr) != length(mr),
                 row.names = NULL)
    }))
  } else {
    group_summary <- NULL; linear_check <- NULL
    warning("No valid rows remain after checking.", call. = FALSE)
  }

  list(valid = valid, problems = problems,
       group_summary = group_summary, linear_check = linear_check,
       data = ok)
}