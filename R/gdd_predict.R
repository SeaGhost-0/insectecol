# ============================================================
# insectecol --- Degree-day module: prediction (all models)
# ============================================================

#' Predict Developmental Duration at Given Temperatures
#'
#' Evaluates the fitted model (linear or nonlinear) at the given
#' temperatures and returns the predicted developmental rate and
#' duration \eqn{D = 1 / V}.
#'
#' @param object A \code{"gdd"} object returned by [gdd_calc()].
#' @param temp Numeric vector of target temperatures (deg C).
#' @param group Character vector of group names to predict; default is
#'   all groups.
#' @return A data.frame with columns: \code{group}, \code{temp},
#'   \code{pred_rate}, \code{pred_duration}, \code{within_range}.
#' @examples
#' f <- system.file("extdata", "gdd_example.csv", package = "insectecol")
#' fit <- gdd_calc(gdd_read(f), by = "stage")
#' gdd_predict(fit, temp = c(20, 24, 28), group = "Egg")
#' @export
gdd_predict <- function(object, temp, group = NULL) {
  if (!inherits(object, "gdd"))
    stop("object must be a 'gdd' object returned by gdd_calc().",
         call. = FALSE)
  if (!is.numeric(temp))
    stop("temp must be a numeric vector.", call. = FALSE)

  fits <- object$fits
  if (!is.null(group)) {
    idx <- match(group, names(fits))
    if (any(is.na(idx)))
      stop("Group(s) not found: ",
           paste(group[is.na(idx)], collapse = ", "), call. = FALSE)
    fits <- fits[idx]
  }

  out <- do.call(rbind, lapply(names(fits), function(g) {
    f <- fits[[g]]
    ## unified: evaluate the model curve, keep only positive rates
    cf <- gdd_curve(f$model, f$params)
    pr <- cf(temp)
    pr <- ifelse(is.finite(pr) & pr > 0, pr, NA_real_)
    data.frame(group = g, temp = temp,
               pred_rate = pr,
               pred_duration = ifelse(is.na(pr), NA_real_, 1 / pr),
               within_range = temp >= f$obs_temp_range[1] &
                              temp <= f$obs_temp_range[2],
               row.names = NULL)
  }))
  if (any(is.na(out$pred_duration)))
    warning("Some temperatures fall outside the viable range ",
            "(at/below C or at/above the upper threshold); ",
            "duration = NA.", call. = FALSE)
  if (any(!out$within_range))
    warning("Some predicted temperatures fall outside the observed ",
            "range; extrapolation should be treated with caution.",
            call. = FALSE)
  out
}

## S3 generic compatibility: predict(fit, temp = 24)
#' @method predict gdd
#' @export
predict.gdd <- function(object, temp, group = NULL, ...) {
  gdd_predict(object, temp, group)
}