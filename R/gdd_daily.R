# ============================================================
# insectecol --- Degree-day module: daily field accumulation
# Used for forecasting emergence / phenology in the field.
# ============================================================

#' Daily Accumulation of Degree-Days (Field Application)
#'
#' Accumulates daily degree-days above the developmental threshold
#' temperature, useful for predicting field phenology (e.g. the date
#' on which a stage completes its effective accumulated temperature K).
#'
#' @param Tmin,Tmax Numeric vectors of daily minimum / maximum
#'   temperatures (deg C).
#' @param C Developmental threshold temperature (deg C), e.g. taken
#'   from a \code{gdd_calc()} fit: \code{fit$fits$Egg$C}.
#' @param method \code{"avg"} (default) uses the daily mean temperature;
#'   \code{"triangle"} uses the single-triangle method, which credits
#'   partial degree-days when the daily minimum lies below C.
#' @return A list with \code{daily}, \code{cumulative} and \code{total}.
#' @examples
#' gdd_daily(c(8, 10, 12), c(20, 22, 25), C = 11)$total
#' @export
gdd_daily <- function(Tmin, Tmax, C, method = c("avg", "triangle")) {
  method <- match.arg(method)
  if (!is.numeric(Tmin) || !is.numeric(Tmax))
    stop("Tmin and Tmax must be numeric vectors.", call. = FALSE)
  if (length(Tmin) != length(Tmax))
    stop("Tmin and Tmax must have the same length.", call. = FALSE)
  if (!length(Tmin))
    stop("Tmin/Tmax are empty.", call. = FALSE)
  if (anyNA(Tmin) || anyNA(Tmax))
    stop("Tmin/Tmax contain NA values; please remove or impute them ",
         "before calling gdd_daily().", call. = FALSE)
  if (any(Tmin > Tmax))
    stop("Some Tmin values exceed Tmax; please check the input.",
         call. = FALSE)
  Tmean <- (Tmin + Tmax) / 2
  daily <- switch(method,
    avg      = pmax(0, Tmean - C),
    triangle = ifelse(Tmin >= C, Tmean - C,
               ifelse(Tmax <= C, 0, (Tmax - C)^2 / (2 * (Tmax - Tmin)))))
  list(daily = daily, cumulative = cumsum(daily), total = sum(daily))
}