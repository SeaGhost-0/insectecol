# ==================== Internal helpers ====================

# Age-stage survival counts (core of the original survival_rate function)
.calc_survival_counts <- function(data, n) {
  all_results <- list()
  max_wide <- 1; max_length <- 1

  # Determine the maximum lifespan (in days) and the number of stages
  for (i in 1:nrow(data)) {
    row_char <- as.character(unlist(data[i, ]))
    n_pos <- which(row_char == "F" | row_char == "M" | row_char == "N")
    if (length(n_pos) == 0) next                  # skip rows without a sex marker
    n_pos <- n_pos[1]
    sub_char <- row_char[1:n_pos]
    te <- which(!is.na(sub_char) & sub_char != "" &
                  sub_char != "F" & sub_char != "M" & sub_char != "N")
    wide <- length(te)
    if (max_wide < wide) max_wide = wide
    num_values <- as.numeric(row_char[te[2:length(te)]])
    length_val <- sum(num_values, na.rm = TRUE)
    if (max_length < length_val) max_length = length_val
  }

  # Lay out the survival days of every individual and add them up element-wise
  i <- 1
  while (i <= nrow(data)) {
    row_char <- as.character(unlist(data[i, ]))
    n_pos <- which(row_char == "F" | row_char == "M" | row_char == "N")
    if (length(n_pos) == 0) { i <- i + 1; next }
    n_pos <- n_pos[1]
    num_cols <- which(!is.na(data[i, 1:(n_pos - 1)]))
    num_values <- as.numeric(row_char[num_cols[2:length(num_cols)]])
    last_char <- row_char[n_pos]

    result_df <- data.frame(matrix(0, nrow = max_length, ncol = max_wide))
    start_row <- 1; x <- 1
    while (x <= length(num_values)) {
      if (!is.na(num_values[x]) && num_values[x] != 0) {
        end_row <- start_row + abs(num_values[x]) - 1
        if (x == length(num_values)) {            # last column: assign by sex
          if (last_char == "F")      col <- ncol(result_df) - 1
          else if (last_char == "M") col <- ncol(result_df)
          else                       col <- x
        } else col <- x
        if (end_row <= nrow(result_df)) result_df[start_row:end_row, col] <- 1
        start_row <- end_row + 1
      }
      x <- x + 1
    }
    all_results[[i]] <- result_df
    i <- i + 1
  }

  all_results <- all_results[!sapply(all_results, is.null)]
  if (length(all_results) == 0) stop("No valid data rows containing a sex marker (F/M/N) were found")
  list(counts = Reduce(`+`, all_results), max_length = max_length,
       N = nrow(data))
}

# Number of females
.count_females <- function(data) {
  sum(apply(data, 2, function(x) sum(x == "F", na.rm = TRUE)))
}

# Age-specific fecundity of the females (core of the original fecundity_xj
# function); returns a numeric vector
.calc_fxj_raw <- function(lt, sxj_length, count_F) {
  data <- lt$data
  result_egg <- data.frame(matrix(NA, nrow = sxj_length, ncol = count_F))
  i <- 1; z <- 1
  while (i <= nrow(data)) {
    row_char_i <- as.character(unlist(data[i, ]))
    if (row_char_i[lt$n] == "F") {                # female
      row_data <- data[i, lt$n_1:ncol(data)]
      num_values <- as.numeric(row_data[!is.na(row_data)])
      num_length <- sum(as.numeric(data[i, 2:lt$n_2]), na.rm = TRUE)
      start_row <- num_length + 1                 # age at emergence
      x <- 1
      while (x <= length(num_values)) {
        if (start_row <= nrow(result_egg)) {
          result_egg[start_row, z] <- num_values[x]
        }
        start_row <- start_row + 1; x <- x + 1
      }
      z <- z + 1
    }
    i <- i + 1
  }
  average <- rep(0, sxj_length)
  for (c in 1:sxj_length) {
    egg_age <- result_egg[c, ]
    average[c] <- sum(egg_age[!is.na(egg_age)]) /
      length(egg_age[!is.na(egg_age)])
  }
  average
}

# Age-specific female fecundity vector (NAs replaced by 0, for use in m_x and R0)
.fxj_values <- function(lt, sxj = NULL) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  F_r <- .calc_fxj_raw(lt, nrow(sxj), .count_females(lt$data))
  replace(F_r, is.na(F_r), 0)
}

# Solve the Euler-Lotka equation by bisection (originally
# Intrinsicrate_of_increase)
.intrinsic_rate <- function(l_x, m_x) {
  obj_fun <- function(r) {
    sum_val <- 0
    for (x in 0:(length(l_x) - 1)) {
      sum_val <- sum_val + exp(-r * (x + 1)) * l_x[x + 1] * m_x[x + 1]
    }
    sum_val - 1
  }
  bisection_method <- function(func, a, b, tol = 1e-6, max_iter = 100) {
    if (func(a) * func(b) >= 0) stop("The function values at both ends of the interval [a, b] must have opposite signs")
    for (i in 1:max_iter) {
      c <- (a + b) / 2
      fc <- func(c)
      if (abs(fc) < tol) return(c)
      if (func(a) * fc < 0) b <- c else a <- c
    }
    warning("Maximum number of iterations reached; a solution within the tolerance may not have been found")
    (a + b) / 2
  }
  bisection_method(obj_fun, 0, 1)
}

# ==================== Indicator functions (one per indicator) ====================

#' Cohort Size N
#'
#' The number of individuals in the raw data set, i.e. the number of
#' newly hatched eggs with which the cohort started. All survival rates
#' of the age-stage, two-sex life table are expressed as proportions of
#' this cohort size.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @return A single numeric value: the number of data rows.
#'
#' @references Chi, H. (1988) Life-table analysis incorporating both
#'   sexes and variable development rates among individuals.
#'   \emph{Environmental Entomology} 17(1), 26-34.
#'
#' @seealso \code{\link{lifeTable_calculate_all}} computes all parameters at once.
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_N(lt)
calc_N <- function(lt) nrow(lt$data)

#' Mean Fecundity F
#'
#' The mean fecundity of the females of the cohort: the total number of
#' eggs laid by all females divided by the number of females,
#' \code{F = (total number of eggs) / (number of females)}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @return A single numeric value: the mean number of eggs per female.
#'
#' @seealso \code{\link{calc_fxj}} for the age-specific fecundity.
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_F(lt)
calc_F <- function(lt) {
  egg_number <- sum(apply(lt$data[, lt$n_1:ncol(lt$data)], 2,
                          function(x) sum(as.numeric(x), na.rm = TRUE)))
  count_F <- .count_females(lt$data)
  if (count_F == 0) stop("No females (F) found in the data; mean fecundity cannot be calculated")
  egg_number / count_F
}

#' Age-Stage Survival Rate s_xj
#'
#' Calculates the age-stage survival rate: the proportion of the original
#' cohort that is alive and in developmental stage j at age x,
#' \code{s_xj = n_xj / N}, where \code{n_xj} is the number of individuals
#' of age x in stage j. This is the fundamental curve set of the
#' age-stage, two-sex life table: because individuals of the same age may
#' be in different stages, s_xj describes the cohort much better than a
#' single l_x curve.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#'
#' @details The returned data frame has one row per age class (day) and
#'   one column per stage, in the order of the stage names returned by
#'   \code{\link{get_stage_names}} (immature stages first, then
#'   \code{"Female"} and \code{"Male"}).
#'
#' @return A data frame of age-stage survival rates (values in [0, 1]):
#'   rows = ages in days, columns = developmental stages.
#'
#' @references
#' Chi, H. and Liu, H. (1985) Two new methods for study of insect
#' population ecology. \emph{Bull. Inst. Zool. Acad. Sin} 24(2), 225-240.
#'
#' Chi, H. (1988) Life-table analysis incorporating both sexes and
#' variable development rates among individuals. \emph{Environmental
#' Entomology} 17(1), 26-34.
#'
#' @seealso \code{\link{get_stage_names}}, \code{\link{calc_lx}},
#'   \code{\link{plot_sxj}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(calc_sxj(lt))
calc_sxj <- function(lt) {
  sc <- .calc_survival_counts(lt$data, lt$n)
  rates <- sc$counts / sc$N
  gp <- get_stage_names(lt)
  if (length(gp) == ncol(rates)) {
    colnames(rates) <- gp
  } else {
    warning("The number of stage names does not match the number of data columns; default column names are kept")
  }
  rates
}

#' Age-Specific Survival Rate l_x
#'
#' The proportion of the original cohort that is still alive at age x,
#' obtained by summing the age-stage survival rates over all stages:
#' \code{l_x = sum over j of s_xj}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param sxj Optional; the result of \code{\link{calc_sxj}}. Supplying it
#'   avoids recomputing the age-stage survival rates.
#'
#' @return A data frame with the columns \code{Age} and \code{l_x}; a
#'   terminal row with \code{l_x = 0} is appended so that the survival
#'   curve ends at zero.
#'
#' @references Chi, H. (1988) Life-table analysis incorporating both
#'   sexes and variable development rates among individuals.
#'   \emph{Environmental Entomology} 17(1), 26-34.
#'
#' @seealso \code{\link{calc_sxj}}, \code{\link{calc_mx}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(calc_lx(lt))
calc_lx <- function(lt, sxj = NULL) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  l_x <- apply(sxj, 1, sum)
  l_df <- data.frame("Age" = NA, "l_x" = NA)
  l_df[(1:length(l_x)), 2] <- l_x
  l_df[length(l_x) + 1, 2] <- 0
  l_df[(1:(length(l_x) + 1)), 1] <- 1:(length(l_x) + 1)
  l_df
}

#' Age-Specific Female Fecundity F_xj
#'
#' The mean number of eggs laid per living female at age x. For every
#' female the daily egg counts are aligned to her age at emergence, so
#' that eggs are attributed to the correct age class; the mean is taken
#' over the females still alive at each age.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param sxj Optional; the result of \code{\link{calc_sxj}}. Supplying it
#'   avoids recomputing the age-stage survival rates.
#'
#' @return A data frame with the columns \code{Age} and \code{F_xj}; a
#'   terminal row with \code{F_xj = 0} is appended.
#'
#' @seealso \code{\link{calc_F}} for the overall mean fecundity,
#'   \code{\link{calc_mx}} for the age-specific fecundity of the cohort.
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(calc_fxj(lt))
calc_fxj <- function(lt, sxj = NULL) {
  F_r <- .fxj_values(lt, sxj)
  F_df <- data.frame("Age" = NA, "F_xj" = NA)
  F_df[(1:length(F_r)), 2] <- F_r
  F_df[length(F_r) + 1, 2] <- 0
  F_df[(1:(length(F_r) + 1)), 1] <- 1:(length(F_r) + 1)
  F_df
}

#' Age-Specific Fecundity m_x
#'
#' The age-specific fecundity of the cohort, computed from the female
#' age-stage survival rate, the age-specific female fecundity and the
#' overall survival rate: \code{m_x = s_x,Female * F_xj / l_x}. With this
#' definition \code{sum(l_x * m_x)} equals the net reproductive rate
#' \code{\link{calc_R0}}, so the Euler-Lotka equation solved by
#' \code{\link{calc_r}} is consistent.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param sxj Optional; the result of \code{\link{calc_sxj}}.
#' @param fxj Optional; the result of \code{\link{calc_fxj}}.
#' @param lx Optional; the result of \code{\link{calc_lx}}. Supplying any
#'   of these avoids recomputing them.
#'
#' @return A data frame with the columns \code{Age} and \code{m_x}; a
#'   terminal row with \code{m_x = 0} is appended.
#'
#' @references Chi, H. and Liu, H. (1985) Two new methods for the study
#'   of insect population ecology. \emph{Bull. Inst. Zool. Acad. Sin} 24(2), 225-240.
#'
#' @seealso \code{\link{calc_fxj}}, \code{\link{calc_lx}}, \code{\link{calc_r}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(calc_mx(lt))
calc_mx <- function(lt, sxj = NULL, fxj = NULL, lx = NULL) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  if (is.null(fxj)) fxj <- calc_fxj(lt, sxj)
  if (is.null(lx))  lx  <- calc_lx(lt, sxj)
  s   <- unlist(sxj[ncol(sxj) - 1], use.names = FALSE)   # female survival column
  F_r <- fxj[["F_xj"]][1:(nrow(fxj) - 1)]                # drop the appended zero row
  l_x <- lx[["l_x"]][1:(nrow(lx) - 1)]
  m_x <- s * F_r / l_x
  m_df <- data.frame("Age" = NA, "m_x" = NA)
  m_df[(1:length(m_x)), 2] <- m_x
  m_df[length(m_x) + 1, 2] <- 0
  m_df[(1:(length(m_x) + 1)), 1] <- 1:(length(m_x) + 1)
  m_df
}

#' Net Reproductive Rate R0
#'
#' The net reproductive rate: the expected number of eggs produced by an
#' average individual of the cohort over its whole life,
#' \code{R0 = sum over x of s_x,Female * F_xj}. A population with
#' \code{R0 = 1} exactly replaces itself; \code{R0 > 1} indicates growth
#' and \code{R0 < 1} decline.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param sxj Optional; the result of \code{\link{calc_sxj}}.
#' @param fxj Optional; the result of \code{\link{calc_fxj}}. Supplying
#'   them avoids recomputing.
#'
#' @return A single numeric value: the net reproductive rate R0.
#'
#' @references Goodman, D. (1982) Optimal life histories, optimal
#'   notation, and the value of reproductive value. \emph{The American
#'   Naturalist} 119(6), 803-823.
#'
#' @seealso \code{\link{calc_T}}, \code{\link{lifeTable_calculate_all}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_R0(lt)
calc_R0 <- function(lt, sxj = NULL, fxj = NULL) {
  if (is.null(sxj)) sxj <- calc_sxj(lt)
  if (is.null(fxj)) fxj <- calc_fxj(lt, sxj)
  s   <- unlist(sxj[ncol(sxj) - 1], use.names = FALSE)
  F_r <- fxj[["F_xj"]][1:(nrow(fxj) - 1)]
  sum(s * F_r, na.rm = TRUE)
}

#' Intrinsic Rate of Increase r
#'
#' The intrinsic rate of increase (instantaneous rate of natural
#' increase): the positive root of the Euler-Lotka equation
#' \code{sum over x of l_x * m_x * exp(-r * x) = 1}, where x runs over
#' the age classes (days) starting at 1. The root is located with the
#' bisection method on the interval [0, 1] (tolerance 1e-6, at most 100
#' iterations).
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param lx Optional; the result of \code{\link{calc_lx}}.
#' @param mx Optional; the result of \code{\link{calc_mx}}. Supplying them
#'   avoids recomputing.
#'
#' @return A single numeric value: the intrinsic rate of increase r
#'   (per day).
#'
#' @references Birch, L. C. (1948) The intrinsic rate of natural increase
#'   of an insect population. \emph{Journal of Animal Ecology} 17(1), 15-26.
#'
#' @seealso \code{\link{calc_lambda}}, \code{\link{calc_T}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_r(lt)
calc_r <- function(lt, lx = NULL, mx = NULL) {
  if (is.null(lx)) lx <- calc_lx(lt)
  if (is.null(mx)) mx <- calc_mx(lt)
  l_x <- lx[["l_x"]][1:(nrow(lx) - 1)]
  m_x <- mx[["m_x"]][1:(nrow(mx) - 1)]
  .intrinsic_rate(l_x, m_x)
}

#' Finite Rate of Increase lambda
#'
#' The finite rate of increase: the multiplication factor of the
#' population per unit time (day), \code{lambda = exp(r)}. The population
#' grows when \code{lambda > 1}, stays constant at \code{lambda = 1} and
#' declines when \code{lambda < 1}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param r Optional; the result of \code{\link{calc_r}}. Supplying it
#'   avoids recomputing.
#'
#' @return A single numeric value: the finite rate of increase lambda.
#'
#' @references Birch, L. C. (1948) The intrinsic rate of natural increase
#'   of an insect population. \emph{Journal of Animal Ecology} 17(1), 15-26.
#'
#' @seealso \code{\link{calc_r}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_lambda(lt)
calc_lambda <- function(lt, r = NULL) {
  if (is.null(r)) r <- calc_r(lt)
  exp(r)
}

#' Mean Generation Time T
#'
#' The mean generation time, calculated as \code{T = log(R0) / r}: the
#' time needed for the population to grow to an \code{R0}-fold of its
#' current size when increasing at the constant rate \code{r}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param R0 Optional; the result of \code{\link{calc_R0}}.
#' @param r Optional; the result of \code{\link{calc_r}}. Supplying them
#'   avoids recomputing.
#'
#' @return A single numeric value: the mean generation time T (days).
#'
#' @references Goodman, D. (1982) Optimal life histories, optimal
#'   notation, and the value of reproductive value. \emph{The American
#'   Naturalist} 119(6), 803-823.
#'
#' @seealso \code{\link{calc_R0}}, \code{\link{calc_r}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' calc_T(lt)
calc_T <- function(lt, R0 = NULL, r = NULL) {
  if (is.null(R0)) R0 <- calc_R0(lt)
  if (is.null(r))  r  <- calc_r(lt)
  log(R0) / r
}

#' Life Expectancy e_x
#'
#' The life expectancy of the individuals that have reached age x,
#' calculated as the cumulative sum of the age-specific survival rates
#' from age x to the end of the life table,
#' \code{e_x = sum over y >= x of l_y}.
#'
#' @param lt A \code{life_table} object returned by
#'   \code{\link{read_life_table}}.
#' @param lx Optional; the result of \code{\link{calc_lx}}. Supplying it
#'   avoids recomputing.
#'
#' @return A data frame with the columns \code{Age} and \code{e_x}; one
#'   row per age class.
#'
#' @references Chi, H. and Liu, H. (1985) Two new methods for the study
#'   of insect population ecology. \emph{Bulletin of the Institute of
#'   Zoology, Academia Sinica} 24(2), 225-240.
#'
#' @seealso \code{\link{calc_lx}}
#' @export
#' @examples
#' f <- system.file("extdata", "Example.csv", package = "insectecol")
#' lt <- read_life_table(f)
#' head(calc_ex(lt))
calc_ex <- function(lt, lx = NULL) {
  if (is.null(lx)) lx <- calc_lx(lt)
  l_x <- lx[["l_x"]][1:(nrow(lx) - 1)]
  e_x <- data.frame("Age" = NA, "e_x" = NA)
  for (e in 1:(length(l_x) + 1)) {
    e_x[e, 1] <- e
    e_x[e, 2] <- sum(replace(l_x[e:(length(l_x) + 1)],
                             is.na(l_x[e:(length(l_x) + 1)]), 0))
  }
  e_x
}
