## Packaged example data (inst/extdata), read once for all tests
gdd_csv   <- system.file("extdata", "gdd_example.csv", package = "insectecol")
gdd_batch <- system.file("extdata", "gdd_batch", package = "insectecol")
df_ex     <- gdd_read(gdd_csv)

test_that("gdd_read reads the packaged csv example", {
  expect_equal(nrow(df_ex), 18)
  expect_true(all(c("stage", "temp", "duration") %in% names(df_ex)))
})

test_that("gdd_read batch mode: one file per temperature", {
  b <- gdd_read(gdd_batch, temp_from_file = TRUE)
  expect_equal(nrow(b), 18)
  expect_setequal(b$temp, c(16, 19, 22, 25, 28, 31))
  expect_true("source_file" %in% names(b))
  ## same fits as the long-format csv
  r1 <- gdd_calc(df_ex, by = "stage")$results
  r2 <- gdd_calc(b,    by = "stage")$results
  expect_equal(r1$C, r2$C)
  expect_equal(r1$K, r2$K)
})

test_that("gdd_read rejects an invalid path", {
  expect_error(gdd_read("Z:/no/such/path.csv"), "Invalid path")
})

test_that("Linear model (default) recovers exact parameters", {
  T <- c(16, 19, 22, 25, 28)
  res <- gdd_calc(data.frame(temp = T, duration = 150 / (T - 10)))
  expect_s3_class(res, "gdd")
  expect_equal(res$results$model, "linear")
  expect_equal(res$results$C, 10,  tolerance = 1e-8)
  expect_equal(res$results$K, 150, tolerance = 1e-8)
})

test_that("Grouped linear fits match per-group fits", {
  T1 <- c(16, 19, 22, 25); T2 <- c(15, 18, 21, 24)
  df <- data.frame(stage = c(rep("A", 4), rep("B", 4)),
                   temp = c(T1, T2),
                   duration = c(100 / (T1 - 12), 200 / (T2 - 9)))
  res <- gdd_calc(df, by = "stage")
  expect_equal(res$results[res$results$group == "A", ]$C,
               gdd_calc(subset(df, stage == "A"))$results$C)
})

test_that("Invalid input raises errors", {
  expect_error(gdd_calc(data.frame(x = 1:3, y = 1:3)), "temperature")
  expect_error(gdd_calc(data.frame(temp = 16, duration = 5)), "at least 3")
})

test_that("predict works for the linear model", {
  T <- c(16, 19, 22, 25, 28)
  res <- gdd_calc(data.frame(temp = T, duration = 150 / (T - 10)))
  expect_equal(gdd_predict(res, temp = 20)$pred_duration, 15,
               tolerance = 1e-6)
})

test_that("Briere-1 recovers known parameters", {
  set.seed(42)
  a <- 2e-4; T0 <- 10; Tm <- 35
  T <- seq(15, 30, by = 2.5)
  V <- a * T * (T - T0) * sqrt(Tm - T) + rnorm(length(T), 0, 2e-4)
  fit <- gdd_calc(data.frame(temp = T, duration = 1 / V),
                  model = "briere1")
  f <- fit$fits$Overall
  expect_equal(unname(f$params[["a"]]),  a,  tolerance = 0.1)
  expect_equal(unname(f$params[["T0"]]), T0, tolerance = 0.1)
  expect_equal(unname(f$params[["Tm"]]), Tm, tolerance = 0.1)
  expect_equal(f$C, T0, tolerance = 0.1)          # C = T0 (direct)
  expect_equal(f$Tmax_est, Tm, tolerance = 0.1)
  expect_true(is.finite(f$Topt))
})

test_that("'auto' selects the generating model and skips over-parameterized ones", {
  set.seed(7)
  a <- 3e-4; T0 <- 12; Tm <- 34
  T <- seq(16, 30, by = 2)                        # 8 points
  V <- a * T * (T - T0) * sqrt(Tm - T) + rnorm(length(T), 0, 1e-5)
  fit <- gdd_calc(data.frame(temp = T, duration = 1 / V), model = "auto")
  expect_equal(fit$fits$Overall$model, "briere1")
  cmp <- fit$comparison
  expect_equal(nrow(cmp), 6)
  expect_equal(cmp$note[cmp$model == "wang"], "insufficient n")
  expect_true(all(cmp$delta_AICc[cmp$best] == 0))
})

test_that("gdd_compare returns the full comparison table", {
  set.seed(11)
  T <- seq(16, 30, by = 2)
  V <- 2e-4 * T * (T - 12) * sqrt(34 - T) + rnorm(length(T), 0, 1e-4)
  tab <- gdd_compare(data.frame(temp = T, duration = 1 / V))
  expect_s3_class(tab, "data.frame")
  expect_true(all(c("model", "AICc", "delta_AICc", "best") %in% names(tab)))
  expect_equal(sum(tab$best), 1)
  expect_equal(tab$note[tab$model == "wang"], "insufficient n")
})

test_that("predict works for nonlinear models", {
  set.seed(3)
  a <- 2e-4; T0 <- 10; Tm <- 35
  T <- seq(15, 30, by = 2.5)
  V <- a * T * (T - T0) * sqrt(Tm - T) + rnorm(length(T), 0, 1e-5)
  fit <- gdd_calc(data.frame(temp = T, duration = 1 / V),
                  model = "briere1")
  pr <- gdd_predict(fit, temp = 22)
  expect_equal(pr$pred_rate, a * 22 * 12 * sqrt(13), tolerance = 1e-3)
  expect_equal(pr$pred_duration, 1 / (a * 22 * 12 * sqrt(13)),
               tolerance = 1e-3)
})

test_that("gdd_check flags invalid rows", {
  df <- data.frame(temp = c(20, "abc", 30),
                   duration = c(10, 9, -1),
                   stringsAsFactors = FALSE)
  chk <- gdd_check(df)
  expect_equal(which(!chk$valid), c(2L, 3L))
  expect_equal(nrow(chk$problems), 2)
  expect_setequal(chk$problems$reason, c("non-numeric", "value <= 0"))
})

test_that("gdd_check detects a high-temperature decline and passes clean data", {
  # Max mean rate at 25 deg C although 30 deg C was measured -> decline
  df <- data.frame(temp    = c(20, 20, 25, 25, 30, 30),
                   duration = c(10, 11, 8, 8.5, 9, 9.2))
  chk <- gdd_check(df)
  expect_true(all(chk$valid))
  expect_true(chk$linear_check$rate_declines)
  expect_equal(chk$linear_check$temp_of_max_rate, 25)

  # Packaged example data: monotone increase, no decline
  chk2 <- gdd_check(df_ex, by = "stage")
  expect_true(all(chk2$valid))
  expect_false(any(chk2$linear_check$rate_declines))
  expect_equal(nrow(chk2$group_summary), 3)
})

test_that("gdd_daily is unchanged", {
  expect_equal(gdd_daily(5, 25, 10, "triangle")$daily, 225 / 40)
  ## avg method: (8+20)/2 - 11 = 3 on day 1, (10+22)/2 - 11 = 5 on day 2
  expect_equal(gdd_daily(c(8, 10), c(20, 22), 11)$total, 8)
})