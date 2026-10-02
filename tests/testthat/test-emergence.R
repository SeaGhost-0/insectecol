## Tianyang overwintering-generation case: 40 individuals, 10 stages,
## days-to-eclosion 0/2/4/.../18; survey 2026-03-20
ty <- data.frame(
  stage = c("Pupal exuviae", paste("Pupa", 7:1), "Prepupa", "Larva 5"),
  count = c(2, 3, 5, 7, 7, 5, 3, 4, 2, 2),
  days  = seq(0, 18, 2))
SD <- "2026-03-20"

test_that("cumulative development is computed correctly", {
  fit <- emergence_calc(ty, survey_date = SD)
  expect_equal(fit$table$cumulative,
               c(0.05, 0.125, 0.25, 0.425, 0.6, 0.725,
                 0.8, 0.9, 0.95, 1))
  expect_equal(fit$n, 40)
  expect_equal(fit$table$eclosion_date[1], as.Date(SD))
  expect_equal(fit$table$eclosion_date[10], as.Date("2026-04-07"))
})

test_that("the 16/50/84 quantiles interpolate correctly", {
  fit <- emergence_calc(ty, survey_date = SD)
  expect_equal(fit$predictions$days, c(2.56, 6 + 0.075 / 0.175 * 2, 12.8),
               tolerance = 1e-12)
  expect_equal(fit$predictions$date,
               as.Date(c("2026-03-22", "2026-03-26", "2026-04-01")))
  expect_equal(fit$predictions$label,
               c("Beginning (16%)", "Peak (50%)", "End (84%)"))
})

test_that("row order does not matter (rows are sorted by days)", {
  set.seed(1)
  fit <- emergence_calc(ty[sample(10), ], survey_date = SD)
  expect_equal(fit$predictions$days, c(2.56, 6 + 0.075 / 0.175 * 2, 12.8),
               tolerance = 1e-12)
})

test_that("percent input matches count input", {
  dp <- data.frame(stage = ty$stage, percent = ty$count / 40 * 100,
                   days = ty$days)
  fit <- emergence_calc(dp, survey_date = SD)
  expect_equal(fit$predictions$days,
               c(2.56, 6 + 0.075 / 0.175 * 2, 12.8), tolerance = 1e-12)
  expect_true(is.na(fit$n))
})

test_that("hatch projection adds pre-oviposition and egg duration", {
  fit <- emergence_calc(ty, survey_date = SD, pre_ovip = 3, egg_days = 10)
  expect_equal(fit$predictions$hatch_date,
               as.Date(c("2026-04-04", "2026-04-08", "2026-04-14")))
  expect_equal(predict(fit, c(0.25, 0.75), event = "hatch")$date,
               as.Date(c("2026-04-06", "2026-04-12")))
})

test_that("quantiles outside the surveyed range extrapolate with a warning", {
  ## p below the share of the most developed stage (5%): the quantile
  ## eclosed before the survey -> extrapolated backwards
  expect_warning(fit2 <- emergence_calc(ty, survey_date = SD, p = 0.02),
                 "extrapolated backwards")
  expect_equal(fit2$predictions$days, -(0.05 - 0.02) / 0.075 * 2,
               tolerance = 1e-12)
  ## a truncated table (first 6 stages only) is renormalised, so the
  ## default quantiles stay inside the surveyed range
  d6 <- ty[1:6, ]                       # counts 2,3,5,7,7,5 -> sum 29
  fit <- emergence_calc(d6, survey_date = SD)
  expect_equal(fit$predictions$days[3],
               8 + (0.84 - 24 / 29) / (5 / 29) * 2, tolerance = 1e-12)
})

test_that("input validation catches impossible data", {
  expect_error(emergence_calc(ty, survey_date = SD, p = 1), "between 0 and 1")
  expect_error(emergence_calc(ty, survey_date = SD, pre_ovip = -1),
               "non-negative")
  expect_error(emergence_calc(data.frame(stage = "Pupa", count = 5,
                                         days = 3),
                              survey_date = SD), "At least two stages")
  dbad <- ty; dbad$days[2] <- -2
  expect_error(emergence_calc(dbad, survey_date = SD), "non-negative")
  expect_error(emergence_calc(ty, survey_date = "not a date"), "survey_date")
})

test_that("zero-count stages are dropped, missing rows are dropped", {
  dz <- rbind(ty, data.frame(stage = "Pupa 0", count = 0, days = 30))
  fit <- emergence_calc(dz, survey_date = SD)
  expect_equal(nrow(fit$table), 10)
  expect_equal(fit$predictions$days, c(2.56, 6 + 0.075 / 0.175 * 2, 12.8),
               tolerance = 1e-12)
})

test_that("predict works for arbitrary quantiles", {
  fit <- emergence_calc(ty, survey_date = SD)
  pr <- predict(fit, c(0.25, 0.75))
  expect_equal(pr$days,
               c(4, 10 + (0.75 - 0.725) / 0.075 * 2), tolerance = 1e-12)
  expect_equal(pr$date, as.Date(c("2026-03-24", "2026-03-30")))
})

test_that("the example csv is read and analysed end to end", {
  f <- system.file("extdata", "emergence_example.csv",
                   package = "insectecol")
  d <- emergence_read(f)
  expect_equal(nrow(d), 10)
  expect_equal(names(d), c("stage", "count", "days"))
  out <- emergence_analyze(path = f, survey_date = SD)
  expect_equal(out$fit$predictions$days,
               c(2.56, 6 + 0.075 / 0.175 * 2, 12.8), tolerance = 1e-12)
  out2 <- emergence_analyze(stage = d$stage, count = d$count,
                            days = d$days, survey_date = SD)
  expect_equal(out2$fit$predictions, out$fit$predictions)
  out3 <- emergence_analyze(data = d, survey_date = SD,
                            pre_ovip = 3, egg_days = 10)
  expect_equal(out3$fit$predictions$hatch_date,
               as.Date(c("2026-04-04", "2026-04-08", "2026-04-14")))
})

test_that("Chinese column aliases are detected", {
  dc <- data.frame(ty$stage, ty$count, ty$days, check.names = FALSE)
  names(dc) <- c("\u866b\u6001", "\u6570\u91cf",
                 "\u8ddd\u7fbd\u5316\u5929\u6570")
  fit <- emergence_calc(dc, survey_date = SD)
  expect_equal(fit$predictions$days, c(2.56, 6 + 0.075 / 0.175 * 2, 12.8),
               tolerance = 1e-12)
})

test_that("export writes csv and xlsx", {
  fit <- emergence_calc(ty, survey_date = SD, pre_ovip = 3, egg_days = 10)
  f1 <- tempfile(fileext = ".csv")
  emergence_export(fit, f1)
  expect_true(file.exists(f1))
  expect_true(file.exists(sub("\\.csv$", "_stages.csv", f1)))
  f2 <- tempfile(fileext = ".xlsx")
  skip_if_not_installed("writexl")
  emergence_export(fit, f2)
  expect_true(file.exists(f2))
})

test_that("the plot method draws without error", {
  fit <- emergence_calc(ty, survey_date = SD, pre_ovip = 3, egg_days = 10)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(fit), NA)
  expect_error(plot(fit, show_hatch = FALSE), NA)
})

test_that("emergence_export_plot writes a png", {
  fit <- emergence_calc(ty, survey_date = SD, pre_ovip = 3, egg_days = 10)
  f <- tempfile(fileext = ".png")
  out <- suppressMessages(emergence_export_plot(fit, f))
  expect_equal(out, f)
  expect_true(file.exists(f))
  expect_true(file.size(f) > 0)
})
