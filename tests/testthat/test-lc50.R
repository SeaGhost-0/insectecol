example_csv <- system.file("extdata", "lc50_example.csv", package = "insectecol")

test_that("Abbott 校正、LC 估计与 delta 法置信区间符合闭式解", {
  lcd <- suppressMessages(lc50_read(example_csv))
  res <- suppressMessages(lc50_calculate(lcd, method = "all"))
  s <- res$summary_df
  expect_equal(s$Valid_groups, c(6L, 6L, 6L))     # 7 组去掉对照
  expect_equal(s$Dropped_groups, c(0L, 0L, 0L))

  prep <- res$results[[1]]$traditional$prep
  expect_equal(attr(prep, "pc"), 1 / 30)          # 对照死亡率 1/30
  expect_equal(attr(prep, "p")[1], (4 / 30 - 1 / 30) / (1 - 1 / 30),
               tolerance = 1e-12)                 # Abbott 公式

  # 三种方法互相吻合（示例数据的核对值）
  expect_equal(s$Estimate[1], 0.2065, tolerance = 1e-3)
  expect_equal(s$Estimate[2], 0.2077, tolerance = 1e-3)
  expect_equal(s$Estimate[3], 0.2075, tolerance = 1e-3)

  # delta 法 CI 在 lg(C) 尺度上对称，反变换后在浓度尺度上右偏
  for (key in c("traditional", "improved", "probit")) {
    r <- res$results[[1]][[key]]
    expect_equal(log10(r$upper) - log10(r$estimate),
                 log10(r$estimate) - log10(r$lower), tolerance = 1e-10)
    expect_true(r$lower < r$estimate && r$estimate < r$upper)
    # 把 LC 代回回归式必须回到 probit 5
    expect_equal(r$intercept + r$slope * log10(r$estimate), 5, tolerance = 1e-10)
  }
})

test_that("lc50_analyze() 的 plot_file 参数导出 png", {
  f <- tempfile(fileext = ".png")
  out <- suppressMessages(lc50_analyze(lc50_read(example_csv),
                                       method = "probit",
                                       plot = TRUE, plot_file = f))
  expect_true(file.exists(f))
  expect_true(file.size(f) > 0)
  expect_equal(unname(out$plot_file), f)
})
