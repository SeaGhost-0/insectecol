example_csv <- system.file("extdata", "bioassay.csv", package = "insectecol")

test_that("lc50_analyze() 的 plot_file 参数导出 png", {
  f <- tempfile(fileext = ".png")
  out <- suppressMessages(lc50_analyze(lc50_read(example_csv),
                                       method = "probit",
                                       plot = TRUE, plot_file = f))
  expect_true(file.exists(f))
  expect_true(file.size(f) > 0)
  expect_equal(unname(out$plot_file), f)
})
