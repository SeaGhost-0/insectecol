# 测试开头：拿到示例数据路径（写一次，整个文件可用）
example_csv <- system.file("extdata", "lifetable_example.csv",
                           package = "insectecol")

test_that("lifeTable_read() 正确读取并定位性别列", {
  lt <- lifeTable_read(example_csv)
  expect_s3_class(lt, "life_table")
  expect_equal(lt$file_name, "lifetable_example")
  expect_identical(sort(unique(lt$data[[lt$n]])), c("F", "M", "N"))
})

test_that("合法数据通过 lifeTable_check()", {
  lt <- lifeTable_read(example_csv)
  expect_true(lifeTable_check(lt))
})

test_that("calc_N() 返回个体数", {
  lt <- lifeTable_read(example_csv)
  expect_equal(calc_N(lt), nrow(lt$data))
})

test_that("calc_sxj() 取值在 [0,1] 且列名与阶段名一致", {
  lt  <- lifeTable_read(example_csv)
  sxj <- calc_sxj(lt)
  expect_true(all(sxj >= 0 & sxj <= 1))
  expect_equal(ncol(sxj), length(get_stage_names(lt)))
})

test_that("l_x 从 1 开始、末行为 0", {
  lx <- calc_lx(lifeTable_read(example_csv))
  expect_equal(lx$l_x[1], 1)
  expect_equal(utils::tail(lx$l_x, 1), 0)
})

test_that("R0、r、lambda、T 相互一致（Euler-Lotka 方程闭合）", {
  res <- lifeTable_calculate_all(lifeTable_read(example_csv))
  expect_equal(res$lambda, exp(res$r))
  expect_equal(res$T, log(res$R0) / res$r)
  # Euler-Lotka 方程左边应等于 1（年龄 x 从 1 开始，与你的实现一致）
  lx <- res$lx$l_x[seq_len(nrow(res$lx) - 1)]
  mx <- res$mx$m_x[seq_len(nrow(res$mx) - 1)]
  x  <- seq_along(lx)
  expect_equal(sum(lx * mx * exp(-res$r * x)), 1, tolerance = 1e-4)
})

test_that("非法数据触发明确报错（报错信息测试）", {
  d <- utils::read.csv(example_csv)
  d[2, 2] <- "abc"                       # 注入一个非法字符
  bad <- file.path(tempdir(), "bad.csv")  # 只写进临时目录！
  utils::write.csv(d, bad, row.names = FALSE)
  expect_error(lifeTable_read(bad), "Age data errors")
})

test_that("lifeTable_calculate() 单文件模式完整跑通", {
  out <- file.path(tempdir(), "lt_out")
  df  <- lifeTable_calculate(example_csv, output_path = out,
                             plot = TRUE)   # 注意 plot = FALSE，原因见下
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true(file.exists(file.path(out, "all.xlsx")))
})

test_that("lifeTable_analyze() 的 plot_file 参数导出 png", {
  d <- read.csv(example_csv)
  f <- tempfile(fileext = ".png")
  out <- suppressMessages(lifeTable_analyze(
    stages = d[2:8], adult_days = d$Adult, sex = d$gender,
    oviposition = d[, 11:17], plot = TRUE, plot_file = f))
  expect_true(file.exists(f))
  expect_true(file.size(f) > 0)
  expect_equal(out$plot_file, f)
})
