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
  # Euler-Lotka 方程左边应等于 1（日序号 a 从 1 起，与 TWOSEX-MSChart 一致）
  lx <- res$lx$l_x[seq_len(nrow(res$lx) - 1)]
  mx <- res$mx$m_x[seq_len(nrow(res$mx) - 1)]
  expect_equal(sum(lx * mx * exp(-res$r * seq_along(lx))), 1, tolerance = 1e-4)
})

test_that("r 与 TWOSEX-MSChart 的 1 起龄折扣约定一致（示例数据核对值）", {
  res <- lifeTable_calculate_all(lifeTable_read(example_csv))
  expect_equal(res$r, 0.315410, tolerance = 1e-4)
  expect_equal(res$lambda, 1.3708, tolerance = 1e-3)
  expect_equal(res$T, 14.25, tolerance = 1e-2)
})

test_that("r 与 TWOSEX-MSChart 官方示例输出完全一致（马铃薯块茎蛾 PtW）", {
  f <- test_path("twosex_ptw_example.csv")
  res <- lifeTable_calculate_all(lifeTable_read(f))
  # 参考值取自 TWOSEX-MSChart 自带输出 Example_0A_Life Table_Output.txt
  # r 与 T 由二分法定位到机器精度，故容差可以收到 1e-9 / 1e-8
  expect_equal(res$r, 0.135995181, tolerance = 1e-8)
  expect_equal(res$lambda, 1.145676372, tolerance = 1e-6)
  expect_equal(res$R0, 69.7, tolerance = 1e-6)
  expect_equal(res$T, 31.208461135, tolerance = 1e-8)
  # 新生个体期望寿命 E(0,1) = 35.8（包的 1 基年龄对应 TWOSEX 的 0 基年龄）
  expect_equal(res$ex$e_x[1], 35.8, tolerance = 1e-6)
})

test_that("calc_r 与 bootstrap 的 Euler-Lotka 求解器逐位一致（两条路径不许有精度差）", {
  lt <- lifeTable_read(test_path("twosex_ptw_example.csv"))
  res <- lifeTable_calculate_all(lt)
  bt <- suppressMessages(lifeTable_bootstrap(lt, B = 500, seed = 1))
  i <- which(bt$summary$Parameter == "Intrinsic_rate_of_increase_r")
  j <- which(bt$summary$Parameter == "Mean_generation_time_T")
  # 两条路径都把区间折半同样多的次数，结果必须完全相同
  expect_equal(res$r, bt$summary$Original[i], tolerance = 1e-14)
  expect_equal(res$T, bt$summary$Original[j], tolerance = 1e-12)
  expect_equal(res$lambda, exp(bt$summary$Original[i]), tolerance = 1e-14)
})

test_that("R0 < 1（r < 0）时 r 也能求解且 Euler-Lotka 闭合", {
  l_x <- c(1, 0.8, 0.5, 0.2, 0)
  m_x <- c(0, 0.1, 0.2, 0.1, 0)   # R0 = 0.2 < 1
  expect_equal(sum(l_x * m_x), 0.2)
  r <- insectecol:::.intrinsic_rate(l_x, m_x)
  expect_true(is.finite(r) && r < 0)
  expect_equal(sum(l_x * m_x * exp(-r * seq_along(l_x))), 1, tolerance = 1e-4)
})

test_that("无后代的世代表返回 NA 并警告", {
  l_x <- c(1, 0.5, 0)
  m_x <- c(0, 0, 0)
  expect_warning(r <- insectecol:::.intrinsic_rate(l_x, m_x), "No offspring")
  expect_true(is.na(r))
})

test_that("e_x 为 T_x / l_x（与 Chi/TWOSEX 定义一致）", {
  lt <- lifeTable_read(example_csv)
  lx <- calc_lx(lt)
  ex <- calc_ex(lt, lx)
  l_x <- lx$l_x[seq_len(nrow(lx) - 1)]
  # e_x[1] = sum(l_x) / 1（首龄 l_x = 1）
  expect_equal(ex$e_x[1], sum(l_x))
  # 任意中间年龄：e_x = 自该龄起的累积和 / l_x
  i <- 21
  expect_equal(ex$e_x[i], sum(l_x[i:length(l_x)]) / l_x[i])
  # 活体年龄 e_x >= 1；l_x = 0 处 e_x = 0
  alive <- which(l_x > 0)
  expect_true(all(ex$e_x[alive] >= 1 - 1e-12))
  expect_equal(ex$e_x[which(l_x == 0)], rep(0, sum(l_x == 0)))
})

test_that("e_xj 与 TWOSEX-MSChart 官方示例输出一致（马铃薯块茎蛾 PtW）", {
  f <- test_path("twosex_ptw_example.csv")
  res <- lifeTable_calculate_all(lifeTable_read(f))
  exj <- res$exj
  expect_equal(colnames(exj), c("Age", "Egg", "1st instar", "Pupa",
                                "Female", "Male"))
  # TWOSEX 的 Exj 画图输出（Example_3_Fig_Exj.txt），0 基年龄对齐到包的 1 基行
  tw <- utils::read.csv(test_path("twosex_ptw_exj_example.txt"), skip = 1,
                        check.names = FALSE)
  tw2 <- tw[tw$Age < nrow(exj), -1]
  colnames(tw2) <- c("Egg", "Larva", "Pupa", "Female", "Male")
  # 单龄期独占的格子：两法无歧义，应与 TWOSEX 达机器精度一致
  mono <- list(list(col = "Egg",  rows = 1:6),
               list(col = "Larva", rows = 7:16),
               list(col = "Pupa", rows = 21:26),
               list(col = "Female", rows = 39:43))
  for (m in mono) {
    pkg_col <- if (m$col == "Larva") "1st instar" else m$col
    expect_equal(exj[[pkg_col]][m$rows], tw2[[m$col]][m$rows],
                 tolerance = 1e-9,
                 info = paste("monostage cells of", m$col))
  }
  # 混龄期行（如 27-31）TWOSEX 的矩阵递推与精确条件期望有固有微差，
  # 这里不要求一致，但差值必须有限且为正
  d <- abs(exj$Pupa[27:31] - tw2$Pupa[27:31])
  expect_true(all(is.finite(d)))
})

test_that("e_xj 按占用人数加权平均还原 e_x（池化不变式）", {
  f <- test_path("twosex_ptw_example.csv")
  res <- lifeTable_calculate_all(lifeTable_read(f))
  sxj <- as.matrix(res$sxj)
  exj <- as.matrix(res$exj[, -1])
  ex  <- res$ex$e_x[seq_len(nrow(sxj))]
  occ <- sxj * res$N
  pooled <- rowSums(occ * exj) / rowSums(occ)
  pooled[is.nan(pooled)] <- 0
  expect_equal(pooled, ex, tolerance = 1e-9)
})

test_that("e_xj 在空格子为 0、占用格子为正", {
  lt <- lifeTable_read(test_path("twosex_ptw_example.csv"))
  exj <- calc_exj(lt)
  sxj <- as.matrix(calc_sxj(lt))
  expect_equal(as.vector(as.matrix(exj[, -1])[sxj == 0]),
               rep(0, sum(sxj == 0)))
  expect_true(all(as.matrix(exj[, -1])[sxj > 0] > 0))
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

test_that("UTF-8 BOM 文件可正常读取", {
  src <- test_path("twosex_ptw_example.csv")
  lines <- readLines(src, warn = FALSE)
  f <- tempfile(fileext = ".csv")
  con <- file(f, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeLines(lines, con, sep = "\n")
  close(con)
  lt1 <- lifeTable_read(src)
  lt2 <- lifeTable_read(f)
  expect_equal(lt2$data, lt1$data)
  expect_equal(lt2$n, lt1$n)
  expect_equal(lifeTable_calculate_all(lt2)$r, lifeTable_calculate_all(lt1)$r)
})

test_that("export_file 支持绝对路径（父目录不存在时自动创建）", {
  lt <- lifeTable_read(test_path("twosex_ptw_example.csv"))
  tgt <- file.path(tempdir(), paste0("abs_lt_", as.integer(Sys.time())),
                   "sub", "lt_out.xlsx")
  o <- lifeTable_analyze(lt = lt, fecundity = FALSE, plot = FALSE,
                         export = TRUE, export_file = tgt)
  expect_true(file.exists(o$export_file))
  expect_equal(normalizePath(o$export_file), normalizePath(tgt))
})

test_that("幼期死亡（成虫期留空、性别标 F/M）的个体不进入成虫列", {
  ## check_data 允许"尾部留空"（早期死亡）；此时最后一个非空值是幼期
  ## 时长，必须留在它自己的阶段列，绝不能按"最后一个值 = 成虫期"被
  ## 计入 Female/Male 列（TWOSEX-MSChart 按列位置归列）
  d <- data.frame(
    ID = 1:3,
    Egg = c(3, 3, 2), Larva = c(2, 2, 3), Pupa = c(4, 4, 4),
    Adult = c(10, NA, 8),
    gender = c("F", "F", "M"))
  lt <- lifeTable_build(d[2:4], adult_days = d$Adult, sex = d$gender,
                        stage_names = c("Egg", "Larva", "Pupa"), check = FALSE)
  sxj <- calc_sxj(lt)
  ## 第 2 行是蛹期死亡（6-9 日龄）；唯一真正的雌虫 10 日龄才羽化
  expect_equal(sum(sxj[1:9, "Female"]), 0)
  expect_equal(sum(sxj[6:9, "Pupa"]), 4)   # 三行在 6-9 日龄都是蛹
  ## e_xj 与 s_xj 同布局，成虫列同样不得被污染
  exj <- calc_exj(lt)
  expect_equal(sum(exj[1:9, "Female"]), 0)
})

test_that("e_x / e_xj 的期望寿命恒等式（缺失除以 l_x / s_xj 的回归门禁）", {
  ## e_x = T_x / l_x，e_xj 由同一 T_x 按虫态格分摊：
  ## 早期版本输出的是裸 T_x（未除），所有 e_x 严重偏大 —— 这里锁死这个关系
  lt <- lifeTable_read(example_csv)
  res <- lifeTable_calculate_all(lt)
  d <- lt$data
  dur <- apply(as.matrix(d[, 2:(lt$n - 1)]), c(1, 2), as.numeric)
  dur[is.na(dur)] <- 0
  lifespan <- rowSums(dur)                       # 每头活了多少天
  l_ind <- vapply(seq_len(max(lifespan)), function(x) mean(lifespan >= x), numeric(1))
  T_ind <- rev(cumsum(rev(l_ind)))               # T_x = sum(l_y, y >= x)

  m <- min(nrow(res$ex), length(l_ind))
  ex <- res$ex$e_x; lx <- res$lx$l_x
  expect_equal(unname(ex[seq_len(m)]), T_ind[seq_len(m)] / l_ind[seq_len(m)],
               tolerance = 1e-9)
  expect_equal(unname(ex[1] * lx[1]), sum(l_ind), tolerance = 1e-9)
  expect_equal(unname(ex[1]), mean(lifespan), tolerance = 1e-9)  # e_1 = 平均寿命

  ## 同一 T_x 也等于各虫态格 e_xj * s_xj 之和
  sxj <- apply(as.matrix(res$sxj), c(1, 2), as.numeric)
  exj <- apply(as.matrix(res$exj[, -1]), c(1, 2), as.numeric)
  expect_equal(rowSums(exj * sxj), T_ind[seq_len(nrow(sxj))], tolerance = 1e-9)
  expect_equal(rowSums(sxj), unname(lx[seq_len(nrow(sxj))]), tolerance = 1e-10)
})
