test_that("export with no record", {
  set.seed(1)
  des <- suppressMessages(takeout())
  fn <- tempfile()
  suppressMessages(export_design(des0, file = fn, overwrite = TRUE))
  dat <- openxlsx2::read_xlsx(fn, sheet = 2)

  expect_equal(dat, as_tibble(des), ignore_attr = TRUE)
})

test_that("export with record", {
  set.seed(1)
  des <- takeout(menu_split(t1 = 2, t2 = 3, r = 2)) %>%
    set_rcrds(yield = mainplot,
              height = subplot,
              genotype = subplot,
              yield_date = mainplot) %>%
    expect_rcrds(yield > 0,
                 #date = to_be_date(range = edibble::)
                 100 > height,
                 height > 0,
                 factor(genotype, levels = c("A", "B")))


  fn <- tempfile()
  suppressMessages(export_design(des, file = fn, overwrite = TRUE))
  dat <- openxlsx2::read_xlsx(fn, sheet = 2)
  expect_equal(dat, as_tibble(des), ignore_attr = TRUE)

  dat <- openxlsx2::read_xlsx(fn, sheet = "Data.mainplot")
  out1 <- data.frame(mainplot = c("mainplot1", "mainplot2", "mainplot3", "mainplot4"),
                    trt = c("trt11", "trt11", "trt12", "trt12"),
                    yield = NA_real_,
                    yield_date = NA_real_)
  expect_equal(out1, dat, ignore_attr = TRUE)

  dat <- openxlsx2::read_xlsx(fn, sheet = "Data.subplot")
  out2 <- data.frame(subplot = c("subplot01", "subplot02", "subplot03", "subplot04", "subplot05",
                                "subplot06", "subplot07", "subplot08", "subplot09", "subplot10",
                                "subplot11", "subplot12"),
                    mainplot = c("mainplot1", "mainplot1", "mainplot1", "mainplot2", "mainplot2",
                                 "mainplot2", "mainplot3", "mainplot3", "mainplot3", "mainplot4",
                                 "mainplot4", "mainplot4"),
                    trt2 = c("trt21", "trt22", "trt23", "trt22", "trt21", "trt23", "trt22",
                             "trt21", "trt23", "trt23", "trt22", "trt21"),
                    height = NA_real_,
                    genotype = NA_real_)
  expect_equal(out2, dat, ignore_attr = TRUE)

  # check hide_treatments
  suppressMessages(export_design(des, file = fn, overwrite = TRUE, hide_treatments = TRUE))
  dat <- openxlsx2::read_xlsx(fn, sheet = "Data.mainplot")
  expect_equal(out1[-2], dat, ignore_attr = TRUE)
  dat <- openxlsx2::read_xlsx(fn, sheet = "Data.subplot")
  expect_equal(out2[-3], dat, ignore_attr = TRUE)

  # check for Variables
  dat <- openxlsx2::read_xlsx(fn, sheet = "Variables")
  expect_equal(dat$variable, c("mainplot", "subplot", "trt1", "trt2", "yield", "height", "genotype",
                               "yield_date"))
  expect_equal(dat$type, c("unit", "unit", "trt", "trt", "rcrd", "rcrd", "rcrd", "rcrd"))
  expect_equal(dat$nlevels, c(4, 12, 2, 3, 4, 12, 12, 4))
  expect_equal(dat$record, c(NA, NA, NA, NA, "numeric", "numeric", "factor", NA))
  expect_equal(dat$value, c(NA, NA, NA, NA, "> 0", "between 0 and 100 inclusive", "A",
                            NA))
  expect_equal(dat[[6]], c(NA, NA, NA, NA, NA, NA, "B", NA))

})
