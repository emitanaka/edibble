test_that("split_by works", {
  crd <- design("Completely Randomised Design", seed = 1) %>%
    set_units(unit = 10) %>%
    set_trts(trt = 2) %>%
    allot_trts(trt ~ unit) %>%
    assign_trts("random", seed = 906) %>%
    serve_table()

  expect_equal(split_by(crd, trt), list(trt1 = tibble::tibble(unit = paste0("unit", c(2, 4, 5, 9, 10))),
                                        trt2 = tibble::tibble(unit = paste0("unit", c(1, 3, 6, 7, 8)))),
               ignore_attr = TRUE)

  spd <- takeout(menu_split(t1 = 2, t2 = 3, r = 2, seed = 1))
  expect_equal(split_by(spd, trt1), list(trt11 = tibble::tibble(mainplot = c("mainplot2", "mainplot3")),
                                         trt12 = tibble::tibble(mainplot = c("mainplot1", "mainplot4"))),
               ignore_attr = TRUE)
  expect_equal(split_by(spd, trt2), list(trt21 = tibble::tibble(subplot = c("subplot01", "subplot04", "subplot07", "subplot12"),
                                                                mainplot = c("mainplot1", "mainplot2", "mainplot3", "mainplot4")),
                                         trt22 = tibble::tibble(subplot = c("subplot03", "subplot05", "subplot09", "subplot10"),
                                                                mainplot = c("mainplot1", "mainplot2", "mainplot3", "mainplot4")),
                                         trt23 = tibble::tibble(subplot = c("subplot02", "subplot06", "subplot08", "subplot11"),
                                                                mainplot = c("mainplot1", "mainplot2", "mainplot3", "mainplot4"))),
               ignore_attr = TRUE)
  expect_equal(split_by(spd, trt1, trt2), list("trt11:trt21" = tibble::tibble(subplot = c("subplot04", "subplot07"),
                                                                              mainplot = c("mainplot2", "mainplot3")),
                                               "trt12:trt21" = tibble::tibble(subplot = c("subplot01", "subplot12"),
                                                                              mainplot = c("mainplot1", "mainplot4")),
                                               "trt11:trt22" = tibble::tibble(subplot = c("subplot05", "subplot09"),
                                                                              mainplot = c("mainplot2", "mainplot3")),
                                               "trt12:trt22" = tibble::tibble(subplot = c("subplot03", "subplot10"),
                                                                              mainplot = c("mainplot1", "mainplot4")),
                                               "trt11:trt23" = tibble::tibble(subplot = c("subplot06", "subplot08"),
                                                                              mainplot = c("mainplot2", "mainplot3")),
                                               "trt12:trt23" = tibble::tibble(subplot = c("subplot02", "subplot11"),
                                                                              mainplot = c("mainplot1", "mainplot4"))),
               ignore_attr = TRUE)
  expect_error(split_by(spd, subplot, trt1))
  expect_equal(split_by(spd, mainplot), list(mainplot1 = tibble::tibble(subplot = c("subplot01", "subplot02", "subplot03"),
                                                                        trt2 = c("trt21", "trt23", "trt22")),
                                             mainplot2 = tibble::tibble(subplot = c("subplot04", "subplot05", "subplot06"),
                                                                        trt2 = c("trt21", "trt22", "trt23")),
                                             mainplot3 = tibble::tibble(subplot = c("subplot07", "subplot08", "subplot09"),
                                                                        trt2 = c("trt21", "trt23", "trt22")),
                                             mainplot4 = tibble::tibble(subplot = c("subplot10", "subplot11", "subplot12"),
                                                                        trt2 = c("trt22", "trt23", "trt21"))),
               ignore_attr = TRUE)
  expect_error(split_by(spd, subplot))

})

