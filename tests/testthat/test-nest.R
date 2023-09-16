test_that("nested-units", {

  expect_snapshot({
    des1 <- design(name = "nested units", seed = 1) %>%
      set_units(block = 3,
                plot = nested_in(block, 2))
    des1
  })

  expect_equal(fct_nodes(des1),
               tibble::tibble(name = c("block", "plot"),
                              role = "edbl_unit"))
  # FIXME there is no attrs for the second factor
  expect_equal(lvl_nodes(des1),
               list(block = tibble::tibble(value = c("block1", "block2", "block3"),
                                           n = NA_integer_),
                    plot = tibble::tibble(value = c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6"),
                                          n = NA_integer_))                         )

  des2 <- des1 %>%
    set_units(sample = nested_in(plot,
                                 1 ~ 20,
                                 . ~ 3))
  expect_equal(fct_nodes(des2),
               tibble::tibble(name = c("block", "plot", "sample"),
                              role = "edbl_unit"))
  expect_equal(lvl_nodes(des2),
               list(block = tibble::tibble(value = c("block1", "block2", "block3"),
                                           n = NA_integer_),
                              plot = tibble::tibble(value = c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6"),
                                                    n = NA_integer_),
                              sample = tibble::tibble(value = c("sample01", "sample02", "sample03",
                                                             "sample04", "sample05", "sample06", "sample07", "sample08",
                                                             "sample09", "sample10", "sample11", "sample12", "sample13",
                                                             "sample14", "sample15", "sample16", "sample17", "sample18",
                                                             "sample19", "sample20", "sample21", "sample22", "sample23",
                                                             "sample24", "sample25", "sample26", "sample27", "sample28",
                                                             "sample29", "sample30", "sample31", "sample32", "sample33",
                                                             "sample34", "sample35"),
                                                      n = NA_integer_)))



})

test_that("nesting-structure", {
  des1 <- design(name = "nested units", seed = 1) %>%
    set_units(block = 3,
              plot = nested_in(block, 2))
  des2 <- des1 %>%
    set_units(sample = nested_in(plot, 10))

  expect_equal(nesting_structure(des1), list(plot = "block"))
  expect_equal(nesting_structure(des2), list(plot = "block", sample = "plot"))

})


test_that("conditioning-structure", {
  cond1 <- design("Completely Randomised Design") %>%
    set_units(unit = 26) %>%
    set_trts(trt1 = 2,
             trt2 = conditioned_on(trt1,
                                   1 ~ "1",
                                   2 ~ c("2", "3"))) %>%
    allot_trts(trt1:trt2 ~ unit) %>%
    assign_trts("random", seed = 554) %>%
    serve_table()

  count_by(cond1, trt1, trt2)


  cond2 <- design("Completely Randomised Design") %>%
    set_units(unit = 26) %>%
    set_trts(trt1 = 2,
             trt2 = conditioned_on(trt1,
                                   1 ~ "1",
                                   2 ~ c("2", "3")),
             trt3 = 3) %>%
    allot_trts(trt1:trt2:trt3 ~ unit) %>%
    assign_trts("random", seed = 554) %>%
    serve_table()

  count_by(cond2, trt1, trt2, trt3)
})

