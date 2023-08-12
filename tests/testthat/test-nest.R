test_that("nested-units", {

  expect_snapshot({
    des1 <- design(name = "nested units", seed = 1) %>%
      set_units(block = 3,
                plot = nested_in(block, 2))
    des1
  })

  expect_equal(fct_nodes(des1),
               tibble::tibble(id = c(1L, 2L),
                              role = "edbl_unit",
                              name = c("block", "plot"),
                              attrs = NA))
  expect_equal(lvl_nodes(des1),
               structure(list(`1` = tibble::tibble(id = 1:3,
                                                   value = c("block1", "block2", "block3")),
                              `2` = tibble::tibble(id = 4:9,
                                                   value = c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6"))),
                         class = c("edbl_lnodes", "list")))

  des2 <- des1 %>%
    set_units(sample = nested_in(plot,
                                 1 ~ 20,
                                 . ~ 3))
  expect_equal(fct_nodes(des2),
               tibble::tibble(id = c(1L, 2L, 3L),
                              role = "edbl_unit",
                              name = c("block", "plot", "sample"),
                              attrs = NA))
  expect_equal(lvl_nodes(des2),
               structure(list(`1` = tibble::tibble(id = 1:3,
                                                   value = c("block1", "block2", "block3")),
                              `2` = tibble::tibble(id = 4:9,
                                                   value = c("plot1", "plot2", "plot3", "plot4", "plot5", "plot6")),
                              `3` = tibble::tibble(id = 10:44,
                                                   value = c("sample01", "sample02", "sample03",
                                                             "sample04", "sample05", "sample06", "sample07", "sample08",
                                                             "sample09", "sample10", "sample11", "sample12", "sample13",
                                                             "sample14", "sample15", "sample16", "sample17", "sample18",
                                                             "sample19", "sample20", "sample21", "sample22", "sample23",
                                                             "sample24", "sample25", "sample26", "sample27", "sample28",
                                                             "sample29", "sample30", "sample31", "sample32", "sample33",
                                                             "sample34", "sample35"))),
                          class = c("edbl_lnodes", "list")))



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

