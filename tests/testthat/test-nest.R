test_that("nested-units", {

  expect_snapshot({
    des1 <- start_design(name = "nested units", seed = 1) %>%
      set_units(block = 3,
                plot = nested_in(block, 2))
    des1
  })

  expect_equal(fct_nodes(des1),
               data.frame(id = c(1L, 2L),
                          name = c("block", "plot"),
                          class = "edbl_unit",
                          n = c(3L, 6L)))
  expect_equal(lvl_nodes(des1),
               data.frame(idvar = rep(1:2, c(3, 6)),
                          id = 1:9,
                          name = c(paste0("block", 1:3), paste0("plot", 1:6)),
                          var = rep(c("block", "plot"), c(3, 6)),
                          label = c(paste0("block", 1:3), paste0("plot", 1:6))))

  des2 <- des1 %>%
    set_units(sample = nested_in(plot,
                                 1 ~ 20,
                                 . ~ 3, leading0 = 3))
  expect_equal(fct_nodes(des2),
               data.frame(id = c(1L, 2L, 3L),
                          name = c("block", "plot", "sample"),
                          class = "edbl_unit",
                          n = c(3L, 6L, 35L)))
  expect_equal(lvl_nodes(des2),
               data.frame(idvar = rep(1:3, c(3, 6, 35)),
                          id = 1:44,
                          name = c(paste0("block", 1:3), paste0("plot", 1:6), sprintf("sample%.3d", 1:35)),
                          var = rep(c("block", "plot", "sample"), c(3, 6, 35)),
                          label = c(paste0("block", 1:3), paste0("plot", 1:6), sprintf("sample%.3d", 1:35))))



})

test_that("nesting-structure", {
  des1 <- start_design(name = "nested units", seed = 1) %>%
    set_units(block = 3,
              plot = nested_in(block, 2))
  des2 <- des1 %>%
    set_units(sample = nested_in(plot, 10))

  expect_equal(nesting_structure(des1), list(plot = "block"))
  expect_equal(nesting_structure(des2), list(plot = "block", sample = "plot"))

})

