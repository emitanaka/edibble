test_that("nested-units", {

  expect_snapshot({
    start_design(name = "nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2))
  })

  des1 <- start_design(name = "nested units") %>%
    set_units(block = 3,
              plot = nested_in(block, 2))

  des2 <- des1 %>%
    set_units(sample = nested_in(plot, 10))

  des3 <- des1 %>%
    set_units(sample = nested_in(plot,
                                 1 ~ 20,
                                 . ~ 3, leading0 = 3))

  expect_equal(nesting(des1), list(plot = "block"))
  expect_equal(nesting(des2), list(plot = "block", sample = "plot"))

})
