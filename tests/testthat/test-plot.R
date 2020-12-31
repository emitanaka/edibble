test_that("plot", {
  initiate_design() %>%
    set_units(mainplot = 4,
              subplot = nested_in(mainplot, 2)) %>%
    plot()

  initiate_design() %>%
    set_units(mainplot = 4,
              subplot = nested_in(mainplot, 2)) %>%
    plot(type = "level")

})
