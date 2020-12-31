test_that("apply treatments", {
  initiate_design() %>%
    set_units(mainplot = 4,
              subplot = nested_in(mainplot, 2)) %>%
    set_trts(var = 2,
             irr = 2) %>%
    apply_trts(var ~ subplot,
               irr ~ mainplot) %>%
    plot()
})
