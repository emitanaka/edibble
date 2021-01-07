test_that("apply treatments", {
  start_design() %>%
    set_units(mainplot = 4,
              subplot = nested_in(mainplot, 2)) %>%
    set_trts(var = 2,
             irr = 2) %>%
    allocate_trts(var ~ subplot,
                  irr ~ mainplot) %>%
    plot(view = "low")

  start_design() %>%
    set_units(mainplot = 4,
              subplot = nested_in(mainplot, 2)) %>%
    set_trts(var = 2,
             irr = 2) %>%
    allocate_trts(var ~ subplot,
                  irr ~ mainplot) %>%
    randomise_trts() %>%
    plot(view = "low")
})
