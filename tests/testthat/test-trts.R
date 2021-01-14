test_that("treatments", {
  expect_snapshot_output({
    set.seed(1)
    start_design() %>%
      set_units(mainplot = 4,
                subplot = nested_in(mainplot, 4)) %>%
      set_trts(var = 2,
               irr = 2) %>%
      allocate_trts(var ~ subplot,
                    irr ~ mainplot) %>%
      randomise_trts() %>%
      serve_table()
  })

  expect_snapshot_output({
    set.seed(1)
    start_design() %>%
      set_units(mainplot = 4,
                subplot = nested_in(mainplot, 4)) %>%
      set_trts(var = 2,
               irr = 2) %>%
      allocate_trts( ~ subplot) %>%
      randomise_trts() %>%
      serve_table()
  })
})
