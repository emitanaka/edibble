test_that("set_units", {

  expect_snapshot_output({
    initiate_design(name = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  expect_snapshot_output({
    initiate_design(name = "unlinked units with table") %>%
      set_units(block = 3,
                plot = 2) %>%
      serve_table()
  })

  expect_snapshot_output({
    initiate_design(name = "supplied levels") %>%
      set_units(block = c("A", "B", "C"),
                plot = 3) %>%
      pull(block) %>%
      levels()
  })

  expect_snapshot_output({
    initiate_design(name = "nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2)) %>%
      serve_table()
  })

  expect_snapshot_output({
    initiate_design(name = "more nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 200),
                plant = nested_in(plot, 5)) %>%
      serve_table()
  })


})
