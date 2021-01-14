test_that("set_units", {

  expect_snapshot_output({
    start_design(name = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  expect_snapshot_output({
    start_design(name = "unlinked units with table") %>%
      set_units(block = 3,
                plot = 2) %>%
      serve_table()
  })

  expect_snapshot_output({
    start_design(name = "nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2)) %>%
      serve_table()
  })

  expect_snapshot_output({
    start_design(name = "check SI prefix") %>%
      set_units(block = 3,
                plot = nested_in(block, 200),
                plant = nested_in(plot, 5)) %>%
      serve_table()
  })

  df <- start_design(name = "complex nesting") %>%
      set_units(block = 3,
                plot = nested_in(block,
                                  1 ~ 10,
                                  . ~ 20),
                plant = nested_in(plot, 5)) %>%
      serve_table()

  expect_equal(length(unique(df$plot)), 10 + 20 *2)
  expect_equal(nrow(df), (10 + 20 * 2) * 5)
  # need vec cast edbl_unit..
  expect_equal(length(unique(df$plot[as.character(df$block)=="block1"])), 10)
  expect_equal(length(unique(df$plot[as.character(df$block)=="block2"])), 20)
  expect_equal(length(unique(df$plot[as.character(df$block)=="block3"])), 20)


  df <- start_design(name = "complex nesting with labels") %>%
    set_units(block = c("A", "B", "C", "D"),
              plot = nested_in(block,
                               c("A", "B") ~ 10,
                                         . ~ 20),
              plant = nested_in(plot, 5)) %>%
    serve_table()

  expect_equal(length(unique(df$plot)), 10 * 2 + 20 *2)
  expect_equal(nrow(df), (10 * 2 + 20 * 2) * 5)
  expect_equal(length(unique(df$plot[as.character(df$block)=="A"])), 10)
  expect_equal(length(unique(df$plot[as.character(df$block)=="B"])), 10)
  expect_equal(length(unique(df$plot[as.character(df$block)=="C"])), 20)
  expect_equal(length(unique(df$plot[as.character(df$block)=="D"])), 20)


})
