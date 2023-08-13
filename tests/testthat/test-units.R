test_that("set_units", {

  expect_snapshot({
    design(title = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  des <- design(title = "unlinked units") %>%
    set_units(block = 3,
              plot = 2)

  expect_equal(nrow(des$graph$factors$nodes), 2)
  expect_equal(names(des$graph$factors$nodes), c("id", "role", "name", "attrs"))
  expect_equal(des$graph$factors$nodes$id, c(1L, 2L))
  expect_equal(des$graph$factors$nodes$name, c("block", "plot"))
  expect_equal(des$graph$factors$nodes$role, c("edbl_unit", "edbl_unit"))
  expect_equal(des$graph$levels$nodes[["1"]]$id, 1:3)
  expect_equal(des$graph$levels$nodes[["2"]]$id, 4:5)
  expect_equal(des$graph$levels$nodes[["1"]]$value, c("block1", "block2", "block3"))
  expect_equal(des$graph$levels$nodes[["2"]]$value, c("plot1", "plot2"))
  expect_equal(names(des$graph$levels$nodes[["1"]]), c("id", "value"))


  expect_snapshot({
    design() %>%
      set_units(row = 3,
                col = 4,
                plot = ~row:col) %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_units(row = 3,
                col = 4,
                site = 4,
                plot = ~site:row:col) %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_units(site = 2,
                row = nested_in(site, 1 ~ 2,
                                      2 ~ 3),
                col = nested_in(site, 3),
                plot = ~site:row:col) %>%
      serve_table()
  })

})
