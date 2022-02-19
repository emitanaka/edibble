test_that("set_units", {

  expect_snapshot({
    design(name = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  des <- design(name = "unlinked units") %>%
    set_units(block = 3,
              plot = 2)

  expect_equal(nrow(des$graph$nodes), 2)
  expect_equal(names(des$graph$nodes), c("id", "name", "class"))
  expect_equal(nrow(des$graph$levels$nodes), 5)
  expect_equal(des$graph$nodes$id, c(1L, 2L))
  expect_equal(des$graph$nodes$name, c("block", "plot"))
  expect_equal(des$graph$nodes$class, c("edbl_unit", "edbl_unit"))
  expect_equal(des$graph$levels$nodes$idvar, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(des$graph$levels$nodes$id, 1:5)
  expect_equal(des$graph$levels$nodes$name, c("block1", "block2", "block3", "plot1", "plot2"))
  expect_equal(names(des$graph$levels$nodes), c("idvar", "id", "name", "var", "label"))


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
