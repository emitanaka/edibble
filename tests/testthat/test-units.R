test_that("set_units", {

  expect_snapshot({
    start_design(name = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  des <- start_design(name = "unlinked units") %>%
    set_units(block = 3,
              plot = 2)

  expect_equal(nrow(des$vgraph$nodes), 2)
  expect_equal(names(des$vgraph$nodes), c("id", "label", "class"))
  expect_equal(nrow(des$lgraph$nodes), 5)
  expect_equal(des$vgraph$nodes$id, c(1L, 2L))
  expect_equal(des$vgraph$nodes$label, c("block", "plot"))
  expect_equal(des$vgraph$nodes$class, c("edbl_unit", "edbl_unit"))
  expect_equal(des$lgraph$nodes$idvar, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(des$lgraph$nodes$id, 1:5)
  expect_equal(des$lgraph$nodes$label, c("block1", "block2", "block3", "plot1", "plot2"))
  expect_equal(names(des$lgraph$nodes), c("idvar", "id", "label"))

  des2 <- start_design() %>%
    set_units(row = 3,
              col = 4,
              plot = ~row:col)

  expect_snapshot({
    start_design() %>%
      set_units(row = 3,
                col = 4,
                plot = ~row:col) %>%
      serve_table()
  })

  expect_snapshot({
    start_design() %>%
      set_units(row = 3,
                col = 4,
                site = 4,
                plot = ~site:row:col) %>%
      serve_table()
  })

  # FIXME
  expect_snapshot({
    start_design() %>%
      set_units(site = 2,
                row = nested_in(site, 1 ~ 2,
                                      2 ~ 3),
                col = nested_in(site, 3),
                plot = ~site:row:col) %>%
      serve_table()
  })

})
