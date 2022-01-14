test_that("set_units", {

  expect_snapshot({
    start_design(name = "unlinked units") %>%
      set_units(block = 3,
                plot = 2)
  })

  des <- start_design(name = "unlinked units") %>%
    set_units(block = 3,
              plot = 2)

  expect_equal(nrow(des$graph$nodes), 2)
  expect_equal(names(des$graph$nodes), c("id", "label", "class"))
  expect_equal(nrow(des$graph$levels$nodes), 5)
  expect_equal(des$graph$nodes$id, c(1L, 2L))
  expect_equal(des$graph$nodes$label, c("block", "plot"))
  expect_equal(des$graph$nodes$class, c("edbl_unit", "edbl_unit"))
  expect_equal(des$graph$levels$nodes$idvar, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(des$graph$levels$nodes$id, 1:5)
  expect_equal(des$graph$levels$nodes$label, c("block1", "block2", "block3", "plot1", "plot2"))
  expect_equal(names(des$graph$levels$nodes), c("idvar", "id", "label"))

  des2 <- start_design() %>%
    set_units(row = 3,
              col = 4,
              plot = ~row:col)

  expect_equal(des2,
               structure(list(name = NULL,
                              graph = structure(list(nodes = data.frame(id = 1:3,
                                                                        label = c("row", "col", "plot"),
                                                                        class = c("edbl_unit",  "edbl_unit", "edbl_unit")),
                                                     edges = data.frame(from = 1:2,
                                                                        to = c(3L, 3L),
                                                                        alloc = NA_integer_),
                                                     levels = list(nodes = data.frame(idvar = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                                                                                      id = 1:19,
                                                                                      label = c("row1", "row2", "row3", "col1", "col2", "col3", "col4", "plot1", "plot2", "plot3", "plot4",
                                                                                          "plot5", "plot6", "plot7", "plot8", "plot9", "plot10", "plot11",
                                                                                          "plot12")),
                                                                   edges = data.frame(from = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 7L, 7L, 7L),
                                                                                      to = c(8:19, 8:19),
                                                                                      alloc = NA_integer_))),
                                                     class = "edbl_graph")),
                         class = c("edbl_design", "edbl")))

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
