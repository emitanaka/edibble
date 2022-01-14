test_that("nested-units", {

  expect_snapshot({
    start_design(name = "nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2))
  })

  des1 <- start_design(name = "nested units") %>%
    set_units(block = 3,
              plot = nested_in(block, 2))

  expect_equal(des1,
               structure(list(name = "nested units",
                              graph = structure(list(nodes = data.frame(id = c(1L, 2L),
                                                                        label = c("block", "plot"),
                                                                        class = "edbl_unit"),
                                                     edges = data.frame(from = 1L,
                                                                        to = 2L,
                                                                        alloc = NA_integer_),
                                                     levels = list(nodes = data.frame(idvar = rep(c(1L, 2L), c(3, 6)),
                                                                                      id = 1:9,
                                                                                      label = c(paste0("block", 1:3), paste0("plot", 1:6))),
                                                                   edges = data.frame(from = rep(c(1L, 2L, 3L), each = 2),
                                                                                      to = 4:9,
                                                                                      alloc = NA_integer_))),
                                                class = "edbl_graph")),
                         class = c("edbl_design", "edbl")))

  des2 <- des1 %>%
    set_units(sample = nested_in(plot, 10))

  expect_equal(des2,
               structure(list(name = "nested units",
                              graph = structure(list(nodes = data.frame(id = c(1L, 2L, 3L),
                                                                        label = c("block", "plot", "sample"),
                                                                        class = "edbl_unit"),
                                                     edges = data.frame(from = c(1L, 2L),
                                                                        to = c(2L, 3L),
                                                                        alloc = NA_integer_),
                                                     levels = list(nodes = data.frame(idvar = rep(c(1L, 2L, 3L), c(3, 6, 60)),
                                                                                      id = 1:(3 + 6 + 60),
                                                                                      label = c(paste0("block", 1:3), paste0("plot", 1:6), paste0("sample", 1:60))),
                                                                   edges = data.frame(from = c(rep(c(1L, 2L, 3L), each = 2), rep(4:9, each = 10)),
                                                                                      to = c(4:9, 10:69),
                                                                                      alloc = NA_integer_))),
                                                class = "edbl_graph")),
                         class = c("edbl_design", "edbl")))

  des3 <- des1 %>%
    set_units(sample = nested_in(plot,
                                 1 ~ 20,
                                 . ~ 3, leading0 = 3))

  expect_equal(des3,
               structure(list(name = "nested units",
                              graph = structure(list(nodes = data.frame(id = c(1L, 2L, 3L),
                                                                        label = c("block", "plot", "sample"),
                                                                        class = "edbl_unit"),
                                                     edges = data.frame(from = c(1L, 2L),
                                                                        to = c(2L, 3L),
                                                                        alloc = NA_integer_),
                                                     levels = list(nodes = data.frame(idvar = rep(c(1L, 2L, 3L), c(3, 6, 20 + 3 * 5)),
                                                                                      id = 1:(3 + 6 + 35),
                                                                                      label = c(paste0("block", 1:3), paste0("plot", 1:6), sprintf("sample%.3d", 1:35))),
                                                                   edges = data.frame(from = c(rep(c(1L, 2L, 3L), each = 2), rep(4:9, c(20, 3, 3, 3, 3, 3))),
                                                                                      to = c(4:9, 10:44),
                                                                                      alloc = NA_integer_))),
                                                class = "edbl_graph")),
                         class = c("edbl_design", "edbl")))

  expect_equal(nesting(des1), list(plot = "block"))
  expect_equal(nesting(des2), list(plot = "block", sample = "plot"))

})
