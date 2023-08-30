test_that("allot works", {
  des1 <- design() %>%
       set_units(block = 10,
                 plot = 20) %>%
       allot_units(block ~ plot)

  fedges1 <- fct_edges(des1)
  expect_equal(fedges1$var_from, "block")
  expect_equal(fedges1$var_to, "plot")


  des2 <- design() %>%
    set_units(block = 10,
               plot = nested_in(block, 3)) %>%
     set_trts(treat = c("A", "B", "C"),
              pest = c("a", "b")) %>%
     allot_trts(treat ~ plot,
                 pest ~ block)

    fedges2 <- fct_edges(des2)
  expect_equal(fedges2$var_from, c("block", "treat", "pest"))
  expect_equal(fedges2$var_to, c("plot", "plot", "block"))

  # it seems that factor has precedence in printing over other types
  # in tibble
  ChickWeight %>%
    edibble() %>%
    set_units(Diet)

  ChickWeight %>%
    edibble() %>%
    set_units(Chick, Time)

})
