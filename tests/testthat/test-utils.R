test_that("addition works", {
  U <- set_units(site = 3,
                 plot = nested_in(site, 2))
  T <- set_trts(var = 2,
                fert = 2)

  U + T
})
