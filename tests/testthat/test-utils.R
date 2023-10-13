test_that("addition works", {
  U <- set_units(site = 3,
                 plot = nested_in(site, 2))
  T <- set_trts(var = 2,
                fert = 2)

  U + T


  ####

  complex <- design("Complex structure") %>%
    set_units(site = c("Narrabri", "Horsham"),
              row = nested_in(site, 2),
              col = nested_in(site, 3),
              plot = nested_in(site, crossed_by(row, col)))

  factrtc <- design("Factorial treatment with control") %>%
    set_trts(variety = c("a", "b"),
             fertilizer = c("none", "A", "B"),
             amount = conditioned_on(fertilizer,
                                     "none" ~ 0,
                                     . ~ c(0.5, 1, 2)))

  complex + factrtc

})
