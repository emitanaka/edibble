## code to prepare `scabbiness` dataset goes here

library(edibble)

scabbiness <- start_design("scab disease in potatoes") %>%
  set_units(plot = 4 * 8,
            potato = nested_in(plot, 100)) %>%
  set_trts(trt = 1:7) %>%
  record_vars(perc = potato,
              potato = c("trait1", "trait2"))
  record_vars(perc = on(potato, unit = "kg"))

  record_vars(potato = c(trait("trait1"),
                         trait("trait2")))

  record_vars(potato = perc)  %>%
  derive_vars(index = calculate(perc, ~mean(.x), .group_by = plot))

derive_vars(index = calculate(perc, ~mean(.x), .by = plot))

# derive_vars(index = calculate(c(perc, tang), xlf("MEAN(.x)"))

usethis::use_data(scabbiness, overwrite = TRUE)
