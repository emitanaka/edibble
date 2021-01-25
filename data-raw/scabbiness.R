## code to prepare `scabbiness` dataset goes here

library(edibble)

scabbiness <- start_design("scab disease in potatoes") %>%
  set_units(plot = 4 * 8,
            potato = nested_in(plot, 100)) %>%
  set_trts(trt = 1:7) %>%
  record_vars(perc = potato)  %>%
  derive_vars(index = calculate(perc, ~mean(.x), .group_by = plot))

# derive_vars(index = calculate(c(perc, tang), xlf("MEAN(.x)"))

usethis::use_data(scabbiness, overwrite = TRUE)
