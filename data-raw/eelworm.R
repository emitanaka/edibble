## code to prepare `eelworm` dataset goes here


eelworm <- start_design("eelworm") %>%
  set_units(block = c("I", "II", "III", "IV"),
            plot = nested_in(block, 12)) %>%
  set_trts(dose = c("zero", "single", "double"),
           type = c("control", "Z", "K", "N", "M", "S"),
           trt = cross_fct(dose, type))

eelworm <- start_design("eelworm") %>%
  set_units(block = c("I", "II", "III", "IV"),
            plot = nested_in(block, 12)) %>%
  set_trts(dose = c("single", "double"),
           type = c("Z", "K", "N", "M", "S")) %>%
  add_trts(.control = TRUE)


usethis::use_data(eelworm, overwrite = TRUE)
