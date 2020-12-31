set.seed(1)
nclassics <- list()

nclassics$CRD <- initiate_design(name = "CRD") %>%
  set_units(unit = 8) %>%
  set_trts(trt = LETTERS[1:4]) %>%
  allocate_trts(trt ~ unit) %>%
  randomise_trts()

nclassics$RCBD <- initiate_design(name = "RCBD") %>%
  set_units(block = c("B1", "B2"),
            unit = nested_in(block, 4)) %>%
  set_trts(trt = LETTERS[1:4]) %>%
  allocate_trts(~unit) %>%
  randomise_trts()

nclassics$split <- initiate_design(name = "Split plot design") %>%
  set_units(mainplot = 4,
            subplot = nested_in(mainplot, 2)) %>%
  set_trts(variety = 2,
           irrigation = c("rainfed", "irrigated")) %>%
  allocate_trts(variety ~ subplot,
             irrigation ~ mainplot) %>%
  randomise_trts()

nclassics$factorial <- initiate_design(name = "Factorial design") %>%
  set_units(mainplot = 4,
            subplot = nested_in(mainplot, 2)) %>%
  set_trts(variety = 2,
           irrigation = c("rainfed", "irrigated")) %>%
  allocate_trts(~ subplot) %>%
  randomise_trts()

usethis::use_data(nclassics, overwrite = TRUE)

classics <- lapply(nclassics, serve_table)
usethis::use_data(classics, overwrite = TRUE)
