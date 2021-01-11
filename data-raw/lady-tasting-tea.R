## code to prepare `lady-tasting-tea` dataset goes here

library(tidyverse)
set.seed(2021)
lady_tasting_tea <- tibble(cup = factor(1:8),
                           first = sample(rep(c("tea", "milk"), each = 4))) %>%
  mutate(guess = first,
         correct = first == guess)

usethis::use_data(lady_tasting_tea, overwrite = TRUE)
