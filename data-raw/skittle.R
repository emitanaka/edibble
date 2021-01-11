## code to prepare `skittle` dataset goes here

skittles <- readr::read_csv("https://raw.githubusercontent.com/njtierney/skittles/master/data/skittles.csv")
skittles$skittle_type <- factor(skittles$skittle_type)


usethis::use_data(skittles, overwrite = TRUE)
