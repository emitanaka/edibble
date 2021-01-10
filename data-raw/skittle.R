## code to prepare `skittle` dataset goes here

skittles <- read.csv("https://raw.githubusercontent.com/njtierney/skittles/master/data/skittles.csv")
skittles$real_skittle <- factor(skittles$real_skittle)

usethis::use_data(skittles, overwrite = TRUE)
