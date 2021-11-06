## code to prepare `rye_grass` dataset goes here

set.seed(2020)
rye_grass <- start_design("Rye-grass") %>%
  set_context(aim = "Compare 3 different cultivars of rye-grass in
                     combination with 4 quantitites of nitrogen fertilizer",
              perc = "percentage of water-soluble carbohydrate in the crop",
              weight = "total weight of dry matter harvested",
              "Cultivars sown on whole strips since cannot sow in small
               area unless done by hand",
              "Fertilizers can be applied to small areas") %>%
  set_units(strip = 6,
            plot = nested_in(strip, 4)) %>%
  set_trts(cultivar = c("Cropper", "Melle", "Melba"),
           fertilizer = c(0, 80, 160, 240)) %>%
  allot_trts(cultivar ~ strip,
                fertilizer ~ plot) %>%
  assign_trts("random") %>%
  set_rcrds_of(plot = c("weight", "perc")) %>%
  expect_rcrds(weight = to_be_numeric(with_value(">=", 0)),
              perc = to_be_numeric(with_value(between = c(0, 100)))) %>%
  serve_table()

usethis::use_data(rye_grass, overwrite = TRUE)
