# nested-units

    Code
      des1 <- start_design(name = "nested units", seed = 1) %>% set_units(block = 3,
        plot = nested_in(block, 2))
      des1
    Output
      nested units
      \-block (3 levels)
        \-plot (6 levels)

