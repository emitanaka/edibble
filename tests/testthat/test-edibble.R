test_that("start designs", {
  des1 <- start_design()
  des2 <- start_design("Some design")

  expect_equal(class(des1), c("EdibbleDesign", "R6"))
  expect_equal(des1$name, "An edibble design")
  expect_equal(des2$name, "Some design")
  expect_s3_class(des1$graph, "edbl_graph")
  expect_s3_class(des1$table, "edbl_table")
})

test_that("initiate designs", {
  skip("skip")

  expect_snapshot({
    start_design("Completely Randomised Design") %>%
      set_units(person = 6) %>%
      set_trts(drug = c("A", "B", "C")) %>%
      allocate_trts(~person) %>%
      randomise_trts()
  })

  expect_snapshot({
    start_design("Randomised Complete Block Design") %>%
      set_units(gender = c("female", "male"),
                person = nested_in(gender, 3)) %>%
      set_trts(drug = c("A", "B", "C")) %>%
      allocate_trts(~person) %>%
      randomise_trts()
  })

  expect_snapshot({
    start_design("Randomised Complete Block Design") %>%
      set_units(person = 6,
                gender = group_of(person,
                                  1:3 ~ "male",
                                  4:6 ~ "female")) %>%
      # OR
      set_units(gender = c("female", "male"),
                person = nested_in(gender, 3)) %>%
      set_trts(drug = c("A", "B", "C")) %>%
      allocate_trts(~person) %>%
      randomise_trts()
  })



  expect_snapshot({
    start_design("Latin Square Design") %>%
      set_units(car = 4,
                pos = c("FL", "FR", "BL", "BR"),
                tire = ~car * pos) %>%
      set_trts(brand = c("A", "B", "C", "D")) %>%
      allocate_trts(~tire) %>%
      randomise_trts()
  })

  expect_snapshot({
    start_design("Balanced Incomplete Block Design") %>%
      set_units(mother = 6,
                chick = nested_in(mother, 4)) %>%
      set_trts(diet = c("A", "B", "C", "D", "E")) %>%
      allocate_trts(~tire) %>%
      randomise_trts()
  })


  expect_snapshot({
    start_design("Factorial design") %>%
      set_trts(fert = 2,
               variety = 4) %>%
      set_units(plot = ntrts(each = 2)) %>%
      # OR
      set_units(plot = ntrts() * 2) %>%
      allocate_trts(~plot) %>%
      randomise_trts()
  })

  expect_snapshot({
    start_design("Split-plot design") %>%
      set_trts(irr = c("irrigated", "rainfed"),
               variety = 4) %>%
      set_units(wholeplot = 4,
                subplot = nested_in(wholeplot, 4)) %>%
      allocate_trts(~subplot) %>%
      randomise_trts(irr ~ wholeplot,
                     variety ~ subplot)
  })

  expect_snapshot({
    start_design("Sample size OR dynamically updated labels") %>%
      set_units(person = symbol_size(npeople)) %>%
      set_trts(drug = symbol_labels(drugs)) %>%
      allocate_trts(~person) %>%
      randomise_trts() %>%
      # define symbols
      set_symbols(drugs = c("A", "B", "C"),
                  npeople = calc_size(level = 0.95, effect = 3))



  })

  expect_snapshot({
    start_design("Design in Gould et al (2013) or Woods et al. (2015)") %>%
      set_units(trial = c("ISU Farm 1", "ISU Farm 2", "Private Farm"),
                calve = within(trial, 12),
                  eye = within(calve, c("left", "right"))) %>%
      set_trts(scarification = c("control", "M. bovoculi", "M.bovis"),
                         trt = c("treat", "disregard")) %>%
      #randomise_trts(~ calve * eye) %>%
      allocate_trts(~ calve * eye) %>%
      restrict_mapping(scarification ~ calve,
                                 trt ~ eye) %>%
      randomise_trts()

    start_design("Silver et al. (2005)") %>%
      set_units(topography = c("Ridge", "Slope", "Valley"),
                transect = within(topography, 4),
                core = within(topography & transect, 6)) %>%
      set_trts(trt = c("Control", "Fertilized"),
               helox = c("0%", "20%")) %>%
      allocate_trts(~ core) %>%
      set_response(DEA = core)

  })
})
