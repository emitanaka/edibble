test_that("treatments", {
  # FIXME
  expect_snapshot({
    design(seed = 1) %>%
      set_trts(vaccine = 2)
  })


  expect_snapshot({
    design(seed = 1) %>%
      set_trts(vaccine = 2,
               sex = 2)
  })


  expect_snapshot({
    design(seed = 1) %>%
      set_units(person = 5) %>%
      set_trts(vaccine = 2,
               sex = 2)
  })

  expect_snapshot({
    design(seed = 1) %>%
      set_trts(vaccine = 2,
               sex = 2) %>%
      set_units(person = 5)
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3,
               sex = 2) %>%
      set_units(person = 30) %>%
      allot_trts(~person) %>%
      assign_trts("systematic") %>%
      serve_table()
  })



  expect_error({
    design(seed = 1) %>%
      set_trts(irrigation = 2) %>%
      set_units(person = 3) %>%
      allot_trts( ~ peason)
  })

  expect_error({
    design(seed = 1) %>%
      set_trts(irrigation = 2) %>%
      set_units(person = 3) %>%
      allot_trts(irr ~ person)
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3,
               sex = c("F", "M")) %>%
      set_units(person = 30) %>%
      allot_trts(vaccine:sex ~ person) %>%
      assign_trts("systematic") %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3,
               sex = c("F", "M")) %>%
      set_units(person = 30) %>%
      allot_trts(vaccine ~ person,
                 sex ~ person) %>%
      assign_trts("systematic") %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 30) %>%
      allot_trts(vaccine ~ person) %>%
      assign_trts("systematic") %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 5) %>%
      allot_trts(vaccine ~ person) %>%
      assign_trts("systematic-random", seed = 2) %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 5) %>%
      allot_trts(vaccine ~ person) %>%
      assign_trts("random", seed = 3) %>%
      serve_table()
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person, 3)) %>%
      allot_trts(vaccine ~ person) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person, 3)) %>%
      allot_trts(vaccine ~ blood) %>%
      assign_trts("random", seed = 2, constrain = NULL) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person, 3)) %>%
      allot_trts(vaccine ~ blood) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person, 2)) %>%
      allot_trts(vaccine ~ blood) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person, 8)) %>%
      allot_trts(vaccine ~ blood) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(vaccine = 3) %>%
      set_units(person = 20,
                blood = nested_in(person,
                                  1 ~ 8,
                                  2 ~ 3,
                                  . ~ 4)) %>%
      allot_trts(vaccine ~ blood) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$vaccine, tab$person)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(fert = 8) %>%
      set_units(site = 10,
                plot = nested_in(site, 10),
                sample = nested_in(plot,
                                  1 ~ 8,
                                  2 ~ 3,
                                  . ~ 4)) %>%
      allot_trts(fert ~ sample) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$fert, tab$plot)
  })

  expect_snapshot({
    tab <- design() %>%
      set_trts(fert = 2,
               irr = 2) %>%
      set_units(block = 10,
                wplot = nested_in(block, 3),
                splot = nested_in(wplot, 4)) %>%
      allot_trts(fert ~ splot,
                 irr ~ wplot) %>%
      assign_trts("random", seed = 2) %>%
      serve_table()
    table(tab$fert, tab$irr, tab$wplot)
  })

  design("McIntyre (1955)") %>%
    set_units(plant = 8,
              position = 1:4,
              leaf = ~plant:position) %>%
    set_trts(light = c("a", "b", "c", "d")) %>%
    allot_trts(light ~ leaf) %>%
    assign_trts(seed = 1) %>%
    serve_table()

  design("McIntyre (1955)") %>%
    set_units(site = 4,
              plant = 8,
              position = 1:4,
              leaf = ~site:plant:position) %>%
    set_trts(light = c("a", "b", "c", "d")) %>%
    allot_trts(light ~ leaf) %>%
    assign_trts(seed = 1) %>%
    serve_table()

})
