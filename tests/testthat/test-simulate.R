test_that("simulate", {
  des <- start_design() %>%
    set_trts(treat = c("A", "B", "C")) %>%
    set_units(unit = 12) %>%
    allot_trts(treat ~ unit) %>%
    assign_trts(seed = 1) %>%
    set_rcrds(response = unit) %>%
    serve_table()

  des2 <- start_design() %>%
    set_trts(treat = c("A", "B", "C"),
             irr = 2) %>%
    set_units(unit = 12) %>%
    allot_trts(~ unit) %>%
    assign_trts(seed = 1) %>%
    set_rcrds(response = unit,
              level = unit) %>%
    serve_table()

  des3 <- start_design() %>%
    set_trts(treat = c("A", "B", "C"),
             irr = 2) %>%
    set_units(unit = 12) %>%
    allot_trts(~ unit) %>%
    assign_trts(seed = 1) %>%
    set_rcrds(response = unit,
              level = unit) %>%
    expect_rcrds(factor(level, c("yes", "no")),
                 response > 0) %>%
    serve_table()

  des3 <- start_design() %>%
    set_trts(treat = c("A", "B", "C"),
             irr = 2) %>%
    set_units(unit = 12) %>%
    allot_trts(~ unit) %>%
    assign_trts(seed = 1) %>%
    set_rcrds(response = unit) %>%
    expect_rcrds(response > 0) %>%
    serve_table()


  des3 %>%
    simulate_rcrds(response = sim_normal(~treat) %>%
                     params("mean",
                            treat = c("A" = 10, "B" = -10, "C" = 0)))

  eff1 <- params(mean = list(treat = c("A" = 3, "B" = 2, "C" = 0)))
  eff2 <- params(mean = list(treat = c("A" = 40, "C" = -100)))
  eff3 <- params(mean = list(treat = c("A" = 40, "C" = -100),
                  irr = c(4, -5)))

  expect_snapshot({
    set.seed(1)
    des %>%
      simulate_rcrds(response = sim_normal(mean = ~30 + treat,
                                           sd = 3,
                                           params = eff1))

    des %>%
      simulate_rcrds(response = sim_t(df = 3,
                                       mean = ~30 + treat,
                                       sd = 3,
                                       params = eff1))

    iris %>%
      simulate_rcrds(petal = sim_normal())

    des %>%
      simulate_rcrds(response = sim_normal(mean = ~1 + treat,
                                           sd = 3,
                                           params = eff2))

    des %>%
      simulate_rcrds(response = sim_beta(shape1 = ~1 + treat,
                                         shape2 = 3,
                                         params = params(shape1 = list(treat = c("A" = 3, "B" = 2, "C" = 0)))))
  })

  expect_snapshot({
    set.seed(1)
    des2 %>%
      simulate_rcrds(response = sim_normal(mean = ~1 + treat + irr,
                                           sd = 3,
                                           params = eff3))
  })

  df2 <- des2 %>%
    simulate_rcrds(response = sim_normal(mean = ~1 + treat + irr,
                                         sd = 3,
                                         params = eff3))

  library(ggplot2)
  as_data_frame(df2) %>%
    ggplot(aes(treat, response, fill = irr)) +
    geom_violin() +
    geom_boxplot(width = 0.1)

  expect_snapshot({
    des %>%
      simulate_rcrds(response = sim_fixed(mean = ~30 + treat,
                                          params = eff1))

    des %>%
      simulate_rcrds(response = sim_fixed(mean = ~30 + treat,
                                          params = eff2))
  })


  eff4 <- params(treat = c("A" = 3, "B" = 2, "C" = 0),
                 unit = ~sim_normal(0, 1))

  eff4 <- params(treat = ~sim_normal(10, 3),
                 unit = ~sim_normal(0, 1))
  des %>%
   simulate_rcrds(response = sim_fixed(mean = ~30 + treat + unit,
                                       params = list(mean = eff4)))
})
