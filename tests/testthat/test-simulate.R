test_that("simulation works", {

  spd <- design("Split-Plot Design | Split-Unit Design") %>%
    set_units(mainplot = 30,
              subplot = nested_in(mainplot, 4)) %>%
    set_trts(trt1 = 3,
             trt2 = 4) %>%
    allot_trts(trt1 ~ mainplot,
               trt2 ~ subplot) %>%
    assign_trts("random", seed = 719) %>%
    serve_table() %>%
    set_rcrds(mass = mainplot,
              yield = subplot,
              type = subplot) %>%
    expect_rcrds(mass > 0,
                 yield > 0,
                 factor(type, levels = c("A", "B")))

  # autofill all rspds
  spd %>%
    autofill_rspds()

  # select dependent factors
  spd %>%
    autofill_rspds(mass = c(mainplot, subplot, trt1))

  # maybe not...
  # spd %>%
  #  simulate_model(lm(mass ~ trt1),
  #                 lmer(yield ~ trt2))

  spd %>%
    simulate_process(mass = function(effects = c("trt11" = 3, "trt12" = 3, "trt13" = -10, "trt14" = -3)) {
        effects[trt1] + rnorm(n())
      })

  spd %>%
    simulate_rcrds(mass = list(effects = c("trt11" = 0, "trt12" = 3, "trt13" = -3, "trt14" = 0)))

  spd %>%
    simulate_process(
    .joint = function(C = matrix(c(1, -0.5, -0.5, 3), 2, 2)) {
      res <- mvtnorm::rmvnorm(n(), sigma = C)
      res <- as.data.frame(res)
      colnames(res) <- c("mass", "yield")
      res
    })

  spd %>%
    simulate_rcrds(.joint = list())



})
