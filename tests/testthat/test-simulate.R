test_that("simulation works", {

  spd <- design("Split-Plot Design | Split-Unit Design") %>%
    set_units(mainplot = 30,
              subplot = nested_in(mainplot, 4)) %>%
    set_trts(trt1 = LETTERS[1:3],
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
                 yield < 100,
                 factor(type, levels = c("A", "B")))


  spd2 <- spd %>%
    simulate_process(mass = function(A = 3, B = -3, C = 0) {
        effects <- c(A = A, B = B, C = C)
        effects[trt1] + rnorm(n())
      }, yield = function() runif(n(), 0, 200))

  spdsim <- spd2 %>%
    simulate_rcrds(mass = with_params(A = 2, B = 4, C = 80,
                                      .aggregate = mean),
                   .seed = 1)

  expect_warning(simulate_rcrds(spd2, mass = with_params(), mass = with_params()))


  expect_equal(tapply(spdsim$mass, spdsim$trt1, function(x) mean(x, na.rm = TRUE)),
               c(2, 4, 80), ignore_attr = TRUE, tolerance = 0.3)


  spdsim <- spd2 %>%
    simulate_rcrds(mass = with_params(A = 2, B = 4, C = 80,
                                      .aggregate = mean),
                   yield = with_params(.censor = c(0, ~max(.x[.x < .upper]))),
                   .seed = 1)


  spd3 <- spd %>%
    simulate_process(
    .joint = function(C = matrix(c(1, -0.5, -0.5, 3), 2, 2)) {
      res <- mvtnorm::rmvnorm(n(), sigma = C)
      res <- as.data.frame(res)
      colnames(res) <- c("mass", "yield")
      res
    })

  expect_error(simulate_rcrds(spd3, nonexistant = with_params()))



  spd3 %>%
    simulate_rcrds(.joint = with_params(.censor = list(mass = NA,
                                                       yield = c(0, ~max(.x[.x < 3])))))


  spd3 %>%
    simulate_rcrds(.joint = with_params(.censor = list(.default = c(NA, 10),
                                                       yield = c(0, ~max(.x[.x < 3])))))

  spd3 %>%
    simulate_rcrds(.joint = with_params(.censor = list(.default = c(NA, 10))))


  spd3 %>%
    simulate_rcrds(.joint = with_params(.censor = list(mass = c(NA, 10),
                                                       yield = c(0, ~max(.x[.x < .upper])))))


  # autofill all rspds
  #spd %>%
  #  autofill_rcrds()

  # select dependent factors
  #spd %>%
  #  autofill_rcrds(mass = c(mainplot, subplot, trt1))

  # maybe not...
  # spd %>%
  #  simulate_model(lm(mass ~ trt1),
  #                 lmer(yield ~ trt2))


})
