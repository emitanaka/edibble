test_that("pivot works", {
  crd <- design("Completely Randomised Design", seed = 1) %>%
    set_units(unit = 29) %>%
    set_trts(trt = 6) %>%
    allot_trts(trt ~ unit) %>%
    assign_trts("random", seed = 906) %>%
    serve_table()


  expect_equal(pivot_trts_widelist(crd),
               list(trt1 = structure(list(unit = c("unit4", "unit19", "unit21",
                                                   "unit24", "unit29")), row.names = c(4L, 19L, 21L, 24L, 29L), class = "data.frame"),
                    trt2 = structure(list(unit = c("unit5", "unit8", "unit14",
                                                   "unit18", "unit26")), row.names = c(5L, 8L, 14L, 18L, 26L
                                                   ), class = "data.frame"), trt3 = structure(list(unit = c("unit2",
                                                                                                            "unit11", "unit12", "unit28")), row.names = c(2L, 11L, 12L,
                                                                                                                                                          28L), class = "data.frame"), trt4 = structure(list(unit = c("unit6",
                                                                                                                                                                                                                      "unit13", "unit23", "unit25", "unit27")), row.names = c(6L,
                                                                                                                                                                                                                                                                              13L, 23L, 25L, 27L), class = "data.frame"), trt5 = structure(list(
                                                                                                                                                                                                                                                                                unit = c("unit1", "unit3", "unit9", "unit15", "unit17"
                                                                                                                                                                                                                                                                                )), row.names = c(1L, 3L, 9L, 15L, 17L), class = "data.frame"),
                    trt6 = structure(list(unit = c("unit7", "unit10", "unit16",
                                                   "unit20", "unit22")), row.names = c(7L, 10L, 16L, 20L, 22L
                                                   ), class = "data.frame")))

  expect_equal(pivot_trts_widelist(crd, drop = TRUE),
               list(
               trt1 = c("unit4", "unit19", "unit21", "unit24", "unit29"),
               trt2 = c("unit5", "unit8", "unit14", "unit18", "unit26"),
               trt3 = c("unit2", "unit11", "unit12", "unit28"),
               trt4 = c("unit6", "unit13", "unit23", "unit25", "unit27"),
               trt5 = c("unit1", "unit3", "unit9", "unit15", "unit17"),
               trt6 = c("unit7", "unit10", "unit16", "unit20", "unit22")))

  expect_equal(pivot_trts_widetable(crd),
               structure(list(trt1 = c("unit4", "unit19", "unit21", "unit24", "unit29"),
                              trt2 = c("unit5", "unit8", "unit14", "unit18", "unit26"),
                              trt3 = c("unit2", "unit11", "unit12", "unit28", NA),
                              trt4 = c("unit6", "unit13", "unit23", "unit25", "unit27"),
                              trt5 = c("unit1", "unit3", "unit9", "unit15", "unit17"),
                              trt6 = c("unit7", "unit10", "unit16", "unit20", "unit22")),
                         class = "data.frame", row.names = c(NA, 5L)))


})
