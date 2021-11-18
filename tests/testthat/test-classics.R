test_that("classics", {
  expect_snapshot(prep_classical_crd(n = 20, t = 3, seed = 3))
  expect_snapshot(prep_classical_rcbd(r = 4, t = 3, seed = 3))
  expect_snapshot(prep_classical_factorial(trt = c(3, 5, 6), r = 2, seed = 3))
  expect_snapshot(prep_classical_factorial(trt = c(3, 5, 6), r = 2,
                                           design = "rcbd", seed = 3))
  expect_snapshot(prep_classical_factorial(trt = 4, r = 2,
                                           design = "rcbd", seed = 3))

  expect_snapshot(find_classical_designs())
  expect_snapshot(code_classical("crd", t = 4, n = 20, seed = 1))
  expect_snapshot(make_classical("rcbd", t = 4, r = 20, seed = 1))
})
