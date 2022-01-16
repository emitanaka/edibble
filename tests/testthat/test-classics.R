test_that("classics", {
  expect_snapshot(prep_classical_crd(n = 20, t = 3, seed = 3))
  expect_snapshot(prep_classical_rcbd(r = 4, t = 3, seed = 3))
  expect_snapshot(prep_classical_factorial(trt = c(3, 5, 6), r = 2, seed = 3))
  expect_snapshot(prep_classical_factorial(trt = c(3, 5, 6), r = 2,
                                           design = "rcbd", seed = 3))
  expect_snapshot(prep_classical_factorial(trt = 4, r = 2,
                                           design = "rcbd", seed = 3))
  expect_snapshot(prep_classical_split(t1 = 3, t2 = 2, r = 2, seed = 3))
  expect_snapshot(prep_classical_lsd(t = 5, seed = 3))

  expect_snapshot(find_classical_designs())
  expect_snapshot(code_classical("crd", t = 4, n = 20, seed = 1))
  expect_snapshot(make_classical("rcbd", t = 4, r = 20, seed = 1))

  crd <- make_classical("crd", n = 24, t = 4, .output = FALSE)
  rcbd <- make_classical("rcbd", r = 3, t = 5, .output = FALSE)
  split <- make_classical("split", t1 = 3, t2 = 2, r = 2, .output = FALSE)
  fac_crd <- make_classical("factorial", trt = c(2, 3, 4), design  = "crd", r = 2, .output = FALSE)
  fac_rcbd <- make_classical("factorial", trt = c(2, 3, 4), design  = "rcbd", r = 2, .output = FALSE)
  lsd <- make_classical("lsd", t = 10, .output = FALSE)

  expect_equal(as.vector(table(crd$trt)), rep(6, 4))
  expect_equal(as.vector(table(rcbd$block, rcbd$trt)), rep(1, 15))
  expect_equal(as.vector(apply(table(split$mainplot, split$trt1), 2, table)), rep(c(4, 2), times = 3))
  expect_equal(as.vector(table(split$trt1)), rep(4, 3))
  expect_equal(as.vector(table(split$trt2)), rep(6, 2))
  expect_equal(as.vector(table(fac_crd$trt1, fac_crd$trt2, fac_crd$trt3)), rep(2, 24))
  expect_equal(as.vector(table(fac_rcbd$trt1, fac_rcbd$trt2, fac_rcbd$trt3)), rep(2, 24))
  expect_equal(as.vector(table(fac_rcbd$trt1, fac_rcbd$trt2, fac_rcbd$trt3, fac_rcbd$block)), rep(1, 48))
  expect_false(any(tapply(lsd$trt, lsd$column, function(x) any(duplicated(x)))))
  expect_false(any(tapply(lsd$trt, lsd$row, function(x) any(duplicated(x)))))
  expect_equal(as.vector(table(lsd$trt)), rep(10, 10))

})
