test_that("crd", {
  expect_snapshot({
    crd <- takeout(menu_crd(n = 24, t = 4, seed = 1))
  })
  expect_equal(as.vector(table(crd$trt)), rep(6, 4))
})

test_that("rcbd", {
  expect_snapshot({
    rcbd <- takeout(menu_rcbd(r = 3, t = 5, seed = 1))
  })
  expect_equal(as.vector(table(rcbd$block, rcbd$trt)), rep(1, 15))
})

test_that("split", {
  expect_snapshot({
    split <- takeout(menu_split(t1 = 3, t2 = 2, r = 2, seed = 1))
  })
  expect_equal(as.vector(apply(table(split$mainplot, split$trt1), 2, table)), rep(c(4, 2), times = 3))
  expect_equal(as.vector(table(split$trt1)), rep(4, 3))
  expect_equal(as.vector(table(split$trt2)), rep(6, 2))
})

test_that("factorial", {
  expect_snapshot({
    fac_crd <- takeout(menu_factorial(trt = c(2, 3, 4), design  = "crd", r = 2, seed = 1))
    fac_rcbd <- takeout(menu_factorial(trt = c(2, 3, 4), design  = "rcbd", r = 2, seed = 1))
  })
  expect_equal(as.vector(table(fac_crd$trt1, fac_crd$trt2, fac_crd$trt3)), rep(2, 24))
  expect_equal(as.vector(table(fac_rcbd$trt1, fac_rcbd$trt2, fac_rcbd$trt3)), rep(2, 24))
  expect_equal(as.vector(table(fac_rcbd$trt1, fac_rcbd$trt2, fac_rcbd$trt3, fac_rcbd$block)), rep(1, 48))
})

test_that("lsd", {
  expect_snapshot({
    lsd <- takeout(menu_lsd(t = 10, seed = 1))
  })
  expect_false(any(tapply(lsd$trt, lsd$column, function(x) any(duplicated(x)))))
  expect_false(any(tapply(lsd$trt, lsd$row, function(x) any(duplicated(x)))))
  expect_equal(as.vector(table(lsd$trt)), rep(10, 10))
})

test_that("youden", {
  expect_snapshot({
    youden <- takeout(menu_youden(nc = 7, t = 10, seed = 1))
  })
})
