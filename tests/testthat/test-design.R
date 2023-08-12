test_that("start designs", {
  des1 <- design()
  des2 <- design("Some design")

  expect_equal(class(des1), c("edbl_design", "edbl"))

  expect_snapshot({
    des1
  })

  expect_snapshot({
    des2
  })
})
