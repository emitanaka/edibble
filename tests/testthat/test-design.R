test_that("start designs", {
  des1 <- design()
  des2 <- design("Some design")

  expect_equal(class(des1), c("edbl_design", "edbl"))
  expect_equal(des1$name, NULL)
  expect_equal(des2$name, "Some design")

  expect_snapshot({
    des1
  })

  expect_snapshot({
    des2
  })
})
