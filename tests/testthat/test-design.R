test_that("start designs", {
  des1 <- start_design()
  des2 <- start_design("Some design")

  expect_equal(class(des1), c("edbl_design", "edbl"))
  expect_equal(des1$name, "An edibble design")
  expect_equal(des2$name, "Some design")

  expect_snapshot({
    des1
  })

  expect_snapshot({
    des2
  })
})
