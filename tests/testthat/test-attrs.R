test_that("check lvls works", {
  des0 <- set_trts(diet = fct_attrs(lvls(value = c("A", "B", "C"))))
  des1 <- set_trts(diet = lvls(value = c("A", "B", "C")))
  expect_equal(fct_graph(des0), fct_graph(des1))
  expect_equal(lvl_graph(des0), lvl_graph(des1))
  trtinfo <- data.frame(label = c("Keto", "Vegan", "Meat"))
  des2 <- set_trts(diet = fct_attrs(lvls(value = c("A", "B", "C"),
                                         label = trtinfo$label)))
  des3 <- set_trts(diet = fct_attrs(lvls(value = c("A", "B", "C"),
                                         data = trtinfo)))
  out <- list(diet = tibble::tibble(value = LETTERS[1:3],
                                    n = NA_integer_,
                                    attrs = trtinfo))

  trtinfo$value <- LETTERS[1:3]
  des4 <- set_trts(diet = fct_attrs(lvls(value = column(value),
                                         data = trtinfo)))
  des5 <- set_trts(diet = fct_attrs(lvls(value = column(2),
                                         data = trtinfo)))

  expect_equal(lvl_nodes(des2), out)
  expect_equal(lvl_nodes(des3), out)
  expect_equal(lvl_nodes(des4), out)
  expect_equal(lvl_nodes(des5), out)

  attrs_lvls <- data.frame(sex = rep(c("F", "M"), 3),
                           height = c(0.514, -0.156, 0.731, -2.633, 0.912, 0.439))
  des5 %>%
    set_units(subject = lvls(1:6,
                             sex = attrs_lvls$sex,
                             height = attrs_lvls$height)) %>%
    lvl_nodes() %>%
    expect_equal(c(out, list(subject = tibble::tibble(value = 1:6,
                                                      n = NA_integer_,
                                                      attrs = attrs_lvls))))


})


test_that("check fct_attrs works", {
  des0 <- set_trts(diet = fct_attrs(c("A", "B", "C"),
                                    desc = "human diet"),
                   exercise = fct_attrs(c("Y", "N"),
                                        label = "Exercise"))
  expect_equal(fct_nodes(des0), tibble::tibble(name = c("diet", "exercise"),
                                               role = "edbl_trt",
                                               attrs = data.frame(desc = c("human diet", NA),
                                                                  label = c(NA, "Exercise"))))

})


test_that("lvls works", {
  expect_error(lvls(value = sample(c("A", "A", "B", "B", "C"))))
})
