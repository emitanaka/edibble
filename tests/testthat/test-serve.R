test_that("serve", {
  expect_error({
    design(title = "unlinked units with table") %>%
      set_units(block = 3,
                plot = 2) %>%
      serve_table()
  })
  # TODO: when the title is long, it cuts off
  # The cut-off seems to have only happened for when the title was "unlinked units with table"
  # and the ANSI styling was cut
  expect_error({
    design("unlinked units with table") %>%
      set_units(block = 3,
                plot = 2) %>%
      serve_table(fail = "ignore")
  },)

  expect_snapshot({
    design(title = "one unit") %>%
      set_units(block = 3) %>%
      serve_table()
  })


  expect_equal({
    design(title = "one unit") %>%
      set_units(block = 3) %>%
      serve_table()
  }, data.frame(block = c("block1", "block2", "block3")), ignore_attr = TRUE)

  expect_snapshot({
    design(title = "serve nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2)) %>%
      serve_table()
  })

  des1 <- design() %>%
    set_units(site = 2)
  des2 <- des1 %>%
    set_units(block = nested_in(site, 3))
  des3 <- des2 %>%
    set_units(plot = nested_in(block, 2))
  tabs <- lapply(list(des1, des2, des3), serve_table)

  expect_equal(nrow(tabs[[1]]), 2)
  expect_equal(ncol(tabs[[1]]), 1)
  expect_equal(as.character(tabs[[1]]$site), c("site1", "site2"))
  expect_equal(nrow(tabs[[2]]), 6)
  expect_equal(ncol(tabs[[2]]), 2)
  expect_equal(as.character(tabs[[2]]$site), rep(c("site1", "site2"), each = 3))
  expect_equal(as.character(tabs[[2]]$block), paste0("block", 1:6))
  expect_equal(nrow(tabs[[3]]), 12)
  expect_equal(ncol(tabs[[3]]), 3)
  expect_equal(as.character(tabs[[3]]$site), rep(c("site1", "site2"), each = 3 * 2))
  expect_equal(as.character(tabs[[3]]$block), rep(paste0("block", 1:6), each = 2))
  expect_equal(as.character(tabs[[3]]$plot), sprintf("plot%.2d", 1:12))


  expect_error({
    design() %>%
      set_trts(vaccine = c("AZ", "M", "P")) %>%
      serve_table()
  })

  expect_snapshot({
    design() %>%
      set_units(site = 2,
                row = nested_in(site,
                                1 ~ 3,
                                2 ~ 2),
                col = nested_in(site,
                                1 ~ 3,
                                2 ~ 2),
                plot = nested_in(site, ~row:col)) %>%
      set_trts(trt = c("A", "B")) %>%
      allot_table(trt ~ plot, seed = 1)
  })
})
