test_that("serve", {
  expect_snapshot({
    start_design(name = "unlinked units with table") %>%
      set_units(block = 3,
                plot = 2) %>%
      serve_table()
  })

  expect_snapshot({
    start_design(name = "one unit") %>%
      set_units(block = 3) %>%
      serve_table()
  })

  expect_snapshot({
    start_design(name = "serve nested units") %>%
      set_units(block = 3,
                plot = nested_in(block, 2)) %>%
      serve_table()
  })

  des1 <- start_design() %>%
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
  expect_equal(as.character(tabs[[3]]$plot), paste0("plot", 1:12))


  expect_snapshot({
    start_design() %>%
      set_trts(vaccine = c("AZ", "M", "P")) %>%
      serve_table()
  })
})
