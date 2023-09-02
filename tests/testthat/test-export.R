test_that("export with no record", {
  set.seed(1)
  des <- takeout()
  fn <- tempfile()
  export_design(des0, file = fn, overwrite = TRUE)
  dat <- openxlsx2::read_xlsx(fn, sheet = 2)

  expect_equal(dat, as_tibble(des), ignore_attr = TRUE)
})

test_that("export with record", {
  set.seed(1)
  des <- takeout(menu_split()) %>%
    set_rcrds(yield = mainplot,
              height = subplot,
              genotype = subplot,
              yield_date = mainplot) %>%
    expect_rcrds(yield > 0,
                 #date = to_be_date(range = edibble::)
                 100 > height,
                 height > 0,
                 factor(genotype, levels = c("A", "B")))


  fn <- tempfile()
  export_design(des, file = fn, overwrite = TRUE)

  dat <- openxlsx2::read_xlsx(fn, sheet = 2)
  #expect_equal(dat, as_tibble(des), ignore_attr = TRUE)

})
