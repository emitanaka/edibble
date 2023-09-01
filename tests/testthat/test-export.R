test_that("export works", {
  set.seed(1)
  des0 <- takeout(menu_split()) %>%
    set_rcrds(yield = mainplot,
              height = subplot,
              genotype = subplot,
              yield_date = mainplot) %>%
    expect_rcrds(yield > 0,
                 #date = to_be_date(range = edibble::)
                 100 > height,
                 height > 0,
                 factor(genotype, levels = c("A", "B")))

  prov <- activate_provenance(des0)
  prov$get_validation("rcrd")

  ## validation by writing file and reading it in

  export_design(des0, file = "~/Downloads/test.xlsx", overwrite = TRUE)

})
