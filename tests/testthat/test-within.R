test_that("multiplication works", {
  within(site & block & plant,
         "site1" & . & . ~ 3,
                       . ~ 2)
})
