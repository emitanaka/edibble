test_that("Setting context", {
   des <- start_design("COVID-19") %>%
     set_context(question = "Does Pfizer vaccine work?",
                    where = "Tested in lab")

   expect_equal(des$context,
                list(question = "Does Pfizer vaccine work?",
                     where = "Tested in lab"))

   f1 <- function() {
     des <- des %>%
       set_context(where = "Tested in USA lab")
   }

   expect_warning(f1(), regexp = "have been ovewritten")

   expect_equal(suppressWarnings(f1()$context),
                list(question = "Does Pfizer vaccine work?",
                     where = "Tested in USA lab"))


})
