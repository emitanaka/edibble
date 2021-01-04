test_that("Adding context", {
   des <- start_design("COVID-19") %>%
     add_context(question = "Does Pfizer vaccine work?",
                    where = "Tested in lab")

   expect_equal(des$context,
                list(question = "Does Pfizer vaccine work?",
                     where = "Tested in lab"))

   f1 <- function() {
     des <- des %>%
       add_context(where = "Tested in USA lab")
   }

   expect_warning(f1(), regexp = "have been ovewritten")

   expect_equal(suppressWarnings(f1()$context),
                list(question = "Does Pfizer vaccine work?",
                     where = "Tested in USA lab"))

   f2 <- function() {
     des <- des %>%
       add_context(where = "Tested in lab",
                   .overwrite = FALSE)
   }

   expect_warning(f2(), regexp = "have been ignored")

   expect_equal(suppressWarnings(f2()$context),
                list(question = "Does Pfizer vaccine work?",
                     where = "Tested in USA lab"))

})
