test_that("label works", {
  expect_equal(label_seq_from_to(from = 8, to = 10, by = 2), c("08", "10"))
  expect_equal(label_seq_from_to(from = 8, to = 10, leading_zero = 3), c("008", "009", "010"))
  expect_equal(label_seq_from_length(from = 8, length = 3, prefix = "P", sep_prefix = "-", leading_zero = TRUE), c("P-08", "P-09", "P-10"))
  expect_equal(label_seq_to_length(to = 10, length = 3, suffix = "P", sep_suffix = "-", leading_zero = 2), c("08-P", "09-P", "10-P"))
  expect_equal(label_seq_length(length = 3, prefix = "P", sep_prefix = "-"), c("P-1", "P-2", "P-3"))

})
