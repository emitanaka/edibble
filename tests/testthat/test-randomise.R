test_that("randomisation", {
  tab <- start_design(name = "response") %>%
    set_units(class = 3,
              person = nested_in(class, 30)) %>%
    set_trts(style = c("flipped", "traditional")) %>%
    allocate_trts(~person) %>%
    randomise_trts() %>%
    serve_table()

  counts <- tab %>%
    dplyr::group_by(class, style) %>%
    dplyr::tally() %>%
    dplyr::pull(n)

  expect_true(all(counts==15))
})
