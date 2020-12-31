test_that("nexus print decorations", {
  expect_snapshot({
    old_opts <- options()
    options(edibble.decorate_trts = cli::bg_cyan,
            edibble.decorate_units = function(x) cli::bg_yellow(glue::glue("<{x}>")),
            edibble.decorate_resp = cli::col_yellow,
            edibble.decorate_levels = function(x) cli::style_inverse(glue::glue("({x})")),
            edibble.decorate_title = cli::style_underline)
    classics_nexus$RCBD

    options(old_opts)
  })

  expect_snapshot({
    old_opts <- options()
    options(edibble.labels.leading_zero = TRUE,
            edibble.labels.min_ndigits = 0)

    options(old_opts)

  })
})

