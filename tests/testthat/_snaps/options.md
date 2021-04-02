# nexus print decorations

    Code
      old_opts <- options()
    Code
      options(edibble.decorate_trts = cli::bg_cyan, edibble.decorate_units = function(
        x) cli::bg_yellow(glue::glue("<{x}>")), edibble.decorate_resp = cli::
        col_yellow, edibble.decorate_levels = function(x) cli::style_inverse(glue::
        glue("({x})")), edibble.decorate_title = cli::style_underline)
    Code
      classics_nexus$RCBD
    Error <simpleError>
      object 'classics_nexus' not found
    Code
      options(old_opts)

---

    Code
      old_opts <- options()
    Code
      options(edibble.labels.leading_zero = TRUE, edibble.labels.min_ndigits = 0)
    Code
      options(old_opts)

