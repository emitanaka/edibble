

new_named_design <- function(name, name_full = name,
                             info_always = NULL,
                             info_this = NULL) {
  dname <- edibble_decorate("title")(paste0('"', paste(name_full, collapse = " | "), '"'))
  structure(list(name = name,
                 name_full = name_full,
                 code = paste0("start_design(", dname, ")"),
                 info_always = info_always,
                 info_this = info_this),
            class = "named_design")
}

named_design_add_code <- function(.named, ...) {
  dots <- list2(...)
  for(x in dots) {
    .named$code <- sprintf('%s %%>%%
  %s', .named$code, x)
  }
  .named
}

#' @export
print.named_design <- function(x, ...) {
  #name_full <- edibble_decorate("title")(x$name_full)
  #cat(cli::style_italic(paste(name_full, collapse = " | ")), "\n")
  cat(x$code)
}

random_integer_small <- function(min = 1) min + sample(10, 1)
random_integer_medium <- function(min = 1) min + sample(10:25, 1)
random_seed_number <- function() sample(1000, 1)

#' Prepare a randomised complete block design
#'
#' @param t The number of treatments.
#' @param r The number of replicate blocks.
#' @param seed A scalar value for computational reproducibility.
#' @family named-designs
#' @export
prep_classical_rcbd <- function(t = random_integer_small(),
                                r = random_integer_small(),
                                seed = random_seed_number()) {

  des <- new_named_design(name = "rcbd",
                          name_full = "Randomised Complete Block Design")
  block <- edibble_decorate("units")("block")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")
  des <- named_design_add_code(des,
        sprintf('set_units(%s = %d,
            %s = nested_in(%s, %d))', block, r, unit, block, t),
        sprintf('set_trts(%s = %d)', trt, t),
        sprintf('allot_trts(%s ~ %s)', trt, unit),
        sprintf('assign_trts("random", seed = %d)', seed),
        'serve_table()')
  des
}

#' Graeco-Latin Square Design
#'
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @export
prep_classical_graeco <- function(t = random_integer_small(),
                                  seed = random_seed_number()) {
  des <- new_named_design(name = "graeco",
                          name_full = "Graeco-Latin Square Design")

  row <- edibble_decorate("units")("row")
  column <- edibble_decorate("units")("collumn")
  unit <- edibble_decorate("units")("unit")
  trt1 <- edibble_decorate("trts")("trt1")
  trt2 <- edibble_decorate("trts")("trt2")

  des <- named_design_add_code(des,
                              sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, column, t, unit, row, column),
            sprintf('set_trts(%s = %d,
           %s = %d)', trt1, t, trt2, t),
            sprintf('allot_trts(%s ~ %s,
             %s ~ %s)', trt1, unit, trt2, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}


#' Completely randomised design
#'
#' @param t The number of treatment levels
#' @param n The number of experimental units
#' @param r (Optional) The number of replicates.
#' @family named-designs
#' @inheritParams prep_classical_rcbd
#' @export
prep_classical_crd <- function(t = random_integer_small(),
                               n = random_integer_medium(min = t),
                               r = n / t,
                               seed = random_seed_number()) {

  # checks
  if(!missing(n) & !missing(r)) {
    abort("You cannot define both `n` and `r`.")
  }
  if(missing(n) & !missing(r)) {
    n <- r * t
  }

  des <- new_named_design(name = "crd",
                          name_full = "Completely Randomised Design")

  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- named_design_add_code(des,
                               sprintf('set_units(%s = %d)', unit, n),
                               sprintf('set_trts(%s = %d)', trt, t),
                               sprintf('allot_trts(%s ~ %s)', trt, unit),
                               sprintf('assign_trts("random", seed = %d)', seed),
                               'serve_table()')

  des
}



#' Prepare a factorial design
#'
#' @param trt A vector of the number of levels for each treatment factor.
#' @param design The unit structure: "crd" or "rcbd". The default is "crd".
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @export
prep_classical_factorial <- function(trt = c(random_integer_small(),
                                             random_integer_small()),
                                     r = random_integer_small(),
                                     design = c("crd", "rcbd"),
                                     seed = random_seed_number()) {
  design <- match.arg(design)
  des <- new_named_design(name = "factorial",
                          name_full = paste0("Factorial Design",
                                             switch(design,
                                                    "crd" = "",
                                                    "rcbd" = " with RCBD structure")))
  unit <- edibble_decorate("units")("unit")
  block <- edibble_decorate("units")("block")
  ntrt <- prod(trt)
  unit_str <- switch(design,
                     "crd" = sprintf('%s = %d', unit, ntrt * r),
                     "rcbd" = sprintf('%s = %d,
             %s = nested_in(%s, %d)',
                                      block, r,
                                      unit, block, ntrt))
  trt_str <- paste0(map_chr(seq_along(trt), function(i)
    paste0(edibble_decorate("trts")(paste0("trt", i)),
           " = ", trt[i])),
    collapse = ",\n           ")

  des <- named_design_add_code(des,
                                sprintf('set_units(%s)', unit_str),
                                sprintf('set_trts(%s)', trt_str),
                                sprintf('allot_trts(~%s)', unit),
                                sprintf('assign_trts("random", seed = %d)', seed),
                                'serve_table()')

  des
}


#' Prepare classical split plot design
#'
#' @param t1 The number of treatment levels for the main plots.
#' @param t2 The number of treatment levels for the subplots.
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @importFrom cli style_italic
#' @export
prep_classical_split <- function(t1 = random_integer_small(),
                                 t2 = random_integer_small(),
                                 r = random_integer_small(),
                                 seed = random_seed_number()) {

  n <- t1 * t2 * r
  des <- new_named_design(name = "split",
                          name_full = c("Split-Plot Design",
                                        "Split-Unit Design"))
  des$info_always <- c(des$info_always,
                       paste0("This design is ", style_italic("balanced.")))

  mainplot <- edibble_decorate("units")("mainplot")
  subplot <- edibble_decorate("units")("subplot")
  trt1 <- edibble_decorate("trts")("trt1")
  trt2 <- edibble_decorate("trts")("trt2")

  des <- named_design_add_code(des,
                               sprintf('set_units(%s = %d,
             %s = nested_in(%s, %d))', mainplot, t1 * r, subplot, mainplot, t2),
              sprintf('set_trts(%s = %d,
           %s = %d)', trt1, t1, trt2, t2),
              sprintf('allot_trts(%s ~ %s,
             %s ~ %s)', trt1, mainplot, trt2, subplot),
              sprintf('assign_trts("random", seed = %d)', seed),
              'serve_table()')

  des
}

#' Youden square design
#'
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @importFrom cli style_italic
#' @export
prep_classical_youden <- function(nc = random_integer_small(),
                                  t = random_integer_small(min = nc + 1),
                               seed = random_seed_number()) {
  des <- new_named_design(name = "youden",
                          name_full = "Youden Square Design")

  row <- edibble_decorate("units")("row")
  column <- edibble_decorate("units")("column")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- named_design_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, column, nc, unit, row, column),
            sprintf('set_trts(%s = %d)', trt, t),
            sprintf('allot_trts(%s ~ %s)', trt, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}



#' Prepare classical Latin square design
#'
#' @param t The number of treatments
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @importFrom cli style_italic
#' @export
prep_classical_lsd <- function(t = random_integer_small(),
                               seed = random_seed_number()) {
  des <- new_named_design(name = "lsd",
                          name_full = "Latin Square Design")
  des$info_always <- c(des$info_always,
                       paste0("This design is ", style_italic("balanced.")))

  row <- edibble_decorate("units")("row")
  column <- edibble_decorate("units")("column")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- named_design_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, column, t, unit, row, column),
             sprintf('set_trts(%s = %d)', trt, t),
           sprintf('allot_trts(%s ~ %s)', trt, unit),
           sprintf('assign_trts("random", seed = %d)', seed),
           'serve_table()')

  des
}


#' Find the short names of the classical named designs
#'
#' @param pkgs A character vector containing the package names to search classical
#' named designs from. By default it will search edibble and other packages loaded.
#'
#' @export
find_classical_designs <- function(pkgs = NULL) {
  # ignore searching in base pkgs
  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets",
                 "methods", "base")
  pkgs <- pkgs %||% setdiff(.packages(), base_pkgs)
  pkgs <- unique(c(pkgs, "edibble")) # always add edibble whether it is loaded or not

  ls_fns <- lapply(pkgs, function(pkg) {
    fns <- unclass(lsf.str(envir = asNamespace(pkg), all = TRUE))
    setdiff(fns[grep("^prep_classical_", fns)], "prep_classical_")
  })
  names(ls_fns) <- pkgs
  ls_fns <- compact(ls_fns)

  pkg_names <- names(ls_fns)
  short_names <- NULL
  for(i in seq_along(ls_fns)) {
    cli_h2(pkg_names[i])
    for(prep_fn in ls_fns[[i]]) {
      args <- as.list(formals(prep_fn))
      des <- do.call(prep_fn, list())
      short_names <- c(short_names, set_names(des$name, pkg_names[i]))
      cli_li("{.pkg {des$name}} with the arguments {.field {names(args)}}
             for a {.combine_words(des$name_full, fun = style_bold, and = ' or a ')}.")
    }
  }
  invisible(short_names)
}


#' Randomly chose a design
#' @importFrom cli cli_alert
#' @export
prep_classical_ <- function(...) {
  cli_alert("No name was supplied so selecting a random named experimental design...")
  .name <- sample(suppressMessages(find_classical_designs()), 1L)
  do.call(paste0("prep_classical_", .name), list2(...))
}



#' Create a classical named experimental design
#'
#' @description
#' The function `make_classical` generates a classical named experimental
#' design by supplying its short name and prints out, by default:
#'
#' * `info`: information about the named experimental design,
#' * `code`: code to create the design using edibble, and
#' * `table`: an edibble data frame for the generated design.
#'
#' You can find the available short names with `find_classical_names()`.
#'
#'
#' @param .name The short name of the classical named experimental design. See
#'   under Details for the available named designs.
#' @param .output A logical value to indicate whether all output should be
#'  printed or not or a vector of character (e.g. `c("info", "code", "table")`) specifying which of the three
#'  outputs should be printed. Default is TRUE.
#' @param ... Parameters passed into the `prep_classical_*` functions.
#' @details
#'
#' For The available named designs are:
#'
#' * "crd": completely randomised design
#' * "rcbd": randomised complete block design
#' * "split": split plot design
#'
#' @examples
#' make_classical("crd", n = 50, t = 5)
#' # if you omit the design parameters then it will use the default
#' # (which may be random)
#' make_classical("rcbd")
#' # if you don't give any short names then it will generate a random one
#' make_classical()
#' @seealso See [find_classical_names()] for finding the short names of the
#'  named experimental designs.
#'
#' @importFrom cli cli_h1 cli_ul cli_end cli_h2 col_grey style_italic ansi_strip
#' @export
make_classical <- function(.name = "", ...,
                           .output = TRUE) {

  des <- do.call(code_classical, c(list(.name = .name,
                                        .quiet = TRUE), list2(...)))

  df <- eval(parse(text = ansi_strip(des$code)))

  if(isTRUE(.output) | "info" %in% .output) {
    cli_h1("experimental design details")
    cli_ul()
    cli_li("This experimental design is often called
           {.combine_words(des$name_full, and = ' or ', fun = style_italic)}.")
    cli_li("You can change the number in {.code seed} to get another random
           instance of the same design.")
    if(!is_empty(des$info_always)) {
      cli_li(des$info_always)
    } else if(!is_empty(des$info_this)) {
      cli_end()
      cli_text(col_grey("The following information is only true for the
                        chosen parameters and not necessary true for all
                        {des$name_full}s."))
      cli_ul()
      cli_li(des$info_this)
    }
    cli_end()
  }

  if(isTRUE(.output) | "code" %in% .output) {
    cli_h1("edibble code")
    cat(des$code, "\n")
  }


  if(isTRUE(.output) | "table" %in% .output) {
    cli_h1("edibble data frame")
    print(df)
  }

  return(invisible(df))
}

#' @rdname make_classical
#' @export
code_classical <- function(.name = "",
                           ..., .quiet = FALSE) {

  des <- do.call(paste0("prep_classical_", .name), list2(...))
  if(!.quiet) cat(des$code)
  invisible(des)
}

