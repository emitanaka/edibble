

new_named_design <- function(name, name_full = name,
                             info_always = NULL,
                             info_this = NULL) {
  dname <- edibble_decorate("title")(paste0('"', name, '"'))
  structure(list(name = name,
                 name_full = name_full,
                 code = paste0("start_design(", dname, ")"),
                 info_always = info_always,
                 info_this = info_this),
            class = "named_design")
}

#' @export
print.named_design <- function(x, ...) {
  name_full <- edibble_decorate("title")(x$name_full)
  cat(cli::style_italic(paste(name_full, collapse = " | ")), "\n")
  cat(x$code)
}

#' Prepare a randomised complete block design
#'
#' @param t The number of treatments.
#' @param r The number of replicate blocks.
#' @param seed A scalar value for computational reproducibility.
#' @family named-designs
#' @export
prep_classical_rcbd <- function(t = 1 + sample(10, 1),
                                r = 1 + sample(10, 1),
                                seed = sample(1000, 1)) {
  des <- new_named_design(name = "rcbd",
                          name_full = "Randomised Complete Block Design")
  block <- edibble_decorate("units")("block")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")
  des$code <- sprintf('%s %%>%%
    set_units(%s = %d,
              %s = nested_in(%s, %d)) %%>%%
    set_trts(%s = %d) %%>%%
    allot_trts(%s ~ %s) %%>%%
    assign_trts("random", seed = %d) %%>%%
    serve_table()',
    des$code,
    block, r,
    unit, block, t,
    trt, t,
    trt, unit,
    seed)
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
prep_classical_crd <- function(t = sample(1:10, 1),
                               n = sample(t + 1:20, 1),
                               r = n / t,
                               seed = sample(1000, 1)) {
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

  des$code <- sprintf('%s %%>%%
    set_units(%s = %d) %%>%%
    set_trts(%s = %d) %%>%%
    allot_trts(%s ~ %s) %%>%%
    assign_trts("random", seed = %d) %%>%%
    serve_table()',
    des$code,
    unit, n,
    trt, t,
    trt, unit,
    seed)

  des
}



#' Prepare a factorial design
#'
#' @param trt A vector of the number of levels for each treatment factor.
#' @param design The unit structure: "crd" or "rcbd". The default is "crd".
#' @inheritParams prep_classical_rcbd
#' @family named-designs
#' @export
prep_classical_factorial <- function(trt = c(sample(1:10, 1),
                                             sample(1:10, 1)),
                                     r = sample(1:10, 1),
                                     design = c("crd", "rcbd"),
                                     seed = sample(1000, 1)) {
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
    collapse = ",\n             ")

  des$code <- sprintf('%s %%>%%
    set_units(%s) %%>%%
    set_trts(%s) %%>%%
    allot_trts(~%s) %%>%%
    assign_trts("random", seed = %d) %%>%%
    serve_table()',
    des$code,
    unit_str,
    trt_str,
    unit,
    seed)

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
prep_classical_split <- function(t1 = sample(1:10, 1),
                                 t2 = sample(1:10, 1),
                                 r = sample(1:10, 1),
                                 seed = sample(1000, 1)) {

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

  des$code <- sprintf('%s %%>%%
  set_units(%s = %d,
            %s = nested_in(%s, %d)) %%>%%
  set_trts(%s = %d,
           %s = %d) %%>%%
  allot_trts(%s ~ %s,
             %s ~ %s) %%>%%
  assign_trts("random", seed = %d) %%>%%
  serve_table()',
  des$code,
  mainplot, t1 * r,
  subplot, mainplot, t2,
  trt1, t1,
  trt2, t2,
  trt1, mainplot,
  trt2, subplot,
  seed)

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
#' @param .quiet Opposite of `.code`. Whether to suppress code output.
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
    cli_li("You can change the number in {.code set.seed} to get another random
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

