

#' @export
prep_classical_factorial <- function(trt = c(random_integer_small(),
                                             random_integer_small()),
                                     r = random_integer_small(),
                                     design = c("crd", "rcbd"),
                                     seed = random_seed_number()) {
  warn("`prep_classical_factorial` is deprecated. Please use `menu_factorial` instead.")
  menu_factorial(t, n, r, seed)
}
#' Create a classical named experimental design
#'
#' @description
#'
#' This function is defunct. Please use `takeout` instead.
#'
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
    cli_h2("experimental design details")
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
    if(isTRUE(.output) | length(.output) > 1) cli_h2("edibble code")
    cat(des$code, "\n")
  }


  if(isTRUE(.output) | "table" %in% .output) {
    if(isTRUE(.output) | length(.output) > 1) cli_h2("edibble data frame")
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


#' Randomly chose a design
#' @importFrom cli cli_alert
#' @export
prep_classical_ <- function(...) {
  cli_alert("No name was supplied so selecting a random named experimental design...")
  .name <- sample(suppressMessages(find_classical_designs()), 1L)
  do.call(paste0("prep_classical_", .name), list2(...))
}


#' @export
find_classical_designs <- function(pkgs = NULL) {
  warn("This function is defunct. Use `scan_menu` instead.")
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


#' @export
prep_classical_lsd <- function(t = random_integer_small(),
                               seed = random_seed_number())  {
  warn("`prep_classical_lsd` is deprecated. Please use `menu_lsd` instead.")
  menu_lsd(t, seed)
}

#' @export
prep_classical_youden <- function(nc = random_integer_small(),
                                  t = random_integer_small(min = nc + 1),
                                  seed = random_seed_number()) {
  warn("`prep_classical_youden` is deprecated. Please use `menu_youden` instead.")
  menu_youden(nc, t, seed)
}


#' @export
prep_classical_split <- function(t1 = random_integer_small(),
                                 t2 = random_integer_small(),
                                 r = random_integer_small(),
                                 seed = random_seed_number()){
  warn("`prep_classical_split` is deprecated. Please use `menu_split` instead.")
  menu_split(t1, t2, r, seed)
}

#' @export
prep_classical_crd <- function(t = random_integer_small(),
                               n = random_integer_medium(min = t),
                               r = NULL,
                               seed = random_seed_number()) {
  warn("`prep_classical_crd` is deprecated. Please use `menu_crd` instead.")
  menu_crd(t, n, r, seed)
}

#' @export
prep_classical_graeco <- function(t = random_integer_small(),
                                  seed = random_seed_number()) {
  warn("`prep_classical_graeco` is deprecated. Please use `menu_graeco` instead.")
  menu_graeco(t, seed)
}

#' @export
prep_classical_rcbd <- function(t = random_integer_small(),
                                r = random_integer_small(),
                                seed = random_seed_number()) {
  warn("`prep_classical_rcbd` is deprecated. Please use `menu_rcbd` instead.")
  menu_rcbd(t, r, seed)
}


#' @export
allocate_trts <- function(design, ...) {
  warn("`allocate_trts` is deprecated. Please use `allot_trts` instead.")
  allot_trts(design, ...)
}

#' @export
randomise_trts <- function(design, ...) {
  warn("`randomise_trts` is deprecated. Please use `assign_trts(\"random\")` instead.")
  assign_trts(design, order = "random", ...)
}
