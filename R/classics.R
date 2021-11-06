
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
make_classical <- function(.name = "", ..., .seed = sample(1000, 1),
                           .output = TRUE) {

  des <- do.call(code_classical, c(list(.name = .name, .seed = .seed,
                                        .quiet = TRUE), list2(...)))

  df <- eval(parse(text = ansi_strip(des$code)))

  if(isTRUE(.output) | "info" %in% .output) {
    cli_h1("experimental design details")
    cli_ul()
    cli_li("This experimental design is often called
           {.combine_words(des$name_full, and = ' or ', fun = style_italic)}.")
    cli_li("You can change the number in {.code set.seed} to get another random
           instance of the same design.")
    cli_li("This design has a total of
           {des$decorate_units(paste(nrow(df), 'units'))}
           testing a total of
           {des$decorate_trts(paste(des$t, 'treatments'))}.")
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
code_classical <- function(.name = "", .seed = sample(1000, 1),
                           ..., .quiet = FALSE) {

  des <- do.call(paste0("prep_classical_", .name), list2(...))
  des$add_seed(.seed)
  if(!.quiet) cat(des$code)
  invisible(des)
}

# randomly chose a design
#' @importFrom cli cli_alert
prep_classical_ <- function(...) {
  cli_alert("No name was supplied so selecting a random named experimental design...")
  .name <- sample(suppressMessages(find_classical_names()), 1L)
  do.call(paste0("prep_classical_", .name), list2(...))
}

#' Completely randomised design
#'
#' @param t The number of treatment levels
#' @param n The number of experimental units
#' @param r (Optional) The number of replicates.
#' @importFrom cli style_italic
#' @export
prep_classical_crd <- function(t = 1 + sample(10, 1), n = t + sample(100, 1),
                               r = n / t) {

  # checks
  if(!missing(n) & !missing(r)) {
    abort("You cannot define both `n` and `r`.")
  }
  if(missing(n) & !missing(r)) {
    n <- r * t
  }

  # des
  des <- NamedDesign$new(name = "crd",
                         name_full = "Completely Randomised Design",
                         n = n,
                         t = t)
  if(r %% 1 == 0) {
    des$add_info(paste0("This design is ", style_italic("balanced"),
                        " for the given numbers."),
                 always = FALSE)
  }

  des$add_code(paste0("set_units(",
                 des$decorate_units("unit"),
                 " = ", n, ")"))
  des$add_code(paste0("set_trts(",
                 des$decorate_trts("treat"),
                 " = ", t, ")"))
  des$add_code(paste0("allot_trts(",
                 des$decorate_trts("treat"),
                 " ~ ", des$decorate_units("unit"), ")"))
  des$final_code()

  des
}


prep_classical_factorial <- function(trt = c(1 + sample(10, 1),
                                             1 + sample(10, 1)),
                                     n = prod(trt) + sample(100, 1),
                                     r = n / prod(trt)) {

  # checks
  if(!missing(n) & !missing(r)) {
    abort("You cannot define both `n` and `r`.")
  }
  if(missing(n) & !missing(r)) {
    n <- r * prod(trt)
  }

  # des
  des <- NamedDesign$new(name = "factorial",
                         name_full = "Factorial Design",
                         n = n,
                         t = prod(trt))
  if(r %% 1 == 0) {
    des$add_info(paste0("This design is ", style_italic("balanced"),
                        " for the given numbers."),
                 always = FALSE)
  }

  des$add_code(paste0("set_units(",
                      des$decorate_units("unit"),
                      " = ", n, ")"))
  des$add_code(paste0("set_trts(",
                      paste0(map_chr(seq_along(trt), function(i)
                              paste0(des$decorate_trts(paste0("treat", i)),
                                     " = ", trt[i])),
                              collapse = ",\n           "),
                      ")"))
  des$add_code(paste0("allot_trts(",
                      " ~ ", des$decorate_units("unit"), ")"))
  des$final_code()

  des
}


#' @importFrom cli style_italic
prep_classical_rcbd <- function(t = 1 + sample(10, 1),
                                b = 1 + sample(10, 1), r = b, n = b * t) {

  if(sum(!missing(b), !missing(r), !missing(n)) > 1) {
    abort("Only one of `b`, `r` and `n` can be defined.")
  }
  if(missing(b) & !missing(r) & missing(n))  {
    n <- r * t
    b <- r
  }
  r <- n / t
  if(r %% 1 != 0) abort("The replication will not a whole number based on chosen `n` and `t`.")

  des <- NamedDesign$new(name = "rcbd",
                         name_full = "Randomised Complete Block Design",
                         n = n,
                         t = t)
  des$add_info(paste0("This design is ",
                      style_italic("balanced"),
                      " and ", style_italic("complete"), "."),
               always = TRUE)
  des$add_code(paste0("set_units(", des$decorate_units("block"), " = ",
                 b, ",\n",
                 "            ", des$decorate_units("unit"),
                 " = nested_in(", des$decorate_units("block"),
                 ", ", t, "))"))
  des$add_code(paste0("set_trts(",
                 des$decorate_trts("treat"),
                 " = ", t, ")"))
  des$add_code(paste0("allot_trts(",
                 des$decorate_trts("treat"),
                 " ~ ", des$decorate_units("unit"), ")"))
  des$final_code()
  des
}

#' @importFrom cli style_italic
prep_classical_split <- function(t1 = 1 + sample(10, 1),
                                 t2 = 1 + sample(10, 1),
                                  r = 1 + sample(10, 1)) {

  des <- NamedDesign$new(name = "split",
                         name_full = c("Split-Plot Design", "Split-Unit Design"),
                         n = t1 * t2 * r,
                         t = t1 * t2)
  des$add_info(paste0("This design is ", style_italic("balanced.")),
               always = TRUE)
  des$add_code(paste0("set_units(", des$decorate_units("mainplot"), " = ",
                 t1 * r, ",\n",
                 "            ", des$decorate_units("subplot"),
                 " = nested_in(", des$decorate_units("mainplot"),
                 ", ", t2, "))"))
  des$add_code(paste0("set_trts(",
                 des$decorate_trts("treat1"),
                 " = ", t1, ",\n           ",
                 des$decorate_trts("treat2"), " = ", t2,
                 ")"))
  des$add_code(paste0("allot_trts(",
                 des$decorate_trts("treat1"),
                 " ~ ", des$decorate_units("mainplot"), ",\n                ",
                 des$decorate_trts("treat2"),
                 " ~ ", des$decorate_units("subplot"), ")"))
  des$final_code()
  des
}


#prep_classical_factorial <- function(...) {
#  dots <- list2(...)
#}

#' An R6 Class for a named experimental design
#'
#' @importFrom rlang eval_tidy
NamedDesign <- R6::R6Class("NamedDesign",
     public = list(
       #' @field name The short name of the experimental design.
       name = NULL,

       #' @field name_full The full name of the experimental design.
       name_full = NULL,

       #' @field code The code, based on the grammar of experimental design,
       #'  to generate the experimental design.
       code = "",

       #' @field n The total number of experimental units.
       n = NULL,

       #' @field t The total number of treatments tested.
       t = NULL,

       #' @field info_always A character vector containing information or properties
       #' that is always true for this named design.  Each entry in the vector forms
       #' a dot point.
       info_always = NULL,

       #' @field info_this A character vector containing information or properties
       #' that is true for this named design for the chosen parameters.
       info_this = NULL,

       #' @description
       #' Initialise the named design.
       #' @param name Short name.
       #' @param name_full The full name of the design.
       #' @param seed The seed number.
       #' @param n Total number of experimental units.
       #' @param t Total number of treatments tested.
       initialize = function(name, name_full = name, n, t) {
        self$name <- name
        self$name_full <- name_full
        self$code <- paste0("start_design(\"", name, "\")")
        self$n <- n
        self$t <- t
       },

       #' @description Print of named design.
       #' @param ... Unused.
       print = function(...) {
         cat(cli::style_italic(self$name_full), "\n")
         cat(self$code)
         invisible(self)
       },

       #' @description
       #' Append code.
       #' @param x A string specifying the code to append.
       add_code = function(x) {
         self$code <- paste0(self$code, " %>%\n  ", x)
       },

       #' @description
       #' Add code to set seed at the beginning.
       #' @param seed The seed number.
       add_seed = function(seed) {
         self$code <- paste0("set.seed(", seed, ")\n", self$code)
       },

       #' @description
       #' Add information about the named design.
       #' @param x A string specifying one info about a design.
       #' @param always A logical to specify whether the added information is
       #'  always true for this this design or only for the given parameter.
       #'  Default is FALSE.
       add_info = function(x, always = FALSE) {
         if(always) {
           self$info_always <- c(self$info_always, x)
         } else {
           self$info_this <- c(self$info_this, x)
         }
       },

       #' @description
       #' This adds the final bit of code needed to generate the edibble
       #' data frame.
       final_code = function() {
         self$code <- paste0(self$code,
                " %>%\n  assign_trts(\"random\")",
                " %>%\n  serve_table()")
       },

       #' @description
       #' A function to decorate units.
       #' @param x A string to decorate.
       decorate_units = function(x) {
         edibble_decorate("units")(x)
       },

       #' @description
       #' A function to decorate treatments.
       #' @param x A string to decorate.
       decorate_trts = function(x) {
         edibble_decorate("trts")(x)
       }
     ))



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
