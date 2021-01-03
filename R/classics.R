
#' Create some classical named experimental designs
#'
#' @description
#' This function creates some classical experimental designs.
#'
#' @param name The name of the classic experiment design. See
#'   under Details for the available named designs.
#' @param t The number of treatments testing.
#' @param r The total number of replicates if applicable.
#' @param b The total number of blocks if applicable.
#' @param n The total number of observations if applicable.
#' @param t1 The number of levels for the first treatment factor.
#' @param t2 The number of levels for the second treatment factor.
#' @param output Show the info, code and print outputs? Default is TRUE.
#' @param info Show information on design. Default is TRUE.
#' @param code Show the edibble code for generating the design. Default is
#'  TRUE.
#' @param print Show the print out of the return object. Default is TRUE.
#'
#' @details
#'
#' For The available named designs are:
#'
#' * "crd": completely randomised design
#' * "rcbd": randomised complete block design
#'
#' @importFrom cli cli_h1 cli_text cli_end cli_li cli_ul style_bold style_italic ansi_strip col_magenta
#' @export
create_classic <- function(name = c("crd", "rcbd", "split"),
                            r = NULL, t = NULL, n = NULL,
                            t1 = t, t2 = NULL, b = r, seed = 2020,
                            output = TRUE,
                            code = output, info = output, print = output, ...) {
  fn_args <- as.list(match.call())[-1]
  des <- do.call(create_classic_code, c(fn_args, list(quiet = TRUE)))

  if(info) {
    cli_h1("experimental design details")
    cli_ul()
    cli_li("This experimental design is often called
           {.emph {des$name_full}}.")
    cli_li("You can change the number in {.code set.seed} to get another random
           instance of the same design.")
    cli_li("This design has a total of
           {des$decorate_units(paste(des$args$n, 'units'))}
           testing a total of
           {des$decorate_trts(paste(des$args$t, 'treatments'))}.")
    cli_li(des$info)
    cli_end()
  }

  if(code) {
    cli_h1("edibble code")
    cat(des$code)
  }

  df <- eval(parse(text = ansi_strip(des$code)))

  if(print) {
    cli_h1("edibble data frame")
    print(df)
  }

  return(invisible(df))
}

#' @rdname create_classic
#' @export
create_classic_code <- function(name = c("crd", "rcbd", "spd"),
                                 r = NULL, t = NULL, n = NULL,
                                 t1 = t, t2 = NULL, b = r,
                                 seed = 2020, ..., quiet = FALSE) {
  fn_args <- as.list(match.call())[-1]
  if(!"seed" %in% names(fn_args)) fn_args <- c(fn_args, list(seed = seed))
  des <- do.call(paste0("classical_", name), fn_args)
  if(!quiet) cat(des$code)
  invisible(des)
}

classical_crd <- function(name, seed, t, r, n, ...) {
  fn_args <- as.list(match.call())[-1]
  des <- NamedDesign$new(name, seed, fn_args)
  des$add_info(paste0("This design is balanced."))
  des$name_full <- "Completely Randomised Design"
  des$args_req("t")
  des$args_opt("r" = n / t, "n" = r * t)
  des$add(paste0("set_units(",
                 des$decorate_units("unit"),
                 " = ", des$args$n, ")"))
  des$add(paste0("set_trts(",
                 des$decorate_trts("treat"),
                 " = ", des$args$t, ")"))
  des$add(paste0("allocate_trts(",
                 des$decorate_trts("treat"),
                 " ~ ", des$decorate_units("unit"), ")"))
  des$final()
  des
}

classical_rcbd <- function(name, seed, t, r, b, n, ...) {
  fn_args <- as.list(match.call())[-1]
  des <- NamedDesign$new(name, seed, fn_args)
  des$name_full <- "Randomised Complete Block Design"
  des$add_info(paste0("This design is ",
                      style_italic("balanced"),
                      " and ", style_italic("complete"), "."))
  des$args_req("t")
  # need OR
  # what if not divisible??
  des$args_opt("r" = n / t, "b" = n / t, "n" = b * t)
  des$add(paste0("set_units(", des$decorate_units("block"), " = ",
                 des$args$b, ",\n",
                 "            ", des$decorate_units("unit"),
                 " = nested_in(", des$decorate_units("block"),
                 ", ", des$args$t, "))"))
  des$add(paste0("set_trts(",
                 des$decorate_trts("treat"),
                 " = ", des$args$t, ")"))
  des$add(paste0("allocate_trts(",
                 des$decorate_trts("treat"),
                 " ~ ", des$decorate_units("unit"), ")"))
  des$final()
  des
}

classical_split <- function(name, seed, t1, t2, r, ...) {
  fn_args <- as.list(match.call())[-1]
  des <- NamedDesign$new(name, seed, fn_args)
  des$args$n <- t1 * t2 * r
  des$args$t <- t1 * t2
  des$add_info(paste0("This design is ", style_italic("balanced.")))
  des$name_full <- c("Split-Plot Design", "Split-Unit Design")
  des$args_req(c("t1", "t2", "r"))
  des$add(paste0("set_units(", des$decorate_units("mainplot"), " = ",
                 des$args$t1 * des$args$r, ",\n",
                 "            ", des$decorate_units("subplot"),
                 " = nested_in(", des$decorate_units("mainplot"),
                 ", ", des$args$t2, "))"))
  des$add(paste0("set_trts(",
                 des$decorate_trts("treat1"),
                 " = ", des$args$t1, ",\n           ",
                 des$decorate_trts("treat2"), " = ", des$args$t2,
                 ")"))
  des$add(paste0("allocate_trts(",
                 des$decorate_trts("treat1"),
                 " ~ ", des$decorate_units("mainplot"), ",\n                ",
                 des$decorate_trts("treat2"),
                 " ~ ", des$decorate_units("subplot"), ")"))
  des$final()
  des
}


NamedDesign <- R6::R6Class("NamedDesign",
                           public = list(
                             initialize = function(name, seed, args) {
                              self$name <- name
                              self$name_full <- name
                              self$code <- paste0("set.seed(", seed, ")\n",
                                                  "start_design(\"", name, "\")")
                              self$args <- args
                             },

                             add = function(line) {
                               self$code <- paste0(self$code, " %>%\n  ", line)
                             },

                             add_info = function(x) {
                               self$info <- c(self$info, x)
                             },

                             args_req = function(x = NULL) {
                               if(!is_null(x) && !all(x %in% names(self$args))) {
                                 abort("Required arguments are missing.")
                               }
                             },

                             args_opt = function(...) {
                               vals <- enquos(...)
                               ones <- names(vals)
                               arg_names <- names(self$args)
                               if(!is_null(ones) && sum(arg_names %in% ones)!=1) {
                                 abort("Unnecessary arguments defined.")
                               } else {
                                 one <- vals[!ones %in% arg_names]
                                 val <- eval_tidy(one[[1]], self$args)
                                 if(val %% 1 !=0 ) {
                                   abort(paste0("The supplied argument cannot be used to",
                                         " construct \"", self$name, "\"."))
                                 }
                                 self$args[names(one)] <- val
                               }
                             },

                             final = function() {
                               self$code <- paste0(self$code,
                                      " %>%\n  randomise_trts()",
                                      " %>%\n  serve_table()")
                             },

                             decorate_units = function(x) {
                               edibble_decorate("units")(x)
                             },

                             decorate_trts = function(x) {
                               edibble_decorate("trts")(x)
                             },

                             name = NULL,
                             name_full = NULL,
                             code = "",
                             args = NULL,
                             info = NULL
                           ))

#' A list of classical experimental designs
#'
#' @format A list of some classical experimental designs as either `edbl_graph`
#' or `edbl_df` object. The designs included are:
#' \describe{
#'   \item{CRD} Completely randomised design.
#'   \item{RCBD} Randomised complete block design.
#'   \item{SPD} Split plot design.
#' }
#' @name classics
"nclassics"


#' @rdname classics
"classics"
