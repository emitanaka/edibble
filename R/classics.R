
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
#' @param code Show the edibble code for generating the design. Default is
#'  TRUE.
#'
#' @details
#'
#' For The available named designs are:
#'
#' * "crd": completely randomised design
#' * "rcbd": randomised complete block design
#'
#' @export
create_classics <- function(name = c("crd", "rcbd", "spd"),
                            r = NULL, t = NULL, n = NULL,
                            t1 = t, t2 = NULL, b = r,
                            code = TRUE, seed = 2020) {
  fn_args <- as.list(match.call())[-1]

  name <- arg_match(name)
  des <- NamedDesign$new(name, seed, fn_args)
  code_text <- switch(name,
                crd = {
                  des$args_req("t")
                  des$args_opt("r" = n / t, "n" = r * t)
                  des$add(paste0("set_units(unit = ", des$args$n, ")"))
                  des$add(paste0("set_trts(treat = ", des$args$t, ")"))
                  des$add("allocate_trts(treat ~ unit)")
                  des$final()
                },
                rcbd = {
                  des$args_req("t")
                  # need OR
                  # what if not divisible??
                  des$args_opt("r" = n / t, "b" = n / t, "n" = r * t)
                  des$add(paste0("set_units(block = ", des$args$b, ",\n",
                                 "          unit = nested_in(block, ", des$args$t, "))"))
                  des$add(paste0("set_trts(treat = ", t, ")"))
                  des$add("allocate_trts(treat ~ unit)")
                  des$final()
                })

  if(code) cat(code_text)

  return(invisible(eval(parse(text = code_text))))
}



NamedDesign <- R6::R6Class("NamedDesign",
                           public = list(
                             initialize = function(name, seed, args) {
                              self$name <- name
                              self$code <- paste0("set.seed(", seed, ")\n",
                                                  "start_design(\"", name, "\")")
                              self$args <- args
                             },

                             add = function(line) {
                               self$code <- paste0(self$code, " %>%\n  ", line)
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
                                 self$args[names(one)] <- val
                               }
                             },

                             final = function() {
                               paste0(self$code,
                                      " %>%\n  randomise_trts()",
                                      " %>%\n  serve_table()")
                             },

                             name = NULL,
                             code = "",
                             args = NULL
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
