
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
                            t1 = t, t2 = NULL,
                            code = TRUE) {
  des_args <- list(r = r, t = t, n = n, t1 = t1, t2 = t2)
  code_text <- switch(name,
                crd = {
                  check_args(des_args, one = c("r", "n"), required = "t")
                  glue::glue("
                    start_design(\"{name}\") %>%
                      set_units(unit = {n}) %>%
                      set_trts(treat = {t}) %>%
                      allocate_trts(treat ~ unit) %>%
                      randomise_trts() %>%
                      serve_table()
                  ")
                },
                rcbd = {
                  check_args(des_args, one = c("r", "n"), required = c("t", "b"))
                  glue::glue("
                    start_design(\"{name}\") %>%
                      set_units(block = {b},
                                unit = nested_in(block, {r})) %>%
                      set_trts(treat = {t}) %>%
                      allocate_trts(treat ~ unit) %>%
                      randomise_trts() %>%
                      serve_table()
                  ")
                })

  if(code) cat(code_text)

  return(invisible(eval(parse(text = code_text))))
}

NamedDesign <- R6::R6Class("NamedDesign",
                           public = list(
                             initialize = function(name) {
                              private$name <- name
                              private$code <- paste0("start_design(\"", name, "\")")},
                             name = NULL,
                             code = ""
                           ))

check_args <- function(args, one, required) {
  arg_names <- names(remove_nulls(args))
  if(sum(arg_names %in% one)!=1) abort("Unnecessary arguments defined.")
  if(!all(required %in% arg_names)) abort("Required arguments are missing.")
}


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
