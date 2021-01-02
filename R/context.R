

#' Describe context related to experiment
#'
#' @description
#' This is just a function that stores a simple context. If the
#' context already exists then it will be overwritten.
#'
#' @param .data An edibble graph.
#' @param ... Name-value pairs.
#' @param .overwrite If the context
#' @importFrom rlang list2 %||% is_empty is_null
#' @importFrom igraph graph_attr set_graph_attr
#' @examples
#' initiate_design("COVID-19") %>%
#'   add_context(question = "Does Pfizer vaccine work?",
#'                    where = "Tested in lab")
#'
#' @export
add_context <- function(.data, ..., .overwrite = TRUE) {
  new_context <- list2(...)
  current_context <- graph_attr(.data, "context") %||% list()
  overlapping_names <- intersect(names(new_context), names(current_context))
  if(.overwrite) {
    current_context[overlapping_names] <- new_context[overlapping_names]
  }
  if(!is_emtpy(overlapping_names)) {
    if(.overwrite) {
      warn("Some contexts already exist and have been ovewritten.")
    } else {
      warn("Some contexts already exist and have been ignored.")
    }
  }
  new_context <- new_context[setdiff(names(new_context), overlapping_names)]

  out <-  set_graph_attr(.data, "context",
                                 c(current_context, new_context))

  reset_graph_attr(out, .data)
}



#' @importFrom igraph E make_empty_graph set_graph_attr
#' @importFrom rlang %||% eval_tidy f_lhs f_rhs
#' @importFrom vctrs vec_as_names new_vctr
EdibbleDesign <- R6::R6Class("EdibbleDesign",
       public = list(
         initialize = function(name = NULL) {
           private$.name <- name %||% "An edibble design"
           private$.graph <- structure(make_empty_graph(),
                                      class = c("edbl_graph", "igraph"))
         },

         set_vars = function(..., .class, .name_repair) {
           private$.graph <- set_vars(private$.graph, ..., .class = .class,
                                      .name_repair = .name_repair)
           self
         },

         allocate_trts = function(..., class) {
           private$.graph <- allocate_trts_ext(private$.graph, ..., class = class)
           self
         },

         randomise_trts = function() {
           private$.graph <- randomise_trts(private$.graph)
           self
         },

         serve_table = function(...) {
           private$.table <- serve_table(private$.graph, ...)
           private$.active <- "table"
           self
         },

         plot = function(...) {
           plot(private$.graph, ...)
           invisible(self)
         },

         print = function(...) {
           if(private$.active == "graph") {
             print(private$.graph, main = private$.name)
           } else if(private$.active == "table") {
             print(private$.table)
           }
           invisible(self)
         }
       ),

       active  = list(
          name = function(value) {
            if(missing(value)) {
              private$.name
            } else {
              private$.name <- value
            } },

          graph = function(value) {
            if(missing(value)) {
              private$.graph
            } else {
              private$.graph <- value
            } },

          table = function(value) {
            if(missing(value)) {
              serve_table(private$.graph)
            } else {
              abort("You cannot modify table")
            } }
       ),
       private = list(
         .name = "",
         .graph = NULL,
         .table = NULL,
         .active = "graph",
         .class = NULL
       ))
