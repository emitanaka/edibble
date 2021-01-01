

#' Describe context related to experiment
#'
#' @description
#' This is just a function that stores a simple context. If the
#' context already exists then it will be overwritten.
#'
#' @param .nexus An edibble nexus.
#' @param ... Name-value pairs.
#' @param .overwrite If the context
#' @examples
#' initiate_design("COVID-19") %>%
#'   describe_context(question = "Does Pfizer vaccine work?",
#'                    where = "Tested in lab")
#'
#' @export
add_context <- function(.nexus, ..., .overwrite = TRUE) {
  new_context <- list2(...)
  current_context <- igraph::graph_attr(.nexus, "context") %||% list()
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

  out <-  igraph::set_graph_attr(.nexus, "context",
                                 c(current_context, new_context))

  structure(out, class = class(.nexus))
}
