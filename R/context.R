

#' Describe context related to experiment
#'
#' @description
#' This is just a function that stores a simple context. If the
#' context already exists then it will be overwritten.
#'
#' @param .data An edibble design.
#' @param ... Name-value pairs.
#' @param .overwrite If the context
#' @examples
#' start_design("COVID-19") %>%
#'   add_context(question = "Does Pfizer vaccine work?",
#'                  where = "Tested in lab")
#'
#' @export
add_context <- function(.data, ..., .overwrite = TRUE) {
  new_context <- list2(...)
  current_context <- .data$context
  overlapping_names <- intersect(names(new_context), names(current_context))
  if(.overwrite) {
    current_context[overlapping_names] <- new_context[overlapping_names]
  }
  if(!is_empty(overlapping_names)) {
    s <- ifelse(length(overlapping_names) > 1, "s", "")
    msg <- paste0("The context", s,", ", .combine_words(overlapping_names, fun = cli::style_italic),
                  ", already exist and have been ")
    if(.overwrite) {
      warn(paste0(msg, cli::style_bold("ovewritten.")))
    } else {
      warn(paste0(msg, cli::style_bold("ignored.")))
    }
  }
  new_context <- new_context[setdiff(names(new_context), overlapping_names)]

  .data$context <- c(current_context, new_context)
  .data
}




