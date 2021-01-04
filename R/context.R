

#' Describe context related to experiment
#'
#' @description
#' This is just a function that stores a simple context. If the
#' context already exists then it will be overwritten.
#'
#' @param .data An edibble design.
#' @param ... Name-value pairs where name is the context name and value is context
#'  in string. Unnamed values will appended as points at the end.
#' @param .overwrite If the named context already exissts, it will overwrite.
#'  By default it is TRUE.
#' @examples
#' start_design("COVID-19") %>%
#'   add_context(question = "Does Pfizer vaccine work?",
#'                  where = "Tested in lab")
#' @seealso [sort_context()] for sorting context alphabetically.
#' @export
add_context <- function(.data, ..., .overwrite = TRUE) {
  new_context <- list2(...)
  current_context <- .data$context
  overlapping_names <- intersect(names(new_context), names(current_context))
  if(.overwrite) {
    ind <- which(names(current_context) %in% overlapping_names)
    if(!is_empty(ind)) {
      current_context <- current_context[-ind]
    }
  } else {
    ind <- which(names(new_context) %in% overlapping_names)
    if(!is_empty(ind)) {
      new_context <- new_context[-ind]
    }
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


  .data$context <- c(current_context, new_context)
  .data
}


#' Context manipulations
#'
#' @name context-manipulations
#' @param .data An edibble design object.
NULL

#' @rdname context-manipulations
#' @export
sort_context <- function(.data, ...) {
  context <- .data$context
  context_names <- names(context)
  if(!is_empty(context_names)) {
    .data$context <- context[order(context_names)]
  }
  .data
}

#' @rdname context-manipulations
#' @export
suppress_context <- function(.data, ...) {
  .data$muffle <- TRUE
  .data
}
