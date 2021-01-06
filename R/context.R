

#' Describe context related to experiment
#'
#' @description
#' This is a function that stores a simple context about the experiment.
#' If the context already exists then it will be overwritten or ignored.
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
#' @importFrom cli style_bold style_italic
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
    msg <- paste0("The context", s,", ",
                  .combine_words(overlapping_names, fun = style_italic),
                  ", already exist and have been ")
    if(.overwrite) {
      warn(paste0(msg, style_bold("ovewritten.")))
    } else {
      warn(paste0(msg, style_bold("ignored.")))
    }
  }

  .data$context <- c(current_context, new_context)
  .data
}


#' Context manipulations
#'
#' @description
#' You can use `sort_context` to reorder named context alphabetically. If
#' you don't want to see the context printed out each time, use `suppress_context`
#' to muffle the context and `express_context` to turn on the context print out.
#' Use `switch_context` to turn on print out of context if it was switched off,
#' or turn off print out if it was switched on.
#'
#' @param .data An edibble design object.
#' @param descending Whether to sort it in ascending or descending alphabetical
#' order.
#' @param method the method to use for ordering. See [base::order()] for
#' explanation.
#' @name context-manipulations
NULL

#' @rdname context-manipulations
#' @export
sort_context <- function(.data, descending = FALSE,
                         method = c("auto", "shell", "radix")) {
  context <- .data$context
  context_names <- names(context)
  if(!is_empty(context_names)) {
    .data$context <- context[order(context_names, decreasing = descending,
                                   method = method)]
  }
  .data
}

#' @rdname context-manipulations
#' @export
suppress_context <- function(.data) {
  .data$muffle()
  .data
}

#' @rdname context-manipulations
#' @export
express_context <- function(.data) {
  .data$chatty()
  .data
}

