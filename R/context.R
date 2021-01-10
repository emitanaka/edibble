



#' Describe context related to experiment
#'
#' @description
#' This is a function that stores a simple context about the experiment.
#' The context is printed when the return object is printed as well
#' as exported in the title sheet (see [export_design()]). If the context
#' already exists then it will be overwritten.
#'
#' @param .edibble An edibble design (`EdibbleDesign`), an edibble data frame (`edbl_df`) or an
#'   object that contains the edibble data frame in the attribute
#'   `design`.
#' @param ... Strings of contexts or notes for the experiment. The context
#'  maybe named. If you context it is wrong, consider writing the context
#'  in an external file then write the reference to the file in context
#'  instead. Input strings support inline markup that use [glue braces](https://github.com/tidyverse/glue)
#'  as well [cli markup formatter](https://cli.r-lib.org/articles/semantic-cli.html#inline-text-formatting).
#'  The formatting is evaluated and stored in `EdibbleDesign`.
#' @return Same as original except `EdibbleDesign` updated with context.
#' @examples
#' start_design("COVID-19") %>%
#'   set_context(question = "Does Pfizer vaccine work?",
#'                  where = "Tested in lab",
#'                  "Context do not have to be named")
#' @seealso [sort_context()] for sorting context alphabetically.
#' @importFrom cli style_bold style_italic
#' @export
set_context <- function(.edibble, ...) {
  .design <- get_design(.edibble)

  new_context <- lapply(list2(...), function(x) {
      capture.output(cli_text(x), type = "message")
    })
  current_context <- .design$context
  overlapping_names <- intersect(names(new_context), names(current_context))

  ind <- which(names(current_context) %in% overlapping_names)
  if(!is_empty(ind)) {
      current_context <- current_context[-ind]
  }


  if(!is_empty(overlapping_names)) {
    s <- ifelse(length(overlapping_names) > 1, "s", "")
    msg <- paste0("The context", s,", ",
                  .combine_words(overlapping_names, fun = style_italic),
                  ", already exist and have been ")
    warn(paste0(msg, style_bold("ovewritten.")))
  }

  .design$context <- c(current_context, new_context)
  update_design(.edibble, .design)
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
#' @param .design An edibble design object.
#' @param descending Whether to sort it in ascending or descending alphabetical
#' order.
#' @param method the method to use for ordering. See [base::order()] for
#' explanation.
#' @name context-manipulations
NULL

#' @rdname context-manipulations
#' @export
sort_context <- function(.design, descending = FALSE,
                         method = c("auto", "shell", "radix")) {
  context <- .design$context
  context_names <- names(context)
  if(!is_empty(context_names)) {
    .design$context <- context[order(context_names, decreasing = descending,
                                   method = method)]
  }
  .design
}

#' @rdname context-manipulations
#' @export
suppress_context <- function(.design) {
  .design$muffle()
  .design
}

#' @rdname context-manipulations
#' @export
express_context <- function(.design) {
  .design$chatty()
  .design
}



get_design <- function(.edibble) {
  if(is_edibble_design(.edibble)) {
    .edibble
  } else {
    attr(.edibble, "design")
  }
}

update_design <- function(old, new) {
  if(is_edibble_design(old)) {
    new
  } else {
    attr(old, "design") <- new
    old
  }
}
