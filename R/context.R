



#' Describe context related to experiment
#'
#' @description
#' `set_context` stores a simple context about the experiment.
#' The context is printed when the return object is printed as well
#' as exported in the title sheet (see [export_design()]). If the context
#' already exists then it will be overwritten.
#'
#'
#' @param .edibble An edibble design (`edbl_design`), an edibble data frame (`edbl_table`) or an
#'   object that contains the edibble data frame in the attribute
#'   `design`.
#' @param ... Strings of contexts or notes for the experiment. The context
#'  maybe named. If you context it is wrong, consider writing the context
#'  in an external file then write the reference to the file in context
#'  instead. Input strings support inline markup that use [glue braces](https://github.com/tidyverse/glue)
#'  as well [cli markup formatter](https://cli.r-lib.org/articles/semantic-cli.html#inline-text-formatting).
#'  The formatting is evaluated and stored in `edbl_design`.
#' @param .record A logical value. This indicates whether to record this
#'    code step. The default is TRUE. It should remain TRUE unless this
#'    function is used as a wrapper in other code.
#' @return Same as original except `edbl_design` updated with context.
#' @examples
#' files <- c("details.txt", "about.docx")
#' start_design("COVID-19") %>%
#'   set_context(question = "Does {.field Pfizer vaccine} work?",
#'               where = "Tested in {.emph lab}",
#'               contact = "{.strong Jane Doe} ({.email jane.doe@fakeaddress.com}) for domain knowledge",
#'               "Context do not have to be named",
#'               "The function {.fn designRandomise} from {.pkg dae} randomises allocation of treatments",
#'               "Other detailed information in {.file {files}}",
#'               "Check more details at {.url https://covid-19-au.com/}")
#' @name design-context
#' @importFrom cli style_bold style_italic
#' @family user-facing functions
#' @export
set_context <- function(.edibble, ..., .record = TRUE) {

  if(.record) record_step()

  new_context <- lapply(list2(...), function(x) {
      capture.output(cli_text(x), type = "message")
    })
  current_context <- .edibble$context
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

  .edibble$context <- c(current_context, new_context)
  .edibble
}
