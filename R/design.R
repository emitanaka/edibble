#' An R6 Class for an edibble design
#'
#' @description
#' An object with EdibbleDesign holds the core information related to
#' the experimental design.
#'
#' @importFrom igraph make_empty_graph
#' @importFrom cli cli_rule cli_text cli_ul cli_li cli_alert_info
EdibbleDesign <- R6::R6Class("EdibbleDesign",

  public = list(


      #' @description
      #' Initialise the edibble design. See also [start_design()].
      #' @param name The name or title of the project. The title
      #'  should be informative and not overly long.
      #'  If not supplied the default is "An edibble design".
      initialize = function(name = NULL) {
        private$.name <- name %||% "An edibble design"
        private$.active <- "graph"
        private$.graph <- structure(make_empty_graph(),
                                    class = c("edbl_graph", "igraph"))
      },

      #' @description
      #' Invoked when the EdibbleDesign object is deleted.
      finalize = function() {
        cli_alert_info("Cleaning up {.field {private$.name}}")
      },

      #' @description
      #' Plot the intermediate contruct.
      #' @param view A high- or -low level view.
      #' @param ... Arguments passed into `plot.igraph`.
      #' @param main The title of the plot. By default it is the
      #'  name of the EdibbleDesign object.
      #' @importFrom igraph plot.igraph
      plot = function(view = c("high", "low"), ..., main = NULL) {
        main <- main %||% private$.name
        view <- match.arg(view)
        out <- switch(view,
                      high = subset_vars(private$.graph),
                      low = subset_levels(private$.graph))
        plot.igraph(private$.graph, ...,
                    annotate.plot = TRUE,
                    main = main)
        invisible(self)
      },

      #' @description
      #' Print the intermediate construct.
      #' @param ... Arguments passed into `print`.
      print = function(...) {
        if(private$.active == "graph") {
          context_names <- names(private$.context)
          prefix <- paste0("{.emph ", context_names, "}: ")
          prefix[is_empty(context_names) | context_names==""] <- ""
          context <- private$.context
          context_all <- paste0(prefix, context)
          if(!is_empty(context) && !private$.muffle) {
            cli_rule(left = "{.emph Context of the experiment}")
            cli_text()
            cli_ul()
            cli_li(context_all)
            cli_end()
            cli_text()
            cli_rule()
          }
          print(private$.graph, main = private$.name, ...)
        } else if(private$.active == "table") {
          print(private$.table, ...)
        }
        invisible(self)
      },

      #' @description
      #' This saves the `.Random.seed` used just prior to the
      #' randomisation process.
      save_seed = function() {
        if(!exists(".Random.seed")) set.seed(NULL)
        private$.seed <- .Random.seed
      },

      #' @description
      #' muffle the nam
      muffle = function() {
        private$.muffle <- TRUE
      },

      #' @description chatty turn off muffle
      chatty = function() {
        private$.muffle <- FALSE
      },

      #' @description
      #' active Which is active
      activate_table = function() {
        private$.active <- "table"
      },

      #' @description
      #'  active Which is active
      activate_graph = function() {
        private$.active <- "graph"
      }
    ),

    active  = list(

      #' @field name the name
      name = function(value) {
        if(missing(value)) {
          private$.name
        } else {
          stopifnot(length(value)!=1)
          private$.name <- value
        }
      },

      #' @field graph the name
      graph = function(value) {
        if(missing(value)) {
          private$.graph
        } else {
          not_edibble_graph(value)
          private$.graph <- value
        } },

      #' @field table the name
      table = function(value) {
        if(missing(value)) {
          private$.table <- serve_table(private$.graph)
          private$.table
        } else {
          abort("cannot modify table")
        } },

      #' @field context the name
      context = function(value) {
        if(missing(value)) {
          private$.context
        } else {
          private$.context <- value
        }
      }
   ),

    private = list(
      .name = NULL,
      .graph = NULL,
      .table = NULL,
      .active = NULL,
      .muffle = FALSE,
      .seed = NULL,
      .method = NULL,

      use_method = function(.x, .f, ...) {
        x <- .x
        class(x) <- c(private$.method, class(.x))
        res <- .f(x, ...)
        # if method included in returning class, remove it
        class(res) <- setdiff(class(res), private$.method)
        res
      }
    ))


#' This sets a class.
use_method <- function(.data, class = NULL) {
  .data$method <- class
  .data
}

reset_method <- function(.data) {
  .data$method <- NULL
  .data
}

