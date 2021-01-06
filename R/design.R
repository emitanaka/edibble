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
                      high = self$subset_graph("var"),
                      low = self$subset_graph("level"))
        plot.igraph(out, ...,
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
      },

      #' @description
      #' Subset graph.
      #' @param type A variable or level.
      #' @importFrom igraph induced_subgraph
      subset_graph = function(type = c("level", "var")) {
        type <- match.arg(type)
        graph <- private$.graph
        structure(induced_subgraph(graph, V(graph)$vtype==type),
                  class = class(graph))
      },

      #' @description
      #' Get the variable names.
      #' @param vindex Optional vertex index from graph object.
      var_names = function(vindex) {
        if(missing(vindex)) {
          vgraph <- self$subset_graph(type = "var")
          V(vgraph)$name
        } else {
          if(any(w <- V(private$.graph)$vtype[vindex]!="var")) {
            abort(paste0("The vertex index ", vindex[w], " is not an edibble variable node."))
          }
          V(private$.graph)$name[vindex]
        }
      },

      #' @description
      #' Get the variable class.
      #' @param vname The name of the variable node.
      var_class = function(vname) {
        if(missing(vname)) {
          vgraph <- self$subset_graph(type = "var")
          V(vgraph)$class
        } else {
          ind <- self$var_index(vname)
          V(private$.graph)$class[ind]
        }
      },

      #' @description
      #' Get the variable vertex index given the name.
      #' @param vname The name of the variable (or label).
      #' @param var TRUE or FALSE.
      var_index = function(vname, var = FALSE) {
        if(var) {
          which(V(private$.graph)$vname %in% vname)
        } else {
          which(V(private$.graph)$name %in% vname)
        }
      },

      #' @description
      #' Vertex labels.
      #' @param name The name of the variable.
      #' @param nlevels The total number of levels.
      vertex_var_label = function(name, nlevels) {
        paste0(name, "\n(", nlevels, " levels)")
      },

      #' @description
      #' Vertex levels.
      #' @param vname The name of the variable.
      #' @param lnames The level names.
      vertex_level_names = function(vname, lnames) {
        paste0(vname, ":", lnames)
      }
    ),

    active  = list(

      #' @field active get the active view
      active = function() {
        private$.active
      },

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
          private$.table <- render_table(self)
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
      .context = NULL,
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

