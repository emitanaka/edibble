
#' @importFrom igraph E make_empty_graph set_graph_attr
#' @importFrom cli cli_rule cli_text cli_li cli_end cli_ul
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
          context_names <- names(private$.context)
          context <- private$.context
          if(!is_empty(context) && !private$.muffle) {
            cli_rule(left = "{.emph Context of the experiment}")
            cli_text()
            cli_ul()
            for(i in seq_along(context)) {
              if(is_null(context_names[i]) || context_names[i]=="") {
                cli_li("{context[[i]]}")
              } else {
                cli_li("{.emph {context_names[i]}}: {context[[i]]}")
              }
            }
            cli_end()
            cli_text()
            cli_rule()
          }
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
          not_edibble_graph(value)
          private$.graph <- value
        } },

      table = function(value) {
        if(missing(value)) {
          serve_table(private$.graph)
        } else {
          abort("You cannot modify table")
        } },

      context = function(value) {
        if(missing(value)) {
          private$.context
        } else {
          private$.context <- value
        }
      },

      muffle = function(value) {
        if(missing(value)) {
          private$.muffle
        } else {
          private$.muffle <- value
        }
      }
   ),

    private = list(
      .name = "",
      .graph = NULL,
      .table = NULL,
      .active = "graph",
      .context = list(),
      .muffle = FALSE
    ))
