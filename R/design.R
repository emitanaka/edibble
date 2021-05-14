#' An R6 Class for an edibble design
#'
#' @description
#' An object with `EdibbleDesign` holds the core information related to
#' the experimental design. Typically the user should not be interacting
#' with the `EdibbleDesign` methods directly but should be invoking the methods
#' using the friendlier user-facing family of functions instead, beginning
#' with [start_design()] or [restart_design()].
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
      #' Turn off messages.
      muffle = function() {
        private$.muffle <- TRUE
      },

      #' @description
      #' Turn on messages.
      chatty = function() {
        private$.muffle <- FALSE
      },

      #' @description
      #' Change active view to table.
      activate_table = function() {
        private$.active <- "table"
      },

      #' @description
      #' Change active view to graph.
      activate_graph = function() {
        private$.active <- "graph"
      },

      #' @description
      #' Rename the variables
      #' @param ... Similar to `dplyr::rename`, use `new_name = old_name` to
      #'  rename variables.
      #' @importFrom tidyselect eval_rename
      rename = function(...) {
        table <- serve_table(self)
        loc <- eval_rename(expr(c(...)), table)
        old_names <- new_names <- names(table)
        new_names[loc] <- names(loc)
        dict <- set_names(new_names, old_names)

        graph <- private$.graph
        vnames <- vertex_attr(graph, "vname")
        graph <- set_vertex_attr(graph, "vname",
                                          value = dict[vnames])
        vindex <- which(V(graph)$vtype == "var")
        graph <- set_vertex_attr(graph, "name", vindex,
                                 dict[vnames[vindex]])
        # need to refresh the level names.
        private$.graph <- private$reinstate(graph)
      },



      #' @description
      #' Add the variable to the graph.
      #' @param value Short hand value of what to add.
      #' @param vname The variable name
      #' @param attr The vertex attributes.
      add_variable = function(value, vname, attr) {
        private$.graph <- add_edibble_vertex(value, vname, self, attr)
      },

      #' @description
      #' Add the variable to the graph.
      #' @param vnames_new The names of new variables to add.
      #' @param vname_unit The unit variable that measurement is taken on.
      #' @param attr The vertex attributes.
      add_record_node = function(vnames_new, vname_unit, attr) {
        graph <- private$.graph
        graph <- add_vertices(graph, length(vnames_new),
                                       name = vnames_new,
                                       vtype = "var",
                                       vname = vnames_new,
                                       label = vnames_new,
                                       attr = attr)
        graph <- add_edges(graph,
                                    cross_edge_seq(graph, vnames_new, vname_unit),
                                    attr = edge_attr_opt("r2v"))
        private$.graph <- reinstate_graph_attrs(graph, private$.graph)
      },

      #' @description
      #' This adds edges that connect treatment to unit.
      #' @param trts The name of the treatments.
      #' @param unit The name of the unit. There should be only one unit.
      add_allocation = function(trts, unit) {
        if(length(trts)) {
          # there should be an error if .trt is not within vars
          # maybe there should be a check that .trt is edbl_trt
          vnames_from <- trts
        } else {
          vnames_from <- names(subset(private$.graph, class=="edbl_trt", .vtype = "var"))
        }
        graph <- private$.graph
        graph <- add_edges(graph,
                         cross_edge_seq(graph,
                                        var_levels(graph, vnames_from),
                                        var_levels(graph, unit)),
                         attr = edge_attr_opt("t2vmay"))
        graph <- add_edges(graph,
                         cross_edge_seq(graph,
                                        vnames_from,
                                        vnames_to = unit),
                         attr = edge_attr_opt("t2v"))
        private$.graph <- private$reinstate(graph)
      },

      #' @description
      #' This function assigns the allocation of treatments to units.
      #' @param randomise whether the allocation should be random or not
      #' This removes "t2vmay" edges and assigns edges from treatment to variable.
      assign_allocation = function(randomise = FALSE) {
        if(randomise) {
          graph <- randomise_trts_internal(self)
          private$.graph <- private$reinstate(graph)
          private$.allocation <- "randomised"
         } else {
           # systematic allocation
           graph <- systematic_trts_allocation(self)
           private$.graph <- private$reinstate(graph)
           private$.allocation <- "systematic"
         }
      },

      #' @description
      #' Appending validation rule
      #' @param validation A new list of validation. The name of the list
      #' should correspond to a record variable. The elements of the list
      #' should be a named list with "operator", "value" and "type". If
      #' type is list then it is will contain only "type" and "values".
      append_validation = function(validation) {
        private$.validation <- c(private$.validation, validation)
      },

      #' @description
      #' Append treatments to graph. These will not be factorial structure.
      #' @param value Short hand value of what to add.
      #' @param vname The variable name
      #' @param attr The vertex attributes.
      append_trts = function(value, vname, attr) {
        # [FIXME!!!]
        # private$.graph <- add_edibble_vertex(value, vname, self, attr)
      },

      #' @description
      #' Delete variable
      #' @param vnames The name of the variable to delete.
      delete_variable = function(vnames) {
        vindex <- which(V(private$.graph)$vname %in% vnames)
        graph <- delete_vertices(private$.graph, vindex)
        private$.graph <- reinstate_graph_attrs(graph, private$.graph)
      }

    ),

    active  = list(

      #' @field active get the active view
      active = function(value) {
        if(missing(value)) {
          private$.active
        } else {
          abort("You must use `$activate_to_table()` or `$activate_to_graph()`
                to change active view.")
        }
      },

      #' @field allocation systematic or random or null
      allocation = function(value) {
        if(missing(value)) {
          private$.allocation
        } else {
          abort("You cannot modify allocation.")
        }
      },

      #' @field name The name of the design or project
      name = function(value) {
        if(missing(value)) {
          private$.name
        } else {
          stopifnot(length(value)!=1)
          private$.name <- value
        }
      },

      #' @field names_trts The treatment names.
      names_trts = function(value) {
        private$getting_names_by_class(value, "edbl_trt")
      },

      #' @field names_units The unit variable names.
      names_units = function(value) {
        private$getting_names_by_class(value, "edbl_unit")
      },

      #' @field names_resp The unit variable names.
      names_resp = function(value) {
        private$getting_names_by_class(value, "edbl_resp")
      },

      #' @field names_vars The name of all variables.
      names_vars = function(value) {
        private$getting_names_by_class(value)
      },
      # I shouldn't allow people to modify graph outside
      #' @field graph An edibble graph.
      graph = function(value) {
        if(missing(value)) {
          private$.graph
        } else {
          abort("You cannot modify the edibble graph directly.")
        } },


      #' @field table A table served from the edibble graph.
      table = function(value) {
        if(missing(value)) {
          private$.table <- serve_table(self)
          private$.table
        } else {
          abort("You cannot modify the edibble table directly.")
        } },

      #' @field validation Data validation for response.
      validation = function(value) {
        if(missing(value)) {
          private$.validation
        } else {
          abort("You cannot modify data validation directly.")
        } },

      #' @field context The context of the experiment.
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
      .validation = NULL,
      .allocation = NULL,

      # possibly delete
      use_method = function(.x, .f, ...) {
        x <- .x
        class(x) <- c(private$.method, class(.x))
        res <- .f(x, ...)
        # if method included in returning class, remove it
        class(res) <- setdiff(class(res), private$.method)
        res
      },

      # A helper function to getting names.
      getting_names_by_class = function(value, class) {
        if(missing(value)) {
          if(missing(class)) {
            names_vars(private$.graph)
          } else {
            names_by_class(private$.graph, class = class)
          }
        } else {
          abort("Use `$rename` to change names instead.")
        }
      },

      reinstate = function(g) {
        reinstate_graph_attrs(g, private$.graph)
      }
    ))


# This sets a class.
# Maybe delete
use_method <- function(.data, class = NULL) {
  .data$method <- class
  .data
}

# Maybe delete
reset_method <- function(.data) {
  .data$method <- NULL
  .data
}



update_design <- function(old, new) {
  if(is_edibble_design(old)) {
    new$clone(deep = TRUE)
  } else {
    attr(old, "design") <- new$clone(deep = TRUE)
    old
  }
}


# print -------------------------------------------------------------------


#' Print edibble graph to terminal
#'
#' @description
#' This function prints an `edbl_graph` object as a tree to terminal.
#' The variables are color coded (or decorated) with the given options.
#' Any ANSI coloring or styling are only visible in the console or terminal
#' outputs that support it. The print output is best used interactively since
#' any text styling are lost in text or R Markdown output. More details can
#' be found in `vignette("edbl-output", package = "edibble")`.
#'
#' @param .data An edibble graph.
#' @param decorate_trts,decorate_units,decorate_resp,decorate_levels,decorate_title
#' A function applied to the name of treatment, unit, response factors or
#' design title. The function should return a string. Most often this wraps the name with
#' ANSI colored text. Run [edibble_opt()] to see the list of default
#' values for the options.
#'
#' @examples
#' # stylizing are only visible in terminal output that supports it
#' print(nclassics$split)
#' ## Split plot design
#' ## ├─mainplot (4 levels)
#' ## │ └─subplot (8 levels)
#' ## ├─subplot (8 levels)
#' ## ├─variety (2 levels)
#' ## └─irrigation (2 levels)
#' @importFrom igraph V vertex_attr neighbors
#' @importFrom cli tree
#'
#' @export
print.edbl_graph <- function(.graph,
                             decorate_units  = edibble_decorate("units"),
                             decorate_trts   = edibble_decorate("trts"),
                             decorate_resp   = edibble_decorate("resp"),
                             decorate_levels = edibble_decorate("levels"),
                             decorate_main  = edibble_decorate("main"),
                             main = NULL) {

  main <- main %||% "An edibble design"
  vgraph <- subset_vars(.graph)
  gnames <- V(vgraph)$name

  if(is_empty(gnames)) {
    data <- data.frame(var = "root", child = NA,
                       label = as.character(decorate_main(main)))
  } else {

    classes <- V(vgraph)$class
    label_names <- decorate_vars(gnames,
                                 decorate_units,
                                 decorate_trts,
                                 decorate_resp,
                                 classes)


    var_nlevels <- lengths(vars_levels(.graph, gnames))
    nvar <- length(gnames)
    ll <- lapply(V(vgraph),
                 function(v) {
                   class <- vertex_attr(vgraph, "class", v)
                   children <- neighbors(vgraph, v, mode = "out")
                   if(class!="edbl_trt" & !is_empty(children)) {
                     gnames[children]
                   } else {
                     character()
                   }
                 })
    nodes_with_parents <- unname(unlist(ll))


    data <- data.frame(var = c("root", gnames),
                       child = I(c(list(setdiff(gnames, nodes_with_parents)), ll)),
                       label = c(decorate_main(main),
                                 paste(label_names, map_chr(var_nlevels, decorate_levels))))
  }
  cat(tree(data, root = "root"), sep = "\n")
}
