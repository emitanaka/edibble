
#' A manipulator for the edbl_design.
#'
#' Internal functions should create a new Provenance object.
#' The Provenance contains a set of operations to manipulate the nodes and edges of
#' the edibble design object.
#'
#' @param class The class for the vertex/node.
#' @param data The nodes data
#' @param name The name of the vertex.
#' @param id The id of the corresponding node.
#' @param input The value of the new graph structure to add.
#' @param initial The intial id.
#' @param abort Whether to abort.
#' @importFrom vctrs vec_is
#' @export
Provenance <- R6::R6Class("Provenance",
                       public = list(

                         #' Initialise function
                         #' @param design An edibble design.
                         initialize = function(graph = NULL) {
                           private$record_track_internal()
                           #self$add_tracker_to_set_fns(track_fns)
                           private$graph <- graph %||% empty_edibble_graph()
                           private$edbl_version <- packageVersion("edibble")
                           private$session_info <- utils::sessionInfo()
                           private$tracker <- list(new_trackable())
                         },

                         # add_tracker_to_set_fns = function(fnames) {
                         #   for(f in fnames) {
                         #     b <- body(self[[f]])
                         #     if(length(b) > 1) {
                         #       body(self[[f]]) <- as.call(c(b[[1]], expression(private$record_track_internal()), b[2:length(b)]))
                         #     }
                         #   }
                         # },

                         set_title = function(title) {
                           private$record_track_internal()
                           title <- vctrs::vec_cast(title, character())
                           vctrs::vec_assert(title, character(), 1)
                           private$title <- title
                         },

                         set_name = function(name) {
                           private$record_track_internal()
                           name <- vctrs::vec_cast(name, character())
                           vctrs::vec_assert(name, character(), 1)
                           private$name <- name
                         },

                         reactivate = function(des, overwrite = c("graph", "anatomy", "recipe")) {
                           private$record_track_internal()
                           for(obj in overwrite) {
                             private[[obj]] <- des[[obj]]
                           }
                         },

                         deactivate = function(delete = c("graph", "anatomy", "recipe")) {
                           private$record_track_internal()
                           for(obj in delete) {
                             private[[obj]] <- NULL
                           }
                         },


                         #' @description
                         #' Get the id based on either the name of the factor node.
                         #' If none supplied then it will give all.
                         fct_id = function(name = NULL, role = NULL) {
                           fnodes <- self$fct_nodes
                           if(!is_null(role)) {
                             private$validate_role(role)
                             fnodes <- fnodes[fnodes$role == role, ]
                           }
                           name_to_id <- set_names(fnodes$id, fnodes$name)
                           name <- name %||% names(name_to_id)
                           unname(name_to_id[as.character(name)])
                         },


                         #' @description
                         #' Get the factor parent ids
                         fct_id_parent = function(id = NULL, role = NULL) {
                           private$node_id_parent_child(id = id, role = role, node = "factor", return = "parent")
                         },

                         #' @description
                         #' Get the factor child ids. If `role` is
                         #' supplied then the child has to fit `role`
                         fct_id_child = function(id = NULL, role = NULL) {
                           private$node_id_parent_child(id = id, role = role, node = "factor", return = "child")
                         },


                         #' @description
                         #' Get the factor ancestor ids
                         fct_id_ancestor = function(id = NULL, role = NULL) {
                           private$var_id_ancestor(id = id, role = role, node = "factor")
                         },


                         #' @field fct_leaves
                         #' Get the leave factor ids.
                         fct_id_leaves = function(role = NULL) {
                           fids <- self$fct_id(role = role)
                           has_child <- map_lgl(fids, function(id) length(self$fct_id_child(id = id)) > 0)
                           fids[!has_child]
                         },

                         #' @description
                         #' Get the id based on name of level node.
                         #' Assumes that level ids obtained are all from the same fid
                         lvl_id = function(value = NULL, role = NULL, fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(role)) {
                             private$validate_role(role)
                             private$validate_id(fid, 1)
                             lnodes_list <- lnodes_list[as.character(fct_id(role = role))]
                           }
                           if(is_null(fid)) {
                             if(is_null(value)) {
                               # return all lvl ids
                               return(unname(unlist(lapply(lnodes_list, function(x) x$id))))
                             } else {
                               fid_search <- as.integer(names(lnodes_list))
                               fid <- self$fct_id_from_lvl_values(value = value, fid_search = fid_search)
                               return(self$lvl_id(value = value, role = role, fid = fid))
                             }
                           } else {
                             private$validate_id(fid , 1)
                             lnodes <- lnodes_list[[as.character(fid)]]
                             if(!is_null(value)) {
                               value_to_id <- set_names(lnodes$id, lnodes$value)
                               unname(value_to_id[as.character(value)])
                             } else {
                               lnodes$id
                             }
                           }
                         },

                         #' @description
                         #' Get the level parent ids
                         lvl_id_parent = function(id = NULL, class = NULL) {
                           private$node_id_parent_child(id = id, role = role, node = "level", return = "parent")
                         },


                         #' @description
                         #' Get the level child ids
                         lvl_id_child = function(id = NULL, role = NULL) {
                           private$node_id_parent_child(id = id, role = role, node = "level", return = "child")
                         },

                         #' @description
                         #' Get the level ancestor ids
                         lvl_id_ancestor = function(id = NULL, role = NULL) {
                           private$var_id_ancestor(id = id, role = role, node = "level")
                         },

                         #' @param fid_search A vector of fids to search from.
                         fct_id_from_lvl_id = function(id = NULL, fid_search = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid_search)) lnodes_list <- lnodes_list[as.character(fid_search)]
                           for(fname in names(lnodes_list)) {
                             if(all(id %in% lnodes_list[[fname]]$id)) return(as.integer(fname))
                           }
                         },

                         fct_id_from_lvl_values = function(value = NULL, fid_search = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid_search)) lnodes_list <- lnodes_list[as.character(fid_search)]
                           for(fname in names(lnodes_list)) {
                             if(all(value %in% lnodes_list[[fname]]$value)) return(as.integer(fname))
                           }
                         },

                         lvl_id_from_fct_id = function(fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           lnodes_list[[as.character(fid)]]$id
                         },

                         #' @description
                         #' Get the factor names based on id or class
                         fct_names = function(id = NULL, role = NULL) {
                           fnodes <- self$fct_nodes
                           if(!is_null(role)) {
                             private$validate_role(role)
                             fnodes <- fnodes[fnodes$role == role, ]
                           }
                           id_to_name <- set_names(fnodes$name, fnodes$id)
                           ids <-  id %||% fnodes$id
                           unname(id_to_name[as.character(ids)])
                         },

                         unit_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_unit")
                         },

                         trt_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_trt")
                         },

                         rcrd_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_rcrd")
                         },

                         #' @description
                         #' Get the level values based on id or class
                         #' cannot have just role only defined.
                         #' id must be from the same fid
                         lvl_values = function(id = NULL, role = NULL, fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid)) {
                             private$validate_id(fid, 1, role = role)
                             lnodes <- lnodes_list[[as.character(fid)]]
                             id <- id %||% lnodes$id
                             return(lnodes[lnodes$id %in% id, "value"])
                           }
                           if(!is_null(id)) {
                             fid <- self$fct_id_from_lvl_id(id = id)
                             self$lvl_values(id = id, role = role, fid = fid)
                           } else {
                             abort("`id` or `fid` must be provided.")
                           }
                         },

                         unit_values = function(id = NULL, fid = NULL) {
                           self$lvl_values(id = id, role = "edbl_unit", fid = fid)
                         },

                         trt_values = function(id = NULL, fid = NULL) {
                           self$lvl_values(id = id, role = "edbl_trt", fid = fid)
                         },

                         #' @param uid The unit level id
                         rcrd_values = function(uid = NULL, fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(is_null(fid)) abort("The rcrd id must be supplied.")
                           private$validate_id(fid, 1, role = "edbl_rcrd")
                           uid_fct <- self$fct_id_child(id = fid, role = "edbl_unit")
                           lnodes <- lnodes_list[[as.character(uid_fct)]]
                           id <- uid %||% lnodes$id
                           return(lnodes[["attr"]][lnodes$id %in% id, self$fct_names(id = fid)])
                         },



                         #' @description
                         #' Get the class of the vertex given the factor id
                         fct_role = function(id = NULL) {
                           fnodes <- self$fct_nodes
                           id_to_role <- pull(nodes, class, id)
                           ids_fct <-  id %||% nodes$id
                           unname(id_to_class_fct[as.character(ids_fct)])
                         },

                         #' @description
                         #' Get the levels for each factor
                         fct_levels = function(id = NULL, name = NULL) {
                           qid <- id %||% self$fct_id(name = name)
                           lnodes <- self$lvl_nodes
                           out <- lapply(lnodes[as.character(qid)], function(x) x$value)
                           names(out) <- self$fct_names(id = qid)
                           out
                         },


                         #' @description
                         #' One of `name`, `id` or `role` is defined to check if it exists.
                         #' If more than one of the arguments `name`, `id` and `role` are supplied, then
                         #' the intersection of it will be checked.
                         #' @param abort A logical value to indicate whether to abort if it doesn't exist.
                         fct_exists = function(id = NULL, name = NULL, role = NULL, abort = TRUE) {
                           exist <- TRUE
                           abort_missing <- function(vars = NULL, msg = NULL) {
                             if(abort & !exist) {
                               if(!is_null(vars)) {
                                 abort(sprintf("%s does not exist in the design.",
                                               .combine_words(paste0("`", vars, "`"))))
                               }
                               if(!is_null(msg)) {
                                 abort(msg)
                               }
                             }
                           }

                           fnodes <- self$fct_nodes
                           # at least one node exists
                           if(is_null(name) & is_null(id) & is_null(role)) {
                             exist <- nrow(fnodes) > 0
                             abort_missing(msg = "There are no factor nodes.")

                           } else if(!is_null(name) & is_null(id) & is_null(role)) {
                             vexist <- name %in% fnodes$name
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist])

                           } else if(is_null(name) & !is_null(id) & is_null(role)) {
                             vexist <- id %in% fnodes$id
                             exist <- all(vexist)
                             abort_missing(vars = id[!vexist])

                           } else if(is_null(name) & is_null(id) & !is_null(role)) {
                             exist <- any(class %in% fnodes$class)
                             abort_missing(msg = sprintf("There are no factors with role%s",
                                                         .combine_words(paste0("`", role, "`"))))

                           } else if(is_null(name) & !is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(id, fnodes$id), "role"]
                             vexist <- srole == role
                             exist <- all(vexist)
                             abort_missing(vars = id[!vexist])

                           } else if(!is_null(name) & is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(name, fnodes$name), "class"]
                             vexist <- srole == role
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist])

                           } else if(!is_null(name) & !is_null(id) & is_null(role)) {
                             sid <- fnodes[match(name, fnodes$name), "id"]
                             vexist <- sid == id
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist])

                           } else {
                             snodes <- fnodes[match(name, fnodes$name), ]
                             vexist <- snodes$id == id & snodes$role == role
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist])
                           }

                           return(exist)
                         },

                         #' @description
                         #' Check if treatment exists.
                         trt_exists = function(id = NULL, name = NULL, abort = TRUE) {
                           self$fct_exists(id = id, name = name, role = "edbl_trt", abort = abort)
                         },

                         #' @description
                         #' Check if unit exists.
                         unit_exists = function(id = NULL, name = NULL, abort = TRUE) {
                           self$fct_exists(id = id, name = name, role = "edbl_unit", abort = abort)
                         },

                         #' @description
                         #' Check if record exists.
                         rcrd_exists = function(id = NULL, name = NULL, abort = TRUE) {
                           self$fct_exists(id = id, name = name, role = "edbl_rcrd", abort = abort)
                         },

                         lvl_exists = function(id = NULL, name = NULL, abort = TRUE) {
                           self$fct_exists(id = id, name = name, role = "edbl_rcrd", abort = abort)
                         },

                         #' @description
                         #' Given node data, append the factor nodes
                         append_fct_nodes = function(name, role, attrs = NULL) {
                           n <- length(name)
                           role <- vctrs::vec_recycle(role, n)
                           data <- tibble::tibble(id = private$fct_new_id(n = n),
                                                  name = name,
                                                  role = role,
                                                  attrs = attrs)

                           self$fct_nodes <- rbind_(self$fct_nodes, data)
                         },

                         #' @description
                         #' Given node data, append the level nodes
                         append_lvl_nodes = function(value, attrs = NULL, fid = NULL) {
                           lnodes <- self$lvl_nodes
                           id <- private$lvl_new_id(n = length(value))
                           data <- tibble::tibble(id = id, value = value, attrs = attrs)
                           if(is.null(lnodes[[as.character(fid)]])) {
                             if(!is_null(attrs)) {
                               lnodes[[as.character(fid)]] <- new_lnode(id, value, attrs)
                             } else {
                               lnodes[[as.character(fid)]] <- new_lnode(id, value)
                             }
                           } else {
                             lnodes[[as.character(fid)]] <- rbind_(lnodes[[as.character(fid)]], data)
                           }
                           self$lvl_nodes <- lnodes
                         },

                         #' @description
                         #' Given edge data, append the factor edges
                         append_fct_edges = function(from, to, type = NULL, group = NULL, attrs = NULL) {
                           self$fct_edges <- rbind_(self$fct_edges, tibble::tibble(from = from,
                                                                                   to = to,
                                                                                   type = type,
                                                                                   group = group,
                                                                                   attrs = attrs))
                         },

                         #' @description
                         #' Given edge data, append the level edges
                         append_lvl_edges = function(from, to, attrs = NULL) {
                           self$lvl_edges <- rbind_(self$lvl_edges, tibble::tibble(from = from,
                                                                                   to = to,
                                                                                   attrs = attrs))
                         },

                         save_seed = function(seed) {
                           private$record_track_internal()
                           if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                             stats::runif(1)
                           if (is.null(seed))
                             RNGstate <- get(".Random.seed", envir = .GlobalEnv)
                           else {
                             set.seed(seed)
                             RNGstate <- structure(seed, kind = as.list(RNGkind()))
                           }
                           private$seed <- RNGstate
                         },

                         get_trail = function() {
                           private$trail[-length(private$trail)]
                         },

                         get_seed = function() {
                           private$seed
                         },

                         get_session_info = function() {
                           private$session_info
                         },

                         get_edibble_version = function() {
                           private$edibble_version
                         },

                         record_step = function() {
                           do.call("on.exit",
                                   list(quote(return(add_edibble_code(returnValue(default = FALSE),
                                                                      paste(gsub("(^ +| +$)", "", deparse(match.call())), collapse = "")))),
                                        add = TRUE),
                                   envir = parent.frame())
                         },

                         record_track_external = function(code) {
                           ncmds <- length(private$trail)
                           attr(private$trail[[ncmds]], "external_cmd") <- code
                           attr(private$trail[[ncmds]], "execution_time") <- Sys.time()
                           attr(private$trail[[ncmds]], "time_zone") <- Sys.timezone()
                           private$trail[[ncmds + 1]] <- new_trackable()
                         }

                       ),

                       active = list(
                         #' @field fct_nodes
                         #' Get the factor nodes
                         fct_nodes = function(data) {
                           if(missing(data)) return(self$graph$factors$nodes)
                           else self$graph$factors$nodes <- data
                         },

                         #' @field lvl_nodes
                         #' Get the level nodes
                         lvl_nodes = function(data) {
                           if(missing(data)) {
                             nodes <- self$graph$levels$nodes
                             return(nodes)
                           }
                           else self$graph$levels$nodes <- data
                         },

                         #' @field fct_edges
                         #' Get the factor edges
                         fct_edges = function(data) {
                           if(missing(data)) {
                             edges <- self$graph$factors$edges
                             edges$var_from <- self$fct_names(id = edges$from)
                             edges$var_to <- self$fct_names(id = edges$to)
                             return(edges)
                           } else {
                             self$graph$factors$edges <- data
                           }
                         },

                         #' @field lvl_edges
                         #' Get the level edges
                         lvl_edges = function(data) {
                           if(missing(data)) {
                             edges <- self$graph$levels$edges
                             edges$lvl_from <- self$lvl_names(id = edges$from)
                             edges$lvl_to <- self$lvl_names(id = edges$to)
                             return(edges)
                           } else {
                             self$graph$levels$edges <- data
                           }
                         },

                         #' @field fct_n
                         #' Get the number of nodes in factor graph
                         fct_n = function(value) {
                           if (missing(value)) {
                             nrow(self$fct_nodes)
                           } else {
                             stop("Can't set `$fct_n`.")
                           }
                         },

                         #' @field lvl_n
                         #' Get the number of nodes in level graph
                         lvl_n = function(value) {
                           if (missing(value)) {
                             sum(lengths(self$lvl_nodes_list))
                           } else {
                             stop("Can't set `$lvl_n`.")
                           }
                         },




                         #' @field rcrd_ids
                         #' Get the ids for all edbl_rcrd factors.
                         rcrd_ids = function() {
                           self$fct_id(role = "edbl_rcrd")
                         },

                         #' @field unit_ids
                         #' Get the ids for all edbl_unit factors.
                         unit_ids = function() {
                           self$fct_id(role = "edbl_unit")
                         },

                         #' @field trt_ids
                         #' Get the ids for all edbl_trt factors.
                         trt_ids = function() {
                           self$fct_id(role = "edbl_trt")
                         },


                         #' @field is_connected
                         #' Check if nodes are connected.
                         is_connected = function() {
                           nvar <- self$fct_n - length(self$rcrd_ids)
                           if(nvar==0) return(FALSE)
                           if(nvar==1) return(TRUE)
                           ledges <- self$lvl_edges
                           all(self$lvl_id %in% c(ledges$to, ledges$from))
                         }
                       ),
                       private = list(
                         fct_id_last = 0L,
                         lvl_id_last = 0L,

                         title = "An edibble design",
                         name = NULL,
                         seed = NULL,
                         edbl_version = NULL,
                         session_info = NULL,
                         trail = NULL,

                         anatomy = NULL,
                         recipe = NULL,
                         graph = NULL,

                         validate_id = function(id, n = NULL, role = NULL) {
                           id <- vctrs::vec_cast(id, integer())
                           if(is.null(n)) {
                             vctrs::vec_assert(id, integer())
                           } else {
                             vctrs::vec_assert(id, integer(), n)
                           }
                           if(!is.null(role)) all(fct_role(id) %in% role)
                         },

                         validate_role = function(role) {
                           vctrs::vec_assert(role, character(), 1)
                         },

                         validate_name = function(name) {
                           vctrs::vec_assert(name, character())
                         },

                         node_id_parent_child = function(id = NULL, role = NULL, node = c("factor", "level"), return = c("child", "parent")) {
                           type <- match.arg(type)
                           node <- match.arg(node)
                           if(node == "factor") {
                             edges <- self$fct_edges
                             edges <- edges[!edges$type %in% c("depends", "cross"), ]
                           } else if(node == "level") {
                             edges <- self$lvl_edges
                           }
                           child_ids <- edges$to
                           parent_ids <- edges$from
                           role_ids <- self$fct_id(role = role)
                           if(return == "parent") return(parent_ids[child_ids %in% id & parent_ids %in% role_ids])
                           if(return == "child") return(child_ids[parent_ids %in% id & child_ids %in% role_ids])
                        },

                        var_id_ancestor = function(id = NULL, role = NULL, node = c("factor", "level")) {
                          out <- unique(id)
                          parent_ids <- private$node_id_parent_child(id = id, role = role, node = node, return = "parent")
                          if(!is_empty(parent_ids)) {
                            out <- unique(c(out, private$var_id_ancestor(id = parent_ids, role = role, node = node)))
                          }
                          out
                        },

                        record_track_internal = function() {
                          do.call("on.exit",
                                  list(quote(private$add_trail_internal(paste(gsub("(^ +| +$)", "", deparse(match.call())), collapse = ""))),
                                       add = TRUE),
                                  envir = parent.frame())
                        },

                        add_trail_internal = function(code) {
                          private$trail[[length(private$trail)]] <- rbind(private$trail[[length(private$trail)]],
                                                                          new_trackable(internal_cmd = code,
                                                                                        time_internal = Sys.time(),
                                                                                        time_zone_internal = Sys.timezone()))
                        },


                         #' @field fct_new_id
                         #' Get a new factor id.
                         fct_new_id = function(n = 1) {
                           ids <- seq(private$fct_last_id + 1, private$fct_last_id + n)
                           private$fct_last_id <- private$fct_last_id + n
                           ids
                         },

                         #' @field lvl_new_id
                         #' Get a new level id.
                         lvl_new_id = function(n = 1) {
                           ids <- seq(private$lvl_last_id + 1, private$lvl_last_id + n)
                           private$lvl_last_id <- private$lvl_last_id + n
                           ids
                         }
                       ))
