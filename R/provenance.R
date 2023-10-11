
#' An object to query, record and modify an edibble graph
#'
#' The Provenance contains a set of operations to manipulate the nodes and edges of
#' the edibble graph object.
#'
#' @param role The role for the node.
#' @param name The name of the node.
#' @param value The value of the node.
#' @param id The id of the corresponding node.
#' @param fid The factor id.
#' @param attrs The attributes.
#' @param abort Whether to abort.
#' @param return To return in "id" or "value" format.
#' @importFrom vctrs vec_is
#' @importFrom R6 R6Class
#' @export
Provenance <- R6::R6Class("Provenance",
                       public = list(

                         #' @description
                         #' Initialise function
                         #' @param graph An edibble graph.
                         initialize = function(graph = NULL) {
                           private$record_track_internal()
                           #self$add_tracker_to_set_fns(track_fns)
                           private$graph <- graph %||% empty_edibble_graph()
                           private$edbl_version <- packageVersion("edibble")
                           private$session_info <- utils::sessionInfo()
                           private$trail <- list(new_trackable())
                         },

                         # add_tracker_to_set_fns = function(fnames) {
                         #   for(f in fnames) {
                         #     b <- body(self[[f]])
                         #     if(length(b) > 1) {
                         #       body(self[[f]]) <- as.call(c(b[[1]], expression(private$record_track_internal()), b[2:length(b)]))
                         #     }
                         #   }
                         # },

                         #' @description
                         #' Set the title.
                         #' @param title The title of the experiment
                         set_title = function(title) {
                           private$record_track_internal()
                           title <- vctrs::vec_cast(title, character())
                           vctrs::vec_assert(title, character(), 1)
                           private$title <- title
                         },

                         #' @description
                         #' Set the name.
                         #' @param name The name of the edibble graph object.
                         set_name = function(name) {
                           private$record_track_internal()
                           name <- vctrs::vec_cast(name, character())
                           vctrs::vec_assert(name, character(), 1)
                           private$name <- name
                         },

                         #' @description
                         #' Set the validation.
                         #' @param validation The validation statement.
                         #' @param type The type of validation.
                         set_validation = function(validation, type = "rcrds") {
                           private$record_track_internal()
                           private$validation[[type]] <- validation
                         },


                         #' @description
                         #' Set the simulation process
                         #' @param name The name of the process
                         #' @param process A function to simulate the record
                         #' @param rcrds The record factor name simulating for.
                         set_simulate = function(name, process, rcrds) {
                           private$record_track_internal()
                           private$simulate[[name]] <- list(process = process,
                                                            rcrds = rcrds)
                         },


                         #' @description
                         #' Reactivate the graph in the provenance object.
                         #' @param design An edibble design
                         #' @param overwrite A vector of character to overwrite from the
                         #' supplied design object.
                         reactivate = function(design, overwrite = c("graph", "anatomy", "recipe", "validation", "simulate")) {
                           #private$record_track_internal()
                           for(obj in overwrite) {
                             private[[obj]] <- design[[obj]]
                           }
                         },

                         #' @description
                         #' Deactivate the provenance object.
                         #' @param delete A vector of character to delete.
                         deactivate = function(delete = c("graph", "anatomy", "recipe", "validation")) {
                           #private$record_track_internal()
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
                             fnodes <- fnodes[fnodes$role %in% role, ]
                           }
                           name_to_id <- set_names(fnodes$id, fnodes$name)
                           name <- name %||% names(name_to_id)
                           unname(name_to_id[name])
                         },


                         #' @description
                         #' Get the factor parent ids
                         #' @param type The type of edge link.
                         fct_id_parent = function(id = NULL, role = NULL, type = NULL) {
                           private$node_id_parent_child(id = id, role = role, type = type, node = "factor", return = "parent")
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

                         #' @description
                         #' Get the factor descendant ids
                         fct_id_descendant = function(id = NULL, role = NULL) {
                           private$var_id_descendant(id = id, role = role, node = "factor")
                         },

                         #' @description
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
                             lnodes_list <- lnodes_list[as.character(self$fct_id(role = role))]
                           }
                           if(is_null(fid)) {
                             if(is_null(value)) {
                               # return all lvl ids
                               return(unlist(map(lnodes_list, function(x) x$id)))
                             } else {
                               fid_search <- as.integer(names(lnodes_list))
                               fid <- self$fct_id_from_lvl_values(value = value, fid_search = fid_search)
                               return(self$lvl_id(value = value, role = role, fid = fid))
                             }
                           } else {
                             private$validate_id(fid, 1)
                             lnodes <- lnodes_list[[as.character(fid)]]
                             if(!is_null(value)) {
                               #lnodes[match(value, lnodes$value), ]$id
                               lnodes[match(as.character(value), as.character(lnodes$value)), ]$id
                             } else {
                               lnodes$id
                             }
                           }
                         },

                         #' @description
                         #' Get the level parent ids
                         lvl_id_parent = function(id = NULL, role = NULL) {
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

                         #' @description
                         #' Find the factor id from level ids.
                         #' @param fid_search A vector of fids to search from.
                         fct_id_from_lvl_id = function(id = NULL, fid_search = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid_search)) lnodes_list <- lnodes_list[as.character(fid_search)]
                           for(fname in names(lnodes_list)) {
                             if(all(id %in% lnodes_list[[fname]]$id)) return(as.integer(fname))
                           }
                         },

                         #' @description
                         #' Find the factor id from level values.
                         #' @param fid_search A vector of fids to search from.
                         fct_id_from_lvl_values = function(value = NULL, fid_search = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid_search)) lnodes_list <- lnodes_list[fid_search]
                           for(fname in names(lnodes_list)) {
                             if(all(value %in% lnodes_list[[fname]]$value)) return(as.integer(fname))
                           }
                         },

                         #' @description
                         #' Find the level id from the given fid
                         lvl_id_from_fct_id = function(fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           lnodes_list[[as.character(fid)]]$id
                         },

                         #' @description
                         #' Get the factor names based on id or role
                         fct_names = function(id = NULL, role = NULL) {
                           fnodes <- self$fct_nodes
                           if(!is_null(role)) {
                             private$validate_role(role)
                             fnodes <- fnodes[fnodes$role %in% role, ]
                           }
                           id <-  id %||% fnodes$id
                           fnodes[match(id, fnodes$id), ]$name
                         },

                         #' @description
                         #' Get the unit names
                         unit_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_unit")
                         },

                         #' @description
                         #' Get the treatment names
                         trt_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_trt")
                         },

                         #' @description
                         #' Get the record names.
                         rcrd_names = function(id = NULL) {
                           self$fct_names(id = id, role = "edbl_rcrd")
                         },

                         #' @description
                         #' Get the class for record with validation.
                         rcrd_class = function(name = NULL) {
                           map_chr(name,
                                   function(x) {
                                     vrcrds <- private$validation[["rcrds"]]
                                     if(x %in% names(vrcrds)) return(vrcrds[[x]]$record)
                                     NA_character_
                                   })
                         },

                         #' @description
                         #' Get the level values based on id or role
                         #' cannot have just role only defined.
                         #' id must be from the same fid
                         lvl_values = function(id = NULL, role = NULL, fid = NULL) {
                           lnodes_list <- self$lvl_nodes
                           if(!is_null(fid)) {
                             private$validate_id(fid, 1, role = role)
                             lnodes <- lnodes_list[[as.character(fid)]]
                             id <- id %||% lnodes$id
                             return(lnodes[match(id, lnodes$id), ]$value)
                           }
                           if(!is_null(id)) {
                             fid <- self$fct_id_from_lvl_id(id = id)
                             self$lvl_values(id = id, role = role, fid = fid)
                           } else {
                             abort("`id` or `fid` must be provided.")
                           }
                         },

                         #' @description
                         #' Get the unit values.
                         unit_values = function(id = NULL, fid = NULL) {
                           self$lvl_values(id = id, role = "edbl_unit", fid = fid)
                         },

                         #' @description
                         #' Get the treatment values.
                         trt_values = function(id = NULL, fid = NULL) {
                           self$lvl_values(id = id, role = "edbl_trt", fid = fid)
                         },

                         #' @description
                         #' Get the record values.
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
                         #' Get the role of the vertex given the factor id
                         fct_role = function(id = NULL) {
                           fnodes <- self$fct_nodes
                           id <- id %||% fnodes$id
                           fnodes[match(id, fnodes$id), ]$role
                         },

                         #' @description
                         #' Get the levels for each factor
                         fct_levels = function(id = NULL, name = NULL, return = c("id", "value")) {
                           return <- match.arg(return)
                           qid <- id %||% self$fct_id(name = name)
                           lnodes <- self$lvl_nodes
                           switch(return,
                                  id = lapply(unclass(lnodes[as.character(qid)]), function(x) x$id),
                                  value = {
                                    out <- lapply(unclass(lnodes[as.character(qid)]), function(x) x$value)
                                    names(out) <- self$fct_names(id = qid)
                                    out
                                  })
                         },

                         #' @description
                         #' Factor levels to edble factor
                         #' @param fct_levels The factor levels in id.
                         fct_levels_id_to_edbl_fct = function(fct_levels, role) {
                           ret <- lapply(names(fct_levels), function(fid) {
                             lvls <- fct_levels[[fid]]
                             fid <- as.numeric(fid)
                             fname <- self$fct_names(id = fid)
                             lvls_value <- self$lvl_values(id = lvls, fid = fid)
                             new_edibble_fct(labels = lvls_value,
                                             name = fname,
                                             class = role)
                           })
                           names(ret) <- self$fct_names(id = as.numeric(names(fct_levels)))
                           ret
                         },

                         #' @description
                         #' Get the factor levels in value given id format
                         #' @param  fct_levels A list of factor levels in id format.
                         fct_levels_id_to_value = function(fct_levels) {
                           out <- lapply(names(fct_levels), function(fid) {
                             lvls <- fct_levels[[fid]]
                             self$lvl_values(id = lvls, fid = as.numeric(fid))
                           })
                           names(out) <- self$fct_names(id = as.numeric(names(fct_levels)))
                           out
                         },

                         #' @description
                         #' Get the factor levels in id given value format.
                         #' @param  fct_levels A list of factor levels in id format.
                         fct_levels_value_to_id = function(fct_levels) {
                           out <- lapply(names(fct_levels), function(fname) {
                             lvls <- fct_levels[[fname]]
                             self$lvl_id(value = lvls, fid = self$fct_id(name = fname))
                           })
                           names(out) <- self$fct_id(name = names(fct_levels))
                           out
                         },


                         #' @description
                         #' One of `name`, `id` or `role` is defined to check if it exists.
                         #' If more than one of the arguments `name`, `id` and `role` are supplied, then
                         #' the intersection of it will be checked.
                         fct_exists = function(id = NULL, name = NULL, role = NULL, abort = TRUE) {
                           exist <- TRUE
                           abort_missing <- function(vars = NULL, msg = NULL) {
                             if(abort & !exist) {
                               abort(msg)
                             }
                           }

                           msg_vars_missing <- function(vars, post_msg) {
                             sprintf(paste0("%s ", post_msg),
                                     .combine_words(paste0("`", vars, "`")))
                           }

                           fnodes <- self$fct_nodes
                           # at least one node exists
                           if(is_null(name) & is_null(id) & is_null(role)) {
                             exist <- nrow(fnodes) > 0
                             abort_missing(msg = "There are no factor nodes.")

                           } else if(!is_null(name) & is_null(id) & is_null(role)) {
                             vexist <- name %in% fnodes$name
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist],
                                           msg = msg_vars_missing(name[!vexist], "does not exist in the design."))

                           } else if(is_null(name) & !is_null(id) & is_null(role)) {
                             vexist <- id %in% fnodes$id
                             exist <- all(vexist)
                             abort_missing(vars = id[!vexist],
                                           msg = msg_vars_missing(id[!vexist], "does not exist in the design."))

                           } else if(is_null(name) & is_null(id) & !is_null(role)) {
                             exist <- any(role %in% fnodes$role)
                             abort_missing(msg = sprintf("There are no factors with role %s",
                                                         .combine_words(paste0("`", role, "`"))))

                           } else if(is_null(name) & !is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(id, fnodes$id), "role", drop = TRUE]
                             vexist <- srole %in% role
                             exist <- all(vexist)
                             abort_missing(vars = id[!vexist],
                                           msg = msg_vars_missing(id[!vexist], "doesn't exist or don't have the specified role."))

                           } else if(!is_null(name) & is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(name, fnodes$name), "role", drop = TRUE]
                             vexist <- srole %in% role
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist],
                                           msg = msg_vars_missing(name[!vexist], "doesn't exist or don't have the specified role."))

                           } else if(!is_null(name) & !is_null(id) & is_null(role)) {
                             sid <- fnodes[match(name, fnodes$name), "id", drop = TRUE]
                             vexist <- sid %in% id
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist],
                                           msg = msg_vars_missing(name[!vexist], "doesn't exist or have a specified id."))

                           } else {
                             snodes <- fnodes[match(name, fnodes$name), ]
                             vexist <- snodes$id %in% id & snodes$role %in% role
                             exist <- all(vexist)
                             abort_missing(vars = name[!vexist],
                                           msg = msg_vars_missing(name[!vexist], "doesn't exist or have a specified id or role."))
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



                         #lvl_exists = function(id = NULL, value = NULL, fid = NULL, abort = TRUE) {
                           # FIXME
                         #},

                         #' @description
                         #' Given node data, append the factor nodes
                         append_fct_nodes = function(name, role, attrs = NULL) {
                           private$record_track_internal()
                           fnodes <- self$fct_nodes
                           n <- length(name)
                           role <- vctrs::vec_recycle(role, n)
                           data <- tibble::tibble(id = private$fct_new_id(n = n),
                                                  role = role,
                                                  name = name)
                           out <- rbind_(fnodes[setdiff(names(fnodes), "attrs")], data)
                           out_attrs <- rbind_(fnodes$attrs, attrs %||% data.frame())
                           if(nrow(out_attrs) < nrow(out)) {
                             out_attrs <- data.frame(row.names = nrow(out))
                           }
                           out$attrs <- out_attrs
                           self$fct_nodes <- out
                         },

                         #' @description
                         #' Given node data, append the level nodes
                         #' @param n The number of replications.
                         #' @param label The labels for the levels.
                         append_lvl_nodes = function(value, n = NULL, label = NULL, attrs = NULL, fid = NULL) {
                           private$record_track_internal()
                           lnodes <- self$lvl_nodes
                           id <- private$lvl_new_id(n = length(value))
                           if(is_null(n) & is_null(label)) {
                             data <- tibble::tibble(id = id, value = value, attrs = attrs)
                           } else if(!is_null(n) & is_null(label)) {
                             data <- tibble::tibble(id = id, value = value, n = n, attrs = attrs)
                           } else if(is_null(n) & !is_null(label)) {
                             data <- tibble::tibble(id = id, value = value, label = label, attrs = attrs)
                           } else if(!is_null(n) & !is_null(label)) {
                             data <- tibble::tibble(id = id, value = value, n = n, label = label, attrs = attrs)
                           }
                           if(is.null(lnodes[[as.character(fid)]])) {
                             lnodes[[as.character(fid)]] <- data
                           } else {
                             lnodes[[as.character(fid)]] <- rbind_(lnodes[[as.character(fid)]], data)
                           }
                           self$lvl_nodes <- lnodes
                         },

                         #' @description
                         #' Given edge data, append the factor edges
                         #' @param from The node id from.
                         #' @param to The node id to.
                         #' @param type The type of edges.
                         #' @param group The group id.
                         append_fct_edges = function(from, to, type = NULL, group = NULL, attrs = NULL) {
                           private$record_track_internal()
                           self$fct_edges <- rbind_(self$fct_edges, tibble::tibble(from = from,
                                                                                   to = to,
                                                                                   type = type,
                                                                                   group = group,
                                                                                   attrs = attrs))
                         },

                         #' @description
                         #' Given edge data, append the level edges
                         #' @param from The node id from.
                         #' @param to The node id to.
                         append_lvl_edges = function(from, to, attrs = NULL) {
                           private$record_track_internal()
                           self$lvl_edges <- rbind_(self$lvl_edges, tibble::tibble(from = from,
                                                                                   to = to,
                                                                                   attrs = attrs))
                         },

                         #' @description
                         #' Serve the units.
                         serve_units = function(id = NULL, return = c("id", "value")) {
                           return <- match.arg(return)
                           self$fct_exists(id = id, role = "edbl_unit")
                           id <- id %||% self$fct_id(role = "edbl_unit")
                           if(length(id) == 0) abort("There needs to be at least one unit supplied.")
                           id_ancestors <- self$fct_id_ancestor(id = id, role = "edbl_unit")
                           sub_graph <- self$graph_subset(id = id_ancestors, include = "self")
                           out <- private$build_subtable(sub_graph, return = "id")

                           private$table$units <- out
                           switch(return,
                                  id = out,
                                  value = self$fct_levels_id_to_edbl_fct(out, role = "edbl_unit"))
                         },

                         #' @description
                         #' Serve treatments
                         serve_trts = function(id = NULL, return = c("id", "value")) {

                           return <- match.arg(return)
                           lnodes <- self$lvl_nodes
                           ledges <- self$lvl_edges

                           serve_trt = function(fid) {
                             # linked unit -
                             # each treatment factor should only be applied to a single unit factor
                             uid <- self$fct_id_child(id = fid, role = "edbl_unit")
                             vctrs::vec_assert(uid, integer(), size = 1)
                             if(!uid %in% as.integer(names(private$table$units))) self$serve_units(id = uid)
                             tids <- self$lvl_id(fid = fid, role = "edbl_trt")
                             uids <- private$table$units[[as.character(uid)]]
                             ledges <- ledges[ledges$to %in% uids & ledges$from %in% tids, ]
                             ledges[match(uids, ledges$to), ]$from
                           }
                           id <- id %||% self$trt_ids
                           out <- lapply(id, serve_trt)
                           names(out) <- as.character(id)
                           private$table$trts <- out
                           switch(return,
                                  id = out,
                                  value = self$fct_levels_id_to_edbl_fct(out, role = "edbl_trt"))
                         },


                         #' @description
                         #' Serve records
                         serve_rcrds = function(id = NULL, return = c("id", "value")) {
                           id <- id %||% self$rcrd_ids
                           return <- match.arg(return)
                           out <- lapply(id, function(rid) {
                             uid <- self$fct_id_child(id = rid, role = "edbl_unit")
                             # should be only a single unit factor
                             vctrs::vec_assert(uid, integer(), size = 1)
                             if(!uid %in% as.integer(names(private$table$units))) self$serve_units(id = uid)
                             uid
                           })
                           names(out) <- as.character(id)
                           switch(return,
                                  id = out,
                                  value = {
                                    N <- max(lengths(private$table$units))
                                    lvs <- lapply(id, function(rid) {
                                      uid <- out[[as.character(rid)]]
                                      uids <- private$table$units[[as.character(uid)]]
                                      new_edibble_rcrd(rep(NA_real_, N), uids)
                                    })
                                    names(lvs) <- self$fct_names(id = id)
                                    lvs
                                  })
                         },

                         #' @description
                         #' Make the treatments table
                         #' @return A treatment table
                         make_trts_table = function(id = NULL, return = c("id", "value")) {
                           id <- id %||% self$trt_ids
                           return <- match.arg(return)
                           self$fct_exists(id = id, role = "edbl_trt")
                           fnodes <- self$fct_graph_components(id = id)
                           ncomp <- length(unique(fnodes$component))
                           scomps <- split(fnodes$id, fnodes$component)
                           if(ncomp == length(id)) {
                             trts_list <- self$fct_levels(id = id, return = return)
                             trts_tbl <- expand.grid(trts_list, stringsAsFactors = FALSE)
                           } else {
                             for(i in seq(ncomp)) {
                               if(i == 1L) {
                                 sub_graph <- self$graph_subset(id = scomps[[i]], include = "self")
                                 trts_tbl <- private$build_condtable(sub_graph, return = return)
                               } else {
                                 sub_graph <- self$graph_subset(id = scomps[[i]], include = "self")
                                 new_trts_tbl <- private$build_condtable(sub_graph, return = return)
                                 xtabs <- expand.grid(old = seq(nrow(trts_tbl)), new = seq(nrow(new_trts_tbl)))
                                 trts_tbl <- cbind(trts_tbl[xtabs[[1]], , drop = FALSE], new_trts_tbl[xtabs[[2]], , drop = FALSE])
                               }
                             }
                           }
                           rownames(trts_tbl) <- NULL
                           switch(return,
                                  id = trts_tbl[as.character(id)],
                                  value = trts_tbl[self$fct_names(id = id)])
                         },

                         #' @description
                         #' Subset graph
                         #' @param include "self" for only input id, "child" for child also,
                         #'   "parent" for parent also,
                         #'   nodes immediately related, and "ancestors" for all ancestors
                         #' @return subsetted graph
                         graph_subset = function(id = NULL, include = c("self", "child", "parent", "ancestors")) {
                           include <- match.arg(include)
                           idx <- switch(include,
                                    "self" = id,
                                    "child" = c(id, self$fct_id_child(id = id)),
                                    "parent" = c(id, self$fct_id_parent(id = id)),
                                    "ancestors" = unique(c(id, self$fct_id_ancestor(id = id))))
                           fnodes <- self$fct_nodes
                           fedges <- self$fct_edges
                           lnodes <- self$lvl_nodes
                           ledges <- self$lvl_edges
                           # subset
                           fnodes <- fnodes[fnodes$id %in% idx, ]
                           fedges <- fedges[fedges$from %in% idx & fedges$to %in% idx, ]

                           lnodes <- lnodes[as.character(fnodes$id)]
                           lnodes_id <- unlist(lapply(lnodes, function(x) x$id))
                           ledges <- ledges[ledges$from %in% lnodes_id & ledges$to %in% lnodes_id, ]

                           new_edibble_graph(fnodes, lnodes, fedges, ledges)
                         },

                         #' @description
                         #' Save the seed
                         #' @param seed A seed.
                         #' @param type Type.
                         save_seed = function(seed, type) {
                           private$record_track_internal()
                           if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                             stats::runif(1)
                           if (is.null(seed))
                             RNGstate <- get(".Random.seed", envir = .GlobalEnv)
                           else {
                             set.seed(seed)
                             RNGstate <- structure(seed, kind = as.list(RNGkind()))
                           }
                           private$seed[[type]] <- RNGstate
                         },

                         #' @description
                         #' Get the title
                         get_title = function() {
                           private$title
                         },

                         #' @description
                         #' Get the validation
                         #' @param type A type.
                         get_validation = function(type = NULL) {
                           if(is_null(type)) return(private$validation)
                           private$validation[[type]]
                         },

                         #' @description
                         #' Get the trail.
                         get_trail = function() {
                           private$trail[-length(private$trail)]
                         },

                         #' @description
                         #' Get the graph
                         get_graph = function() {
                           private$graph
                         },

                         #' @description
                         #' Get the seed
                         get_seed = function() {
                           private$seed
                         },

                         #' @description
                         #' Get the session information
                         get_session_info = function() {
                           private$session_info
                         },

                         #' @description
                         #' Get the edibble version.
                         get_edibble_version = function() {
                           private$edibble_version
                         },

                         #' @description
                         #' Get the simulation information
                         #' @param name The process name. Only one name allowed.
                         get_simulate = function(name = NULL) {
                           if(is_null(name)) return(private$simulate)
                           private$simulate[[name]]
                         },

                         #' @description
                         #' Get the simulation results
                         #' @param name The process name. Only one name allowed.
                         get_simulate_result_env = function(name = NULL) {
                           if(is_null(name)) return(private$simulate_result_env)
                           private$simulate_result_env[[name]]
                         },

                         #' @description
                         #' Mapping of a role to role
                         #' @param role_from The role from.
                         #' @param role_to The role to.
                         mapping = function(role_from, role_to) {
                           ids <- self$fct_id(role = role_from)
                           out <- map_int(ids, function(id_from) {
                             id_to <- self$fct_id_child(id = id_from, role = role_to)
                             vctrs::vec_assert(id_to, integer(), size = 1)
                             id_to
                           })
                           names(out) <- ids
                           out
                         },

                         #' @description
                         #' Mapping of an id to a unit
                         mapping_to_unit = function(id = NULL) {
                           self$fct_id_child(id = id, role = "edbl_unit")
                         },

                         #' @description
                         #' Record step.
                         record_step = function() {
                           do.call("on.exit",
                                   list(quote(return(add_edibble_code(returnValue(default = FALSE),
                                                                      paste(gsub("(^ +| +$)", "", deparse(match.call())), collapse = "")))),
                                        add = TRUE),
                                   envir = parent.frame())
                         },

                         #' @description
                         #' Get the level edges by factor
                         #' @param from,to The factor id.
                         lvl_mapping = function(from, to, return = c("vector", "table")) {
                           return <- match.arg(return)
                           lnodes <- self$lvl_nodes
                           nodes_from <- lnodes[[as.character(from)]]$id
                           nodes_to <- lnodes[[as.character(to)]]$id
                           ledges <- self$lvl_edges
                           map <- subset(ledges, from %in% nodes_from & to %in% nodes_to,
                                         select = c(from, to))
                           if(return=="vector") return(setNames(map$to, map$from))
                           map
                         },


                         #' @description
                         #' Record track external.
                         #' @param code The code to record.
                         record_track_external = function(code) {
                           ncmds <- length(private$trail)
                           attr(private$trail[[ncmds]], "external_cmd") <- code
                           attr(private$trail[[ncmds]], "execution_time") <- Sys.time()
                           attr(private$trail[[ncmds]], "time_zone") <- Sys.timezone()
                           private$trail[[ncmds + 1]] <- new_trackable()
                         },

                         #' @description
                         #' Find all id that is linked.
                         #' @param link Whether the link should be direct or indirect
                         #' @return id of linked factors, excluding itself.
                         fct_id_links = function(id = NULL, role = NULL, link = c("direct", "indirect")) {
                           link <- match.arg(link)
                           if(link == "direct") {
                              cid <- self$fct_id_child(id = id, role = role)
                              pid <- self$fct_id_parent(id = id, role = role)

                           }
                           if(link == "indirect") {
                             cid <- self$fct_id_ancestor(id = id, role = role)
                             pid <- self$fct_id_descendant(id = id, role = role)
                           }
                           return(c(cid, pid))
                         },

                         #' @description
                         #' Get the nodes with components (subgraph number)
                         fct_graph_components = function(id = NULL) {
                           fnodes <- self$fct_nodes
                           fnodes <- fnodes[fnodes$id %in% id, ]
                           fedges <- self$fct_edges
                           fedges <- fedges[fedges$to %in% fnodes$id & fedges$from %in% fnodes$id, ]
                           fnodes$component <- NA_integer_

                           label_component <- function(fnodes, comp) {
                             id <- fnodes$id[is.na(fnodes$component)][1]
                             pids <- self$fct_id_ancestor(id = id)
                             cids <- self$fct_id_descendant(id = id)
                             fnodes$component[match(c(pids, cids), fnodes$id)] <- comp
                             fnodes
                           }
                           comp <- 0L

                           while(any(is.na(fnodes$component))) {
                             comp <- comp + 1L
                             fnodes <- label_component(fnodes, comp)
                           }
                           fnodes
                         },

                         #' @description
                         #' Get the nodes with components (subgraph number)
                         lvl_graph_components = function() {
                           lnodes <- self$lvl_nodes
                           ledges <- self$lvl_edges
                           lnodes$component <- NA_integer_

                           label_component <- function(comp) {
                             id <- lnodes$id[is.na(lnodes$component)][1]
                             pids <- self$lvl_id_ancestor(id = id)
                             cids <- self$lvl_id_descendant(id = id)
                             lnodes$component[match(c(pids, cids), lnodes$id)] <- comp
                             lnodes
                           }
                           comp <- 0L

                           while(any(is.na(lnodes$component))) {
                             comp <- comp + 1L
                             lnodes <- label_component(comp)
                           }
                           lnodes
                         }

                       ),

                       active = list(
                         #' @field fct_nodes
                         #' Get the factor nodes
                         fct_nodes = function(data) {
                           if(missing(data)) return(private$graph$factors$nodes)
                           else private$graph$factors$nodes <- data
                         },

                         #' @field lvl_nodes
                         #' Get the level nodes
                         lvl_nodes = function(data) {
                           if(missing(data)) {
                             nodes <- private$graph$levels$nodes
                             return(nodes)
                           }
                           else private$graph$levels$nodes <- data
                         },

                         #' @field fct_edges
                         #' Get the factor edges
                         fct_edges = function(data) {
                           if(missing(data)) {
                             edges <- private$graph$factors$edges
                             edges$var_from <- self$fct_names(id = edges$from)
                             edges$var_to <- self$fct_names(id = edges$to)
                             return(edges)
                           } else {
                             private$graph$factors$edges <- data
                           }
                         },

                         #' @field lvl_edges
                         #' Get the level edges
                         lvl_edges = function(data) {
                           if(missing(data)) {
                             edges <- private$graph$levels$edges
                             return(edges)
                           } else {
                             private$graph$levels$edges <- data
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
                             sum(lengths(self$lvl_nodes))
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
                           # FIXME: use lvl_graph_components instead
                           nvar <- self$fct_n - length(self$rcrd_ids)
                           if(nvar==0) return(FALSE)
                           if(nvar==1) return(TRUE)
                           ledges <- self$lvl_edges
                           all(self$lvl_id() %in% c(ledges$to, ledges$from))
                         }
                       ),
                       private = list(
                         fct_id_last = 0L,
                         lvl_id_last = 0L,

                         title = NULL,
                         name = NULL,
                         seed = list(),
                         edbl_version = NULL,
                         session_info = NULL,
                         trail = NULL,

                         anatomy = NULL,
                         recipe = NULL,
                         graph = NULL,
                         simulate = list(),
                         simulate_result_env = new_environment(),
                         validation = list(rcrds = NULL),
                         # table should only contain the id of levels and factors
                         table = list(units = NULL, trts = NULL, rcrds = NULL),

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

                         node_id_parent_child = function(id = NULL, role = NULL, type = NULL, node = c("factor", "level"), return = c("child", "parent")) {
                           return <- match.arg(return)
                           node <- match.arg(node)
                           if(node == "factor") {
                             edges <- self$fct_edges
                             #edges <- edges[!edges$type %in% c("depends", "cross"), ]
                           } else if(node == "level") {
                             edges <- self$lvl_edges
                           }
                           child_ids <- edges$to
                           parent_ids <- edges$from
                           role_ids <- self$fct_id(role = role)
                           if(is_null(type)) {
                             if(return == "parent") return(parent_ids[child_ids %in% id & parent_ids %in% role_ids])
                             if(return == "child") return(child_ids[parent_ids %in% id & child_ids %in% role_ids])
                           } else {
                             if(return == "parent") return(parent_ids[child_ids %in% id & parent_ids %in% role_ids & edges$type %in% type])
                             if(return == "child") return(child_ids[parent_ids %in% id & child_ids %in% role_ids & edges$type %in% type])
                           }
                        },

                        var_id_ancestor = function(id = NULL, role = NULL, node = c("factor", "level")) {
                          out <- unique(id)
                          parent_ids <- private$node_id_parent_child(id = id, role = role, node = node, return = "parent")
                          if(!is_empty(parent_ids)) {
                            out <- unique(c(out, private$var_id_ancestor(id = parent_ids, role = role, node = node)))
                          }
                          out
                        },

                        var_id_descendant = function(id = NULL, role = NULL, node = c("factor", "level")) {
                          out <- unique(id)
                          child_ids <- private$node_id_parent_child(id = id, role = role, node = node, return = "child")
                          if(!is_empty(child_ids)) {
                            out <- unique(c(out, private$var_id_descendant(id = child_ids, role = role, node = node)))
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


                         #' Get a new factor id.
                         fct_new_id = function(n = 1) {
                           ids <- seq(private$fct_id_last + 1, private$fct_id_last + n)
                           private$fct_id_last <- private$fct_id_last + n
                           ids
                         },

                         #' Get a new level id.
                         lvl_new_id = function(n = 1) {
                           ids <- seq(private$lvl_id_last + 1, private$lvl_id_last + n)
                           private$lvl_id_last <- private$lvl_id_last + n
                           ids
                         },

                        #' Given a particular DAG, return a topological order
                        #' Remember that there could be more than one order.
                        graph_topological_order = function(graph, reverse = TRUE) {
                          fnodes <- graph$factors$nodes
                          lnodes <- graph$levels$nodes
                          fedges <- graph$factors$edges
                          ledges <- graph$levels$edges
                          fnodes$parent <- map_int(fnodes$id, function(id) sum(fedges$to %in% id))
                          fnodes$child <- map_int(fnodes$id, function(id) sum(fedges$from %in% id))
                          fnodes$nlevels <- map_int(fnodes$id, function(id) nrow(lnodes[[as.character(id)]]))
                          if(reverse) {
                            fnodes <- fnodes[order(fnodes$parent==0, fnodes$child!=0, -fnodes$nlevels), ]
                          } else {
                            fnodes <- fnodes[order(fnodes$parent!=0, fnodes$child==0, -fnodes$nlevels), ]
                          }
                          new_edibble_graph(fnodes = fnodes, lnodes = lnodes, fedges = fedges, ledges = ledges)
                        },

                        build_condtable = function(subgraph, return) {
                          top_graph <- private$graph_topological_order(subgraph, reverse = FALSE)
                          sub_fnodes <- top_graph$factors$nodes
                          sub_fedges <- top_graph$factors$edges
                          sub_lnodes <- top_graph$levels$nodes
                          sub_ledges <- top_graph$levels$edges

                          out <- list()
                          if(nrow(sub_fnodes) == 1) {
                            iunit <- sub_fnodes$id[1]
                            out[[as.character(iunit)]] <- self$lvl_id(fid = iunit)
                          } else {
                            for(irow in 2:nrow(sub_fnodes)) {
                              iunit <- sub_fnodes$id[irow]
                              if(sub_fnodes$parent[irow] == 0) {
                                abort("This factor has no parents. This shouldn't happen.")
                              } else {
                                parent_id <- sub_fedges$from[sub_fedges$to == iunit]
                                map_tbl <- self$lvl_mapping(parent_id, iunit, return = "table")
                                colnames(map_tbl) <- c(parent_id, iunit)
                                if(as.character(parent_id) %in% names(out)) {
                                  trts_tbl <- do.call(tibble::tibble, out)
                                  map_tbl <- merge(trts_tbl, map_tbl)
                                }
                                for(anm in names(map_tbl)) {
                                  out[[anm]] <- map_tbl[[anm]]
                                }
                              }
                            }
                          }
                          ret <- switch(return,
                                        id = out,
                                        value = self$fct_levels_id_to_value(out))
                          do.call(tibble::tibble, ret)
                        },

                        build_subtable = function(subgraph, return) {
                          top_graph <- private$graph_topological_order(subgraph, reverse = TRUE)
                          sub_fnodes <- top_graph$factors$nodes
                          sub_fedges <- top_graph$factors$edges
                          sub_lnodes <- top_graph$levels$nodes
                          sub_ledges <- top_graph$levels$edges

                          out <- list()
                          for(irow in seq(nrow(sub_fnodes))) {
                            # check if children.
                            iunit <- sub_fnodes$id[irow]
                            if(sub_fnodes$child[irow] == 0) {
                              # if no children, just render it
                              out[[as.character(iunit)]] <- self$lvl_id(fid = iunit)
                            } else {
                              children_id <- sub_fedges$to[sub_fedges$from == iunit]
                              # all children id should have levels in the `out`
                              # any children should be the same -- take the first one

                              cid <- out[[as.character(children_id[1])]]
                              pid <- sub_lnodes[[as.character(iunit)]]$id
                              cid_to_pid <- map_int(cid, function(id) sub_ledges$from[sub_ledges$to == id & sub_ledges$from %in% pid])
                              out[[as.character(iunit)]] <- cid_to_pid
                            }
                          }
                          switch(return,
                                 id = out,
                                 value = self$fct_levels_id_to_value(out))
                        }
                       ))


#' Check if an object is an instance of the "Provenance" class.
#'
#' This function determines whether the given object is an instance of the
#' "Provenance" class.
#'
#' @param x An object to be checked for its class membership.
#'
#' @return \code{TRUE} if the object is an instance of the "Provenance" class,
#'   \code{FALSE} otherwise.
#' @export
is_provenance <- function(x) {
  inherits(x, "Provenance")
}
