
#' A manipulator for the edbl_design.
#'
#' Internal functions should create a new Kitchen object.
#' The Kitchen contains a set of operations to manipulate the nodes and edges of
#' the edibble design object.
#'
#' @param role The role for the vertex/node.
#' @param data The nodes data
#' @param name The name of the vertex.
#' @param id The id of the corresponding node.
#' @param fresh The value of the new graph structure to add.
#' @param initial The intial id.
#' @param abort Whether to abort.
#' @importFrom vctrs vec_is
#' @export
Kitchen <- R6::R6Class("Kitchen",
                       public = list(
                         #' @field design An edibble design object
                         design = NULL,

                         #' Initialise function
                         #' @param design An edibble design.
                         initialize = function(design = NULL) {
                           self$design <- design %||% design()
                         },



                         #' @description
                         #' Get the id based on either the name of the factor node.
                         #' If none supplied then it will give all.
                         fct_id_by_name = function(name = NULL) {
                           fnodes <- self$fct_nodes
                           name_to_id <- pull(fnodes, id, name)
                           name <- name %||% names(name_to_id)
                           unname(name_to_id[as.character(name)])
                         },

                         #' @description
                         #' Get all ids associated with a role.
                         fct_id_by_role = function(role = NULL) {
                           fnodes <- self$fct_nodes
                           fnodes[fnodes$role %in% role, "id"]
                         },

                         #' @description
                         #' Get the id based on name of level node.
                         #' If no name provided, all names returned.
                         #' FIXME
                         lvl_id_by_name = function(name = NULL) {
                           lnodes <- self$lvl_nodes
                           name_to_id <- pull(self$lvl_nodes, id, name)
                           name <- name %||% names(name_to_id)
                           unname(name_to_id[as.character(name)])
                           if(is_null(role)) {

                           } else {
                             ids <- fnodes[fnodes$role %in% role, "id"]
                             lnodes[lnodes$idvar %in% ids, "id"]
                           }
                         },

                         #' @description
                         #' Get the factor names based on id or role
                         fct_names = function(id = NULL, role = NULL) {
                           private$var_names(self$fct_nodes, id, role)
                         },

                         #' @description
                         #' Get the level names based on id or role
                         lvl_names = function(id = NULL, role = NULL) {
                           private$var_names(self$lvl_nodes, id, role)
                         },

                         #' @description
                         #' Given node data, append the factor nodes
                         append_fct_nodes = function(data) {
                           self$fct_nodes <- rbind_(self$fct_nodes, data)
                         },

                         #' @description
                         #' Given node data, append the level nodes
                         append_lvl_nodes = function(data, fid = NULL) {
                           lnodes <- self$lvl_nodes
                           if(is.null(lnodes[[as.character(fid)]])) {
                             lnodes[[as.character(fid)]] <- new_lnode(data$id, data$value, data$attrs)
                           } else {
                             lnodes[[as.character(fid)]] <- rbind_(lnodes[[as.character(fid)]], data)
                           }
                           self$lvl_nodes <- lnodes
                         },

                         #' @description
                         #' Given edge data, append the factor edges
                         append_fct_edges = function(data) {
                           self$fct_edges <- rbind_(self$fct_edges, data)
                         },

                         #' @description
                         #' Given edge data, append the level edges
                         append_lvl_edges = function(data) {
                           self$lvl_edges <- rbind_(self$lvl_edges, data)
                         },

                         #' @description
                         #' Get the role of the vertex given the factor id
                         fct_role = function(id = NULL) {
                           nodes <- self$fct_nodes
                           id_to_role_fct <- pull(nodes, role, id)
                           ids_fct <-  id %||% nodes$id
                           unname(id_to_role_fct[as.character(ids_fct)])
                         },

                         #' @description
                         #' Get the role of the vertex given the level id
                         lvl_role = function(id = NULL) {
                           nodes <- self$lvl_nodes
                           id_to_role_fct <- pull(nodes, role, id)
                           ids_fct <-  id %||% nodes$id
                           unname(id_to_role_fct[as.character(ids_fct)])
                         },

                         #' @description
                         #' Get the factor child ids. If `role` is
                         #' supplied then the child has to fit `role`
                         fct_child = function(id = NULL, role = NULL) {
                           edges <- subset(self$fct_edges, !type %in% c("depends", "cross"))
                           child_ids <- edges$to
                           parent_ids <- edges$from
                           child_ids[parent_ids %in% id & child_ids %in% self$fct_id_by_role(role = role)]
                         },

                         #' @description
                         #' Get the level child ids
                         lvl_child = function(id = NULL, role = NULL) {
                           edges <- self$lvl_edges
                           child_ids <- edges$to
                           parent_ids <- edges$from
                           child_ids[parent_ids %in% id & child_ids %in% self$lvl_id(role = role)]
                         },

                         #' @description
                         #' Get the factor parent ids
                         fct_parent = function(id = NULL, role = NULL) {
                           edges <- subset(self$fct_edges, !type %in% c("depends", "cross"))
                           role_ids <- self$fct_id_by_role(role = role)
                           parent_ids <- edges$from
                           child_ids <- edges$to
                           parent_ids[child_ids %in% id & parent_ids %in% role_ids & child_ids %in% role_ids]
                         },

                         #' @description
                         #' Get the level parent ids
                         lvl_parent = function(id = NULL, role = NULL) {
                           edges <- self$lvl_edges
                           role_ids <- self$lvl_id(role = role)
                           parent_ids <- edges$from
                           child_ids <- edges$to
                           parent_ids[child_ids %in% id & parent_ids %in% role_ids & child_ids %in% role_ids]
                         },


                         #' @description
                         #' Get the factor ancestor ids
                         fct_ancestor = function(id = NULL, role = NULL) {
                           out <- unique(id)
                           parent_ids <- self$fct_parent(id = id, role = role)
                           if(!is_empty(parent_ids)) {
                             out <- unique(c(out, self$fct_ancestor(id = parent_ids, role = role)))
                           }
                           out
                         },


                         #' @description
                         #' Get the level ancestor ids
                         lvl_ancestor = function(id = NULL, role = NULL) {
                           out <- id
                           parent_ids <- self$lvl_parent(id = id, role = role)
                           if(!is_empty(parent_ids)) {
                             out <- c(out, self$lvl_ancestor(id = parent_ids, role = role))
                           }
                           out
                         },

                         #' @description
                         #' Get the levels for each factor
                         fct_levels = function(id = NULL, name = NULL) {
                           qid <- id %||% self$fct_id_by_name(name)
                           lnodes <- self$lvl_nodes
                           lnodes[as.character(qid)]
                         },



                         #' @description
                         #' Setup the node and edge data
                         setup_data = function(fresh, name, role) {
                           setup_data_internal <- private$next_method("setup_data", class(fresh))
                           setup_data_internal(fresh, name, role)
                         },

                         #' @description
                         #' Add the anatomy structure
                         add_anatomy = function(fresh, name, role) {
                           if(role=="edbl_unit") {
                             if(is.null(self$design$anatomy)) {
                               self$design$anatomy <- as.formula(paste0("~", name))
                             } else {
                               term <- ifelse(inherits(fresh, "nest_lvls"),
                                              paste0(attr(fresh, "keyname"), "/", name),
                                              name)
                               self$design$anatomy <- update(self$design$anatomy,
                                                             as.formula(paste0("~ . + ", term)), evaluate = FALSE)
                             }
                           }
                         },


                         #' @description
                         #' One of `name`, `id` or `role` is defined to check if it exists.
                         #' If more than one of the arguments `name`, `id` and `role` are supplied, then
                         #' the intersection of it will be checked.
                         #' @param abort A logical value to indicate whether to abort if it doesn't exist.
                         fct_exists = function(name = NULL, id = NULL, role = NULL, abort = TRUE) {

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
                             exist <- any(role %in% fnodes$role)
                             abort_missing(msg = sprintf("There are no factors with role%s",
                                                         .combine_words(paste0("`", role, "`"))))

                           } else if(is_null(name) & !is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(id, fnodes$id), "role"]
                             vexist <- srole == role
                             exist <- all(vexist)
                             abort_missing(vars = id[!vexist])

                           } else if(!is_null(name) & is_null(id) & !is_null(role)) {
                             srole <- fnodes[match(name, fnodes$name), "role"]
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
                         trts_exists = function(abort = TRUE) {
                           self$fct_exists(role = "edbl_trt", abort = abort)
                         },

                         #' @description
                         #' Check if unit exists.
                         units_exists = function(abort = TRUE) {
                           self$fct_exists(role = "edbl_unit", abort = abort)
                         },

                         #' @description
                         #' Check if record exists.
                         rcrds_exists = function(abort = TRUE) {
                           self$fct_exists(role = "edbl_rcrd", abort = abort)
                         }

                       ),

                       active = list(

                         #' @field fct_nodes
                         #' Get the factor nodes
                         fct_nodes = function(data) {
                           if(missing(data)) return(self$design$graph$factors$nodes)
                           else self$design$graph$factors$nodes <- data
                         },

                         #' @field lvl_nodes
                         #' Get the level nodes
                         lvl_nodes = function(data) {
                           if(missing(data)) {
                             nodes <- self$design$graph$levels$nodes
                             return(nodes)
                           }
                           else self$design$graph$levels$nodes <- data
                         },

                         #' @field fct_edges
                         #' Get the factor edges
                         fct_edges = function(data) {
                           if(missing(data)) {
                             edges <- self$design$graph$factors$edges
                             edges$var_from <- self$fct_names(id = edges$from)
                             edges$var_to <- self$fct_names(id = edges$to)
                             return(edges)
                           } else {
                             self$design$graph$factors$edges <- data
                           }
                         },

                         #' @field lvl_edges
                         #' Get the level edges
                         lvl_edges = function(data) {
                           if(missing(data)) {
                             edges <- self$design$graph$levels$edges
                             edges$lvl_from <- self$lvl_names(id = edges$from)
                             edges$lvl_to <- self$lvl_names(id = edges$to)
                             return(edges)
                           } else {
                             self$design$graph$levels$edges <- data
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



                         #' @field fct_leaves
                         #' Get the leave factor ids.
                         fct_leaves = function() {
                           uids <- self$fct_id_by_role("edbl_unit")
                           has_child <- map_lgl(uids, function(id) length(intersect(self$fct_child(id), uids)) > 0)
                           uids[!has_child]
                         },

                         #' @field rcrd_ids
                         #' Get the ids for all edbl_rcrd factors.
                         rcrd_ids = function() {
                           self$fct_id_by_role("edbl_rcrd")
                         },

                         #' @field unit_ids
                         #' Get the ids for all edbl_unit factors.
                         unit_ids = function() {
                           self$fct_id_by_role("edbl_unit")
                         },

                         #' @field trt_ids
                         #' Get the ids for all edbl_trt factors.
                         trt_ids = function() {
                           self$fct_id_by_role("edbl_trt")
                         },

                         #' @field trt_names
                         #' Get the node labels for treatments
                         trt_names = function() {
                           private$var_names(self$fct_nodes, role = "edbl_trt")
                         },

                         #' @field unit_names
                         #' Get the node labels for units
                         unit_names = function() {
                           private$var_names(self$fct_nodes, role = "edbl_unit")
                         },

                         #' @field rcrd_names
                         #' Get the node labels for record
                         rcrd_names = function() {
                           private$var_names(self$fct_nodes, role = "edbl_rcrd")
                         },

                         #' @field is_connected
                         #' Check if nodes are connected.
                         is_connected = function() {
                           nvar <- self$fct_n - length(self$rcrd_ids)
                           if(nvar==0) return(FALSE)
                           if(nvar==1) return(TRUE)
                           lnodes <- self$lvl_nodes
                           ledges <- self$lvl_edges
                           all(lnodes$id %in% c(ledges$to, ledges$from))
                         }


                       ),
                       private = list(
                         version = NULL,
                         fct_last_id = 0L,
                         lvl_last_id = 0L,

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
                         },

                         var_names = function(nodes, id, role) {
                           if(is_null(role)) {
                             id_to_name <- pull(nodes, name, id)
                             ids <-  id %||% nodes$id
                             unname(id_to_name[as.character(ids)])
                           } else {
                             nodes <- self$fct_nodes
                             if(is_null(role)) {
                               nodes$name
                             } else {
                               nodes[nodes$role %in% role, "name"]
                             }
                           }
                         },

                         fresh_type = function(fresh) {
                           if(is_edibble_levels(fresh)) return("edbl_lvls")
                           if(is_nest_levels(fresh)) return("nest_lvls")
                           if(vec_is(fresh, numeric(), 1)) return("numeric")
                           if(vec_is(fresh, integer(), 1)) return("numeric")
                           if(is.vector(fresh) && !is_named(fresh)) return("unnamed_vector")
                           if(is.vector(fresh) && is_named(fresh)) return("named_vector")
                           return("unimplemented")
                         },

                         next_method = function(generic, role) {
                           fns <- ls(envir = private)
                           method <- paste0(generic, ".", role[1])
                           if(method %in% fns) {
                             private[[method]]
                           } else {
                             if(length(role)==1L) {
                               private[[paste0(generic, ".default")]]
                             } else {
                               private$next_method(generic, role[-1])
                             }
                           }
                         },


                         setup_data.default = function(fresh, name, class) {
                           type <- private$fresh_type(fresh)
                           levels <- switch(type,
                                            "numeric" = fct_attrs(levels = lvl_attrs(label_seq_length(fresh, prefix = name)),
                                                                  class = class),
                                            "unnamed_vector" = fct_attrs(levels = lvl_attrs(fresh),
                                                                         class = class),
                                            "named_vector" = fct_attrs(levels = lvl_attrs(names(fresh),
                                                                                          rep = unname(fresh)),
                                                                       class = class),
                                            "unimplemented" = abort(paste0("Not sure how to handle ", role(fresh)[1])))
                           private$setup_data.edbl_lvls(levels, name, class)
                         },

                         setup_data.edbl_lvls = function(fresh, name, class) {
                           fid <- private$fct_new_id(n = 1)
                           attrs <- attributes(fresh)

                           fattrs <- data.frame(id = fid, name = name, role = class)
                           self$append_fct_nodes(fattrs)

                           lattrs <- lvl_data(fresh)
                           lattrs$id <- private$lvl_new_id(length(fresh))

                           self$append_lvl_nodes(lattrs, fid)
                         },

                         setup_data.formula = function(fresh, name, role) {
                           flevels <- self$fct_levels()
                           tt <- terms(fresh)
                           vars <- rownames(attr(tt, "factor"))

                           private$setup_data.cross_lvls(vars, name, role)
                         },

                         setup_data.edbl_fct = function(fresh, name, role) {
                           fid <- private$fct_new_id
                           self$append_fct_nodes(tibble(id = fid, name = name, role = role))

                           lvls <- levels(fresh)
                           lattrs <- tibble(id = private$lvl_new_id(length(lvls)),
                                            value = lvls)

                           self$append_lvl_nodes(lattrs, fid)
                         },

                         setup_data.cross_lvls = function(fresh, name, role) {
                           flevels <- self$fct_levels()
                           vars <- fresh

                           pdf <- expand.grid(flevels[vars])
                           pdf[[name]] <- fct_attrs(levels = lvl_attrs(1:nrow(pdf), prefix = name),
                                                    role = role)
                           private$setup_data.edbl_lvls(pdf[[name]], name, role)
                           fnodes <- self$fct_nodes
                           idv <- fnodes[fnodes$name == name, "id"]
                           for(avar in vars) {
                             idp <- fnodes[fnodes$name == avar, "id"]
                             self$append_fct_edges(data.frame(from = idp, to = idv, type = "nest"))
                             self$append_lvl_edges(data.frame(from = self$lvl_id(pdf[[avar]]),
                                                              to = self$lvl_id(pdf[[name]])))
                           }
                           idvs <- fnodes[fnodes$name %in% vars, "id"]
                           cross_df <- expand.grid(from = idvs, to = idvs)
                           cross_df <- subset(cross_df, from!=to)
                           cross_df$type <- "cross"
                           self$append_fct_edges(cross_df)
                         },

                         setup_data.nest_lvls = function(fresh, name, role) {
                           idv <- private$fct_new_id
                           idl <- private$lvl_new_id
                           parent <- fresh %@% "keyname"
                           cross_parents <- fresh %@% "parents"
                           clabels <- fresh %@% "labels"
                           idp <- self$fct_id_by_name(c(parent, colnames(cross_parents[[1]])))
                           attrs <- attributes(fresh)
                           fattrs <- do.call(data.frame, c(attrs[setdiff(names(attrs), c("names", "keyname", "role", "parents", "labels"))],
                                                           list(stringsAsFactors = FALSE,
                                                                id = idv,
                                                                name = name,
                                                                role = role)))
                           self$append_fct_nodes(fattrs)
                           self$append_fct_edges(data.frame(from = idp, to = idv, type = "nest"))
                           plevels <- rep(names(fresh), lengths(fresh))
                           clevels <- unname(unlist(fresh))
                           self$append_lvl_nodes(data.frame(idvar = idv,
                                                            id = idl:(idl + sum(lengths(fresh)) - 1),
                                                            name = clevels,
                                                            var = name,
                                                            label = unname(unlist(clabels)),
                                                            stringsAsFactors = FALSE))
                           pids <- self$lvl_id(plevels)
                           vids <- self$lvl_id(clevels)
                           self$append_lvl_edges(data.frame(from = pids, to = vids))
                           if(!is_null(cross_parents)) {
                             cross_df <- do.call("rbind", cross_parents[names(fresh)])
                             cross_parent_names <- colnames(cross_df)
                             for(across in cross_parent_names) {
                               cpids <- self$lvl_id(cross_df[[across]])
                               self$append_lvl_edges(data.frame(from = cpids, to = vids))
                             }
                           }
                         }


                       ))
