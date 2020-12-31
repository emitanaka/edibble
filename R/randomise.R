find_unit_parents <- function(.nexus, vname) {
  unexus <- subset(.nexus, class=="edbl_unit", .vtype = "var")
  unit_vertex <- which(V(unexus)$name==vname)
  # get direct parents
  parent_vertices <- igraph::neighbors(unexus, unit_vertex, mode = "in")
  if(length(parent_vertices) > 0) {
    var_names(unexus, parent_vertices)
  } else {
    NULL
  }
}

find_unit_ancestors <- function(.nexus, vname) {
  aname <- vname
  ancestors <- NULL
  repeat({
    aname <- find_unit_parents(.nexus, aname)
    if(is_null(aname)) break
    ancestors <- c(ancestors, aname)
  })
  ancestors
}

endpoints <- function(.nexus, etype, vtype) {
  snexus <- switch(vtype,
                   "var" = subset_vars(.nexus),
                   "level" = subset_levels(.nexus))
  ind <- which(E(snexus)$etype == etype)
  es <- E(snexus)[ind]
  ends <- igraph::ends(snexus, es)
  from <- ends[, 1]
  to <- ends[, 2]
  data.frame(from = from, to = to, es = as.numeric(es), stringsAsFactors = FALSE)
}


#' Randomise treatments
#'
#' @param .nexus AN edibble nexus object.
#' @seealso Set the treatments by [set_trts()], set units by [set_units()],
#' and specify which treatment factors are applied to which units by using
#' [apply_trts()].
#' @export
randomise_trts <- function(.nexus, ...) {
  UseMethod("randomise_trts")
}

#' Get the variable names given the vertex name of edibble nexus
#' @param .nexus An edibble nexus.
#' @param names A vector of character with vertex names.
#' @return A character vector of the variable name.
#' @export
names_to_vnames <- function(.nexus, names) {
  dict <- setNames(V(.nexus)$vname, V(.nexus)$name)
  unname(dict[names])
}

names_to_lnames <- function(.nexus, names) {
  vnames <- names_to_vnames(.nexus, names)
  map_chr(seq_along(names),
          function(i) gsub(paste0("^", vnames[i], ":"), "", names[i]))
}

names_to_nesting_names <- function(.nexus, names) {
  dict <- setNames(V(.nexus)$label2, V(.nexus)$name)
  res <- unname(dict[names])
  if(any(is.na(res))) return(names_to_lnames(.nexus, names))
  res
}

#' Get the treatment variables for every unit
#'
#' @return A named list where names are the unit variable name and
#'   element is the character vector with the treatment factors applied
unit_to_trts <- function(.nexus, unit, trts, type = c("var", "level")) {
  type <- match.arg(type)
  switch(type,
         var = {
           tv <- endpoints(.nexus, "t2v", "var")
           split(tv$from, tv$to)
         },
         level = {
           snexus <- subset(.nexus, vname %in% c(unit, trts), .vtype = "level")
           tvl <- endpoints(snexus, "t2vmay", "level")
           split(tvl$from, tvl$to)
         })
}

#' @export
rep_rpbl <- function(.data, times, length.out) {
  res <- .data
  if(times > 1) {
    for(i in seq(times - 1)) {
      res <- rbind(res, .data)
    }
  }
  res[1:length.out, ]
}


#' @export
randomise_trts.edbl_nexus <- function(.nexus) {
  unit_to_trts_list <- unit_to_trts(.nexus)
  units_with_trts_applied <- names(unit_to_trts_list)
  out <- .nexus
  for(i in seq_along(unit_to_trts_list)) {
    unit <- units_with_trts_applied[i]
    trts <- unit_to_trts_list[[i]]
    ancestors <- find_unit_ancestors(.nexus, unit)
    units <- c(unit, ancestors)
    snexus <- subset(.nexus, vname %in% units)
    # need to fix below
    reps <- replicabble(.nexus, unit, trts)

    units_ls <- lapply(serve_units(snexus), as.character)
    units_df <- as.data.frame(lapply(seq_along(units_ls), function(i) {
      vnames <- vertex_level_names(names(units_ls)[i], as.character(units_ls[[i]]))
      as.factor(names_to_nesting_names(snexus, vnames))
    }))
    names(units_df) <- names(units_ls)

    nunit <- nrow(units_df)
    ntrt <- nrow(reps)
    # okay rep has different behaviours depending on called internally
    # in a package, vs terminal!!! Bloody hell!!!!!
    # <in package>
    # rep(cars[1:3,1:2, drop = FALSE], times =2)
    # $speed
    # [1] 4 4 7
    #
    # $dist
    # [1]  2 10  4
    #
    # $speed
    # [1] 4 4 7
    #
    # $dist
    # [1]  2 10  4
    # <terminal>
    # rep(cars[1:3,1:2, drop = FALSE], times = 2)
    # speed dist
    # 1     4    2
    # 2     4   10
    # 3     7    4
    # 4     4    2
    # 5     4   10
    # 6     7    4

    trts_df <- as.data.frame(rep_rpbl(reps[sample(nrow(reps)),trts, drop = FALSE],
                   times = ceiling(nunit / ntrt),
                   length.out = nrow(units_df)))


    nesting <- structure(lapply(units,
                                function(aunit) find_unit_parents(snexus, aunit)),
                         names = units)
    # remove NULL results
    nesting <- nesting[!map_lgl(nesting, is_null)]

    # unfortunately this doesn't work since I label nested factors uniquely
    if(length(nesting) > 0) {
      des <- dae::designRandomize(allocated = trts_df,
                                  recipient = units_df,
                                  nested.recipients = nesting)
    } else {
      des <- dae::designRandomize(allocated = trts_df,
                                  recipient = units_df)

    }

    # allocated <- character()
    # chosen_df <- cbind(rep_df[numeric(0),], "recipient" = character())
    # for(j in 1:nrow(rep_df)) {
    #   recipient <- sample(setdiff(rep_df$units[[j]], allocated), rep_df$rep[j])
    #   allocated <- c(allocated, recipient)
    #   chosen_df <- rbind(chosen_df, cbind(rep_df[j, ], "recipient" = recipient))
    # }
    # chosen_df[, trts]
    for(atrt in trts) {
      es <- match_edge_seq(.nexus, des[[atrt]], vertex_level_names(unit, units_ls[[unit]]))
      out <- igraph::add_edges(out, es, attr = edge_attr_opt("t2v"))
    }
  }

  out <- igraph::delete_edges(out, which(E(out)$etype == "t2vmay"))

  # setting up replicabble

  # if parent_units > 1, assumes there is no higher parents for now
  # if(length(parent_units)==0) {
  #   rep_floor <- floor(nexp / ntrt)
  #   rep_left <- nexp - rep_floor * ntrt
  #   reps <- rep(rep_floor, ntrt)
  #   if(rep_left > 0) {
  #     pos <- sample(ntrt, size = rep_left)
  #     reps[pos] <- reps[pos] + 1
  #   }
  #   rpbl <- replicabble(get_trt_levels(.nexus), .replicate = reps)
  # } else if(length(parent_units)==1) {
  #   ll <- list()
  #   # assumes single hierarchy
  #   while(length(parent_units) > 0) {
  #     ll <- c(ll, parent_units)
  #     parent_units <- find_unit_parents(.nexus, names(parent_units))
  #   }
  #   rep_floor <- floor(nexp / ntrt)
  #   rep_left <- nexp - rep_floor * ntrt
  #   # these extras need to be distributed evenly
  #   if(rep_left > 0) {
  #     extra_trts <- sample(ntrt, size = rep_left)
  #   }
  #
  #   # assumes enough replicate to cover at least 1 for each case
  #   nblocks <- prod(lengths(ll))
  #   rep_floor <- floor(nexp / (ntrt * nblocks))
  #   rep_left <- nexp - rep_floor * ntrt * nblocks
  #
  #   reps <- rep(rep_floor, ntrt * nblocks)
  #
  #   # assume rep_left = 0 for now
  #
  #   ll <- c(ll, get_trt_levels(.nexus))
  #   rpbl <- replicabble(ll, .replicate = reps)
  # } else if(length(parent_units) > 1) {
  #
  # }

  #out <- igraph::set_graph_attr(.nexus, "replicabble", rpbl)
  structure(out, class = class(.nexus))
}