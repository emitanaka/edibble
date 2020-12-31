#' Make a replicabble
#'
#' @param flist list of treatment variables with their levels
#' @param .replicate the number of units to allocate for every level.
#' It may be a vector but the length needs to match the order and
#' the number of combined levels of given trait variables.
#' @importFrom tibble new_tibble
#' @export
replicabble <- function(.nexus, unit, trts) {
  t2vsplit <- unit_to_trts(.nexus, unit, trts, type = "level")
  df <- trts_levels_df(.nexus, trts)
  df$units <- lapply(1:nrow(df),
                         function(i) {
                           ind <- map_lgl(t2vsplit,
                                          function(possible_trts)
                                            all(as.character(df[i,]) %in% possible_trts))
                           names(ind)[ind]
                         })
  df$nunits_max <- lengths(df$units)
  nunits <- length(t2vsplit)
  ntrts <- nrow(df)
  min_nrep <- floor(nunits/ntrts)
  df$rep <- rep(min_nrep, ntrts)
  nremain <- nunits - min_nrep * ntrts
  df$rep <- df$rep + sample(rep(c(1, 0), c(nremain, ntrts - nremain)))

  new_tibble(df, nrow = NROW(df), class = "rpbl_df")
}


#' @importFrom tibble type_sum
#' @export
type_sum.rpbl_df <- function(x) {
  "replicabble"
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.rpbl_df <- function(x) {
  head_meta <- c("A replicabble" = dim_desc(x))
  head_meta
}

