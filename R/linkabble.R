#' Make a link table
#' @param ... trait variables
#' @param .allocate the number of units to allocate for every level.
#' It may be a vector but the length needs to match the order and
#' the number of combined levels of given trait variables.
#' @importFrom tibble new_tibble
#' @seealso set_link
linkabble <- function(flist, .allocate = 1, .type = "nested") {
  x <- expand.grid(flist)
  x$allocate <- .allocate
  new_tibble(x, nrow = NROW(x), class = "lkbl", type = .type)
}

#' Set the number of replicate in linkabble
set_link <- function(.data, cond, .allocate) {
  cond <- enquo(cond)
  ind <- eval_tidy(quo_get_expr(cond), data = .data)
  .data$allocate[ind] <- .allocate
  .data
}

#' @importFrom tibble type_sum
type_sum.lkbl <- function(x) {
  "linkabble"
}

#' @importFrom tibble tbl_sum
tbl_sum.lkbl <- function(x) {
  head_meta <- c("A linkabble" = dim_desc(x))
  head_meta
}

is_linkabble <- function(x, type = "nested") {
  inherits(x, "lkbl") & attr(x, "type")==type
}
