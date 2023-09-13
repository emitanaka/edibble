
# FIXME

#' Pivot factor to a wider list
#'
#' This function makes it easier to see which units or records are associated
#' with a particular level of factor.
#' The arguments closely follow [tidyr::pivot_wider()], but the major difference
#' is that it is aware of the relationships between columns and makes use of
#' this to present to you information without unnecessary duplication.
#'
#' @param data An edibble table
#' @param id_cols Columns in the data where the levels correspond to a unique
#'   level on the row.
#' @param id_expand If multiple columns are selected, whether all combinations of
#'   the levels should be taken into account.
#' @param names_from An expression specifying which columns to pivot from.
#' @param names_prefix A prefix to add to the resulting column names.
#' @param names_sep A separator to use between column names in the resulting wide format.
#' @param names_glue A glue specification to control column names.
#' @param names_sort A logical indicating whether to sort the resulting column names.
#' @param names_vary A character vector of options to specify how to handle varying identifiers.
#' @param names_expand A logical indicating whether to expand identifiers in the column names.
#' @param names_repair A method to handle non-unique resulting column names.
#' @param values_from A character vector specifying columns to use as values in the wide format.
#' @param values_fill A value or function to fill missing values.
#' @param values_fn A function to aggregate values.
#' @seealso [split_by()] and [count_by()]
pivot_wider_by <- function(data,
                           id_cols = NULL,
                           id_expand = FALSE,
                           names_from = where(is_trt),
                           names_prefix = "",
                           names_sep = ":",
                           names_glue = NULL,
                           names_sort = FALSE,
                           names_vary = "fastest",
                           names_expand = FALSE,
                           names_repair = "check_unique",
                           values_from = NULL,
                           values_fill = NULL,
                           values_fn = NULL) {
  not_edibble(data)

  prov <- activate_provenance(data)
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)



}


#' Split or count the data according to certain factors
#'
#' This function has a similar result with `split()` where
#' it returns a named list with names corresponding to the
#' levels of the separating factor (or concatenated strings
#' if multiple separating factors). The key differences to `split()`,
#' are that the splitting factor does not appear in the elements of the
#' list and only linked factors and their ancestors appear in the output, e.g.
#' if treatment is applied to wholeplot and subplots are nested within
#' subplots, then the subplot will not be shown in the output if split by
#' treatment.
#'
#' @param .data An edibble table.
#' @param ... The factors to split or count by. You cannot split by a record
#'   factor or a factor that uniquely indexes the smallest unit in the
#'   design. You cannot also combine treatment and unit factors together.
#' @param .sep The separator to use if more than one factor to split by.
#' @examples
#' spd <- takeout(menu_split())
#' spd %>% split(trt1)
#' spd %>% split_by(trt1)
#' spd %>% split_by(trt2)
#' spd %>% split_by(mainplot)
#'
#' @return A named list.
#' @seealso [pivot_wider_by()]
#' @export
split_by <- function(.data, ..., .sep = ":") {
  not_edibble(.data)

  loc <- eval_select(expr(c(...)), .data)
  if(length(loc) == 0) abort("At least one factor to split by must be defined.")
  prov <- activate_provenance(.data)
  fnames <- names(loc)
  fid <- prov$fct_id(name = fnames)
  role <- prov$fct_role(id = fid)
  tid <- fid[role == "edbl_trt"]
  rid <- fid[role == "edbl_rcrd"]
  uid <- fid[role == "edbl_unit"]

  if(length(rid)) abort("Splitting by record factors is not supported at the moment.")
  if(length(tid) & length(uid)) abort("Splitting by both unit and treatment factor is not allowed. Please choose just unit factors alone or treatment factors alone.")

  if(length(tid)) {
    uids <- prov$fct_id_child(id = tid, role = "edbl_unit")
    data <- tibble::as_tibble(prov$serve_units(id = uids, return = "value"))
    fsplit <- prov$serve_trts(id = tid, return = "value")
  } else if(length(uid)) {
    lids <- prov$fct_id_leaves(role = "edbl_unit")
    if(any(uid %in% lids)) abort("The splitting factor cannot contain the smallest unit factor.")
    uids <- prov$fct_id_child(id = uid, role = "edbl_unit")
    tids <- prov$fct_id_parent(id = uids, role = "edbl_trt")
    data <- tibble::as_tibble(c(prov$serve_units(id = uids, return = "value"),
                                prov$serve_trts(id = tids, return = "value")))
    fsplit <- data[fnames]
    data <- data[setdiff(names(data), fnames)]
  }

  out <- split(data, fsplit, sep = .sep)
  attr(out, "by") <- fnames
  class(out) <- c("split_by", class(out))
  out
}

print.split_by <- function(x, ...) {
  attr(x, "by") <- NULL
  class(x) <- setdiff(class(x), "split_by")
  NextMethod()
}

#' @rdname split_by
#' @export
count_by <- function(.data, ...) {
  out <- split_by(.data, ...)
  by <- attr(out, "by")
  paste_by <- paste(by, collapse = ":")
  n <- function(.x) length(unique(.x))
  out2 <- as.data.frame(do.call(rbind, lapply(out, function(df) lapply(df, n))))
  out2[[paste_by]] <- rownames(out2)
  rownames(out2) <- NULL
  out2[c(paste_by, setdiff(names(out2), paste_by))]
}

#' Pivot treatments to a wider list or table format
#'
#' @param .data An edibble table.
#' @param trts A vector of treatment (tidyselect compatible). By default it is
#'   NULL and includes all the treatments.
#' @param fcts A vector of factors in the edibble table.
#' @param drop Whether the resulting list should drop to a vector within each list
#'   element if there is only one column. Default is FALSE.
#' @examples
#' pivot_trts_widelist(takeout(menu_crd(t = 5, n = 20)))
#' @return A named list where elements are the data and the names are treatments.
#' @keywords internal
#' @export
pivot_trts_widelist <- function(.data, trts = NULL, fcts = NULL, drop = FALSE) {
  lifecycle::deprecate_warn("1.1.0", "pivot_trts_widelist()", "pivot_trts()")
  not_edibble(.data)
  data <- as.data.frame(.data, levels_as = "character")
  prov <- activate_provenance(.data)
  if(is.null(trts)) {
    trt_names <- prov$trt_names()
  } else {
    tloc <- eval_select(enexpr(trts), .data)
    trt_names <- names(tloc)
  }
  if(is.null(fcts)) {
    fct_names <- setdiff(names(data), trt_names)
  } else {
    floc <- eval_select(enexpr(fcts), .data)
    fct_names <- names(floc)
  }
  if(drop & length(fct_names)==1) {
    split(data[[fct_names]], data[trt_names])
  } else {
    split(data[fct_names], data[trt_names])
  }
}

#' @rdname pivot_trts_widelist
#' @export
pivot_trts_widetable <- function(.data, trts = NULL, fcts = NULL) {
  lifecycle::deprecate_warn("1.1.0", "pivot_trts_widetable()", "pivot_trts()")
  out <- suppressWarnings(pivot_trts_widelist(.data, trts, fcts))
  ll <- sapply(out, nrow)
  ml <- max(ll)
  for(i in which(ll < ml)) {
    append <- as.data.frame(matrix(NA, ml - ll[i], ncol(out[[i]])))
    colnames(append) <- colnames(out[[i]])
    out[[i]] <- rbind(out[[i]], append)
  }
  res <- as.data.frame(out)
  colnames(res) <- rep(names(out), each = ncol(out[[1]]))
  rownames(res) <- 1:nrow(res)
  res
}
