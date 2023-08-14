
make_sheet_names <- function(prov = NULL) {
  if(is_null(prov)) {
    data_sheet_names <- "Data"
  } else {
    if(prov$rcrd_exists(abort = FALSE)) {
      rids <- prov$rcrd_ids
      # FIXME
      rcrds <- rcrd_to_unit_dict(prov, rids)
      units <- unique(unname(rcrds))
      if(length(units) == 1) {
        data_sheet_names <- "Data"
      } else {
        data_sheet_names <- data_sheet_name(units)
      }
    } else {
      data_sheet_names <- "Data"
    }
  }

  c("Context", data_sheet_names, "Variables")
}

make_cell_styles <- function() {
  list(context = list(title = openxlsx::createStyle(fontSize = 30,
                                            textDecoration = "bold"),
                        date = openxlsx::createStyle(fontSize = 25),
                        author = openxlsx::createStyle(fontSize = 25),
                        contact = openxlsx::createStyle(fontSize = 25),
                        context_name = openxlsx::createStyle(fontSize = 18, fontColour = "blue"),
                        context = openxlsx::createStyle(fontSize = 18),
                        instructions = openxlsx::createStyle(fontSize = 12)),
       data = list(header = openxlsx::createStyle(fgFill = "#DCE6F1",
                                          halign = "left",
                                          textDecoration = "bold",
                                          border = "Bottom"),
                     body = openxlsx::createStyle(fontSize = 12)),
       variables = list(header = openxlsx::createStyle(fgFill = "#DCE6F1",
                                             halign = "left",
                                             textDecoration = "bold",
                                             border = "Bottom"),
                        names = openxlsx::createStyle(fontSize = 14,
                                              textDecoration = "bold"),
                        type = openxlsx::createStyle(fontSize = 14,
                                           fontColour = "blue"),
                        what = openxlsx::createStyle(fontSize = 14),
                        validation = openxlsx::createStyle(fontSize = 14)))
}

add_creator <- function(wb, authors) {
  openxlsx::addCreator(wb, "Created with edibble using R")
  if(!missing(authors)) {
    for(author in authors) {
      openxlsx::addCreator(wb, author)
    }
  }
}

add_worksheets <- function(wb, sheet_names, title) {
  for(asheet in sheet_names) {
    openxlsx::addWorksheet(wb, asheet,
                 header = c(paste0("Created on ", Sys.Date()), title, "&[Page] / &[Pages]"),
                 footer = c("&[File]", "&[Tab]", "Printed on &[Date]"),
                 gridLines = ifelse(asheet==sheet_names[1], FALSE, TRUE))
  }
}



write_title_sheet <- function(wb, sheet_name, cell_styles, prov, author, date = Sys.Date()) {
  # title
  openxlsx::writeData(wb, sheet = sheet_name, x = prov$design$name,
            startRow = 1, startCol = 1, name = "title")
  openxlsx::addStyle(wb, sheet = sheet_name,
           style = cell_styles$title, 1, 1, stack = TRUE)

  # date
  openxlsx::writeData(wb, sheet = sheet_name, x = date,
            startRow = 2, startCol = 1, name = "date")
  openxlsx::addStyle(wb, sheet_name, cell_styles$date, 2, 1, stack = TRUE)

  # author
  if(!missing(author)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = author,
              startRow = 3, startCol = 1, name = "author")
    openxlsx::addStyle(wb, sheet_name, cell_styles$author, 3, 1, stack = TRUE)
  }

  # context
  ncontext <- length(prov$design$context)
  openxlsx::writeData(wb, sheet = sheet_name,
            x = unlist(prov$design$context),
            startRow = 5, startCol = 2)
  openxlsx::addStyle(wb, sheet_name, cell_styles$context, 5:(5 + ncontext), 2,
           stack = TRUE)

  openxlsx::writeData(wb, sheet = sheet_name,
            x = names(prov$design$context),
            startCol = 1, startRow = 5)
  openxlsx::addStyle(wb, sheet_name, cell_styles$context_name, 5:(5 + ncontext), 1,
           stack = TRUE)

  openxlsx::createNamedRegion(wb, sheet_name, cols = 1:2, rows = 5:(5 + ncontext),
                    name = "context")


}

data_sheet_name <- function(name) {
  paste0("Data.", name)
}

subset_design <- function(prov, unit, rcrds) {
  keep_rids <- prov$fct_id(rcrds)
  keep_uids <- prov$fct_id(unit)
  keep_uids_ancestors <- prov$fct_ancestor(keep_uids)
  sprov <- prov$clone()
  sprov$fct_nodes <- sprov$fct_nodes[sprov$fct_nodes$id %in% c(keep_uids_ancestors, keep_rids), ]
  sprov$fct_edges <- sprov$fct_edges[(sprov$fct_edges$to %in% keep_uids_ancestors &
                                        sprov$fct_edges$from %in% keep_uids_ancestors) |
                                        sprov$fct_edges$to %in% keep_rids, ]
  sprov$lvl_nodes <- sprov$lvl_nodes[sprov$lvl_nodes$idvar %in% keep_uids_ancestors, ]
  keep_lids_ancestors <- sprov$lvl_id()
  sprov$lvl_edges <- sprov$lvl_edges[sprov$lvl_edges$to %in% keep_lids_ancestors & sprov$lvl_edges$from %in% keep_lids_ancestors, ]
  if(!is_null(sprov$design$allotment$trts)) {
    units <- map_chr(sprov$design$allotment$trts, function(x) all.vars(f_rhs(x)))
    allotments <- sprov$design$allotment$trts[units %in% sprov$fct_names()]
    if(is_empty(allotments)) {
      sprov$design$allotment$trts <- NULL
    } else {
      sprov$design$allotment$trts <- allotments
    }
  }
  if(!is_null(sprov$design$validation)) {
    rcrds <- sprov$fct_names(keep_rids)
    if(!any(rcrds %in% names(sprov$design$validation))) {
      sprov$design$validation <- NULL
    } else {
      sprov$design$validation <- sprov$design$validation[rcrds]
    }
  }

  sprov$design
}

write_data_sheet <- function(wb, sheet_names, cell_styles, prov, .data) {
  if(nrow(.data) && ncol(.data)) {
    if(length(sheet_names) > 1) {
      rids <- prov$rcrd_ids
      rcrds2unit <- rcrd_to_unit_dict(prov, rids)
      units <- unique(unname(rcrds2unit))
      for(aunit in units) {
        rcrds <- names(rcrds2unit)[rcrds2unit==aunit]
        des <- subset_design(prov, aunit, rcrds)
        data <- as_data_frame(serve_table(des))
        openxlsx::writeData(wb, sheet = data_sheet_name(aunit),
                  x = data, startCol = 1,
                  headerStyle = cell_styles$header,
                  name = data_sheet_name(aunit))
        openxlsx::addStyle(wb, sheet = data_sheet_name(aunit),
                 rows = 2:(nrow(data) + 1),
                 cols = 1:ncol(data), gridExpand = TRUE, stack = TRUE,
                 style = cell_styles$body)
      }
    } else {
      data <- as_data_frame(.data)
      openxlsx::writeData(wb, sheet = sheet_names, x = data, startCol = 1,
                headerStyle = cell_styles$header,
                name = "Data")
      openxlsx::addStyle(wb, sheet = sheet_names, rows = 2:(nrow(data) + 1),
               cols = 1:ncol(data), gridExpand = TRUE, stack = TRUE,
               style = cell_styles$body)
    }
  }

}


write_variables_sheet <- function(wb, sheet_name, cell_styles, prov, .data) {

  type <- map_chr(.data, function(var) {
      cls <- class(var)
      if("edbl_unit" %in% cls) return("unit")
      if("edbl_trt" %in% cls) return("trt")
      if("edbl_rcrd" %in% cls) return("rcrd")
      "var"
    })
  data <- data.frame(variable = names(.data),
                    type = unname(type),
                    stringsAsFactors = FALSE)
  if(!is_null(prov$design$validation)) {
    data$record <- ""
    data$value <- ""
    valid <- prov$design$validation
    valid_names <- names(valid)
    rids <- prov$rcrd_ids
    rcrds <- rcrd_to_unit_dict(prov, rids)
    n_ounits <- length(unique(rcrds))
    for(i in seq_along(valid)) {
      unit <- rcrds[valid_names[i]]
      data_sheet <- ifelse(n_ounits > 1,
                           data_sheet_name(unit),
                           "Data")
      dat <- openxlsx::read.xlsx(wb, namedRegion = data_sheet)
      j <- which(data$variable == valid_names[i])
      data$record[j] <- valid[[i]]$record
      if(valid[[i]]$type != "list") {
        data$value[j] <- restriction_for_human(valid[[i]]$operator, valid[[i]]$value)
        openxlsx::dataValidation(wb, sheet = data_sheet,
                       rows = 1:nrow(dat) + 1,
                       cols = j,
                       type = valid[[i]]$type,
                       operator = valid[[i]]$operator,
                       value = valid[[i]]$value)
      } else {
        k <- which(names(data) == "value")
        values <- valid[[i]]$values
        data$value[j] <- values[1]
        L <- LETTERS[c(k, k + length(values) - 1)]
        openxlsx::writeData(wb, sheet = sheet_name, x = data.frame(t(values), stringsAsFactors = FALSE),
                  startCol = k,
                  startRow = j + 1, colNames = FALSE)
        openxlsx::dataValidation(wb, sheet = data_sheet,
                       rows = 1:nrow(dat) + 1,
                       cols = j,
                       type = "list", operator = NULL,
                       value = paste0("'", sheet_name, "'!$",
                                      L[1], "$", j + 1, ":$", L[2], "$", j + 1))
      }

    }
  }
  openxlsx::writeData(wb, sheet = sheet_name, x = data, startCol = 1,
            headerStyle = cell_styles$header,
            name = "Variables")

}

restriction_for_human <- function(operator, value) {
  switch(operator,
         equal = paste0("= ", value),
         greaterThanOrEqual = paste0(">= ", value),
         greaterThan = paste0("> ", value),
         lessThanOrEqual = paste0("<= ", value),
         lessThan = paste0("< ", value),
         notEqual = paste0("not equal to ", value),
         between = paste0("between ", value[1], " and ", value[2], " inclusive"),
         notBetween = paste0("< ", value[1], " and > ", value[2]),
         "")
}


#' Export the design to xlsx
#'
#' @description
#' This function is designed to export the design made using edibble to an
#' external xlsx file.
#'
#' @param .data An edibble data frame or design.
#' @param file File, including the path, to export the data to.
#' @param author Name of the author in character. A vector of character is supported
#'  for where there are multiple authors.
#' @param date The date to be inserted in header.
#' @param overwrite A logical indicating whether to overwrite exisitng file or not.
#'
#' @importFrom cli cli_alert_success
#' @family user-facing functions
#' @return The input data object.
#' @export
export_design <- function(.data, file, author, date = Sys.Date(), overwrite = FALSE) {
  if(!requireNamespace("openxlsx")) {
    stop("Please install the `openxlsx` package to use this function.")
  }

  if(is_edibble_table(.data)) {
    .design <- attr(.data, "design")
  } else {
    abort("The input is not an edibble table.")
  }
  prov <- activate_provenance(.design)

  title <- .design$name
  sheet_names <- make_sheet_names(prov)
  cell_styles_list <- make_cell_styles()

  wb <- openxlsx::createWorkbook()
  add_worksheets(wb, sheet_names, title)
  add_creator(wb, author)

  write_title_sheet(wb, sheet_names[1],
                    cell_styles_list$context, prov, author, date)
  write_data_sheet(wb, sheet_names[-c(1, length(sheet_names))],
                     cell_styles_list$data, prov, .data)
  write_variables_sheet(wb, sheet_names[length(sheet_names)],
                        cell_styles_list$variables, prov, .data)

  save_workbook(wb, file, overwrite, prov)
  invisible(.data)
}

save_workbook <- function(wb, file, overwrite, prov) {
  success <- openxlsx::saveWorkbook(wb, file, overwrite = overwrite, returnValue = TRUE)
  if(success) {
    cli::cli_alert_success("{.emph {prov$design$name}} has been written to {.file {file}}")
  } else {
    cli::cli_alert_warning("Something went wrong. {.emph {prov$design$name}} failed to be exported.")
  }
}

#' Convert an edibble data frame to normal data frame
#'
#' A patch function where there is an issue with edbl factors
#'
#' @param .data can be a list or data frame
#' @return A data.frame.
#' @export
as_data_frame <- function(.data) {
  rcrd_names <- names(.data)[map_lgl(.data, function(x) "edbl_rcrd" %in% class(x))]
  .data[rcrd_names] <- lapply(.data[rcrd_names], unclass)
  structure(lapply(.data, function(x) {
    if(inherits(x, "edbl_unit")) return(as.character(x))
    if(inherits(x, "edbl_trt")) return(as.character(x))
    if(inherits(x, "edbl_rcrd")) return(as.numeric(x))
    return(x) }),
            names = names(.data),
            class = "data.frame",
            row.names = 1:vec_size_common(!!!.data))
}
