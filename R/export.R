
make_sheet_names <- function(.design = NULL) {
  if(is_null(.design)) {
    data_sheet_names <- "Data"
  } else {
    if(has_record(.design)) {
      rids <- subset(.design$vgraph$nodes, class=="edbl_rcrd")$id
      rcrds <- rcrd_to_unit_dict(.design, rids)
      units <- unique(unname(rcrds))
      if(length(units) == 1) {
        data_sheet_names <- "Data"
      } else {
        data_sheet_names <- data_sheet_name(units)
      }
    } else {
      data_sheet_nmes <- "Data"
    }
  }

  c("Context", data_sheet_names, "Variables")
}

make_cell_styles <- function() {
  list(context = list(title = createStyle(fontSize = 30,
                                            textDecoration = "bold"),
                        date = createStyle(fontSize = 25),
                        author = createStyle(fontSize = 25),
                        contact = createStyle(fontSize = 25),
                        context_name = createStyle(fontSize = 18, fontColour = "blue"),
                        context = createStyle(fontSize = 18),
                        instructions = createStyle(fontSize = 12)),
       data = list(header = createStyle(fgFill = "#DCE6F1",
                                          halign = "left",
                                          textDecoration = "bold",
                                          border = "Bottom"),
                     body = createStyle(fontSize = 12)),
       variables = list(header = createStyle(fgFill = "#DCE6F1",
                                             halign = "left",
                                             textDecoration = "bold",
                                             border = "Bottom"),
                        names = createStyle(fontSize = 14,
                                              textDecoration = "bold"),
                        type = createStyle(fontSize = 14,
                                           fontColour = "blue"),
                        what = createStyle(fontSize = 14),
                        validation = createStyle(fontSize = 14)))
}

add_creator <- function(wb, authors) {
  addCreator(wb, "Created with edibble using R")
  if(!missing(authors)) {
    for(author in authors) {
      addCreator(wb, author)
    }
  }
}

add_worksheets <- function(wb, sheet_names, title) {
  for(asheet in sheet_names) {
    addWorksheet(wb, asheet,
                 header = c(paste0("Created on ", Sys.Date()), title, "&[Page] / &[Pages]"),
                 footer = c("&[File]", "&[Tab]", "Printed on &[Date]"),
                 gridLines = ifelse(asheet==sheet_names[1], FALSE, TRUE))
  }
}



write_title_sheet <- function(wb, sheet_name, cell_styles, .design, author, date = Sys.Date()) {
  # title
  writeData(wb, sheet = sheet_name, x = .design$name,
            startRow = 1, startCol = 1, name = "title")
  addStyle(wb, sheet = sheet_name,
           style = cell_styles$title, 1, 1, stack = TRUE)

  # date
  writeData(wb, sheet = sheet_name, x = date,
            startRow = 2, startCol = 1, name = "date")
  addStyle(wb, sheet_name, cell_styles$date, 2, 1, stack = TRUE)

  # author
  if(!missing(author)) {
    writeData(wb, sheet = sheet_name, x = author,
              startRow = 3, startCol = 1, name = "author")
    addStyle(wb, sheet_name, cell_styles$author, 3, 1, stack = TRUE)
  }

  # context
  ncontext <- length(.design$context)
  writeData(wb, sheet = sheet_name,
            x = unlist(.design$context),
            startRow = 5, startCol = 2)
  addStyle(wb, sheet_name, cell_styles$context, 5:(5 + ncontext), 2,
           stack = TRUE)

  writeData(wb, sheet = sheet_name,
            x = names(.design$context),
            startCol = 1, startRow = 5)
  addStyle(wb, sheet_name, cell_styles$context_name, 5:(5 + ncontext), 1,
           stack = TRUE)

  createNamedRegion(wb, sheet_name, cols = 1:2, rows = 5:(5 + ncontext),
                    name = "context")


}

data_sheet_name <- function(name) {
  paste0("Data.", name)
}

subset.edbl_design <- function(.edibble, unit, rcrds) {
  keep_rids <- vid(.edibble$vgraph, rcrds)
  keep_uids <- vid(.edibble$vgraph, unit)
  keep_uids_ancestors <- vancestor(.edibble$vgraph, keep_uids)
  .edibble$vgraph$nodes <- subset(.edibble$vgraph$nodes, id %in% c(keep_uids_ancestors, keep_rids))
  .edibble$vgraph$edges <- subset(.edibble$vgraph$edges, (to %in% keep_uids_ancestors &
                                                            from %in% keep_uids_ancestors) |
                                    to %in% keep_rids)
  .edibble$lgraph$nodes <- subset(.edibble$lgraph$nodes, idvar %in% keep_uids_ancestors)
  keep_lids_ancestors <- .edibble$lgraph$nodes$id
  .edibble$lgraph$edges <- subset(.edibble$lgraph$edges, to %in% keep_lids_ancestors & from %in% keep_lids_ancestors)
  if(!is_null(.edibble$allotment)) {
    units <- map_chr(.edibble$allotment, function(x) all.vars(f_rhs(x)))
    allotments <- .edibble$allotment[units %in% .edibble$vgraph$nodes$label]
    if(is_empty(allotments)) {
      .edibble$allotment <- NULL
    } else {
      .edibble$allotment <- allotments
    }
  }
  if(!is_null(.edibble$validation)) {
    rcrds <- vlabel(.edibble$vgraph, keep_rids)
    if(!any(rcrds %in% names(.edibble$validation))) {
      .edibble$validation <- NULL
    } else {
      .edibble$validation <- .edibble$validation[rcrds]
    }
  }

  .edibble
}

write_data_sheet <- function(wb, sheet_names, cell_styles, .design, .data) {
  if(nrow(.data) && ncol(.data)) {
    if(length(sheet_names) > 1) {
      rids <- subset(.design$vgraph$nodes, class=="edbl_rcrd")$id
      rcrds2unit <- rcrd_to_unit_dict(.design, rids)
      units <- unique(unname(rcrds2unit))
      for(aunit in units) {
        rcrds <- names(rcrds2unit)[rcrds2unit==aunit]
        des <- subset(.design, aunit, rcrds)
        data <- as_data_frame(serve_table(des))
        writeData(wb, sheet = data_sheet_name(aunit),
                  x = data, startCol = 1,
                  headerStyle = cell_styles$header,
                  name = data_sheet_name(aunit))
        addStyle(wb, sheet = data_sheet_name(aunit),
                 rows = 2:(nrow(data) + 1),
                 cols = 1:ncol(data), gridExpand = TRUE, stack = TRUE,
                 style = cell_styles$body)
      }
    } else {
      data <- as_data_frame(.data)
      writeData(wb, sheet = sheet_names, x = data, startCol = 1,
                headerStyle = cell_styles$header,
                name = "Data")
      addStyle(wb, sheet = sheet_names, rows = 2:(nrow(data) + 1),
               cols = 1:ncol(data), gridExpand = TRUE, stack = TRUE,
               style = cell_styles$body)
    }
  }

}


write_variables_sheet <- function(wb, sheet_name, cell_styles, .design, .data) {

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
  if(!is_null(.design$validation)) {
    data$record <- ""
    data$value <- ""
    valid <- .design$validation
    valid_names <- names(valid)
    rids <- subset(.design$vgraph$nodes, class=="edbl_rcrd")$id
    rcrds <- rcrd_to_unit_dict(.design, rids)
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
        dataValidation(wb, sheet = data_sheet,
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
        writeData(wb, sheet = sheet_name, x = data.frame(t(values), stringsAsFactors = FALSE),
                  startCol = k,
                  startRow = j + 1, colNames = FALSE)
        dataValidation(wb, sheet = data_sheet,
                       rows = 1:nrow(dat) + 1,
                       cols = j,
                       type = "list", operator = NULL,
                       value = paste0("'", sheet_name, "'!$",
                                      L[1], "$", j + 1, ":$", L[2], "$", j + 1))
      }

    }
  }
  writeData(wb, sheet = sheet_name, x = data, startCol = 1,
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
#' @importFrom openxlsx addCreator addStyle createNamedRegion createStyle createWorkbook addWorksheet writeData dataValidation saveWorkbook
#' @family user-facing functions
#' @export
export_design <- function(.data, file, author, date = Sys.Date(), overwrite = FALSE) {
  if(!require("openxlsx")) {
    stop("Please install the `openxlsx` package to use this function.")
  }

  if(is_edibble_table(.data)) {
    .design <- attr(.data, "design")
  } else {
    abort("The input is not an edibble table.")
  }

  title <- .design$name
  sheet_names <- make_sheet_names(.design)
  cell_styles_list <- make_cell_styles()

  wb <- createWorkbook()
  add_worksheets(wb, sheet_names, title)
  add_creator(wb, author)

  write_title_sheet(wb, sheet_names[1],
                    cell_styles_list$context, .design, author, date)
  write_data_sheet(wb, sheet_names[-c(1, length(sheet_names))],
                     cell_styles_list$data, .design, .data)
  write_variables_sheet(wb, sheet_names[length(sheet_names)],
                        cell_styles_list$variables, .design, .data)

  save_workbook(wb, file, overwrite, .design)
  invisible(.data)
}

save_workbook <- function(wb, file, overwrite, .design) {
  success <- saveWorkbook(wb, file, overwrite = overwrite, returnValue = TRUE)
  if(success) {
    cli_alert_success("{.emph {.design$name}} has been written to {.file {file}}")
  } else {
    cli_alert_warning("Something went wrong. {.emph {.design$name}} failed to be exported.")
  }
}

# .data can be a list or data frame
#' @export
as_data_frame <- function(.data) {
  rcrd_names <- names(.data)[map_lgl(.data, function(x) "edbl_rcrd" %in% class(x))]
  .data[rcrd_names] <- ""
  structure(lapply(.data, as.character),
            names = names(.data),
            class = "data.frame",
            row.names = 1:vec_size_common(!!!.data))
}
