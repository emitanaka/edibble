
make_sheet_names <- function(.design = NULL) {
  if(is_null(.design)) {
    data_sheet_names <- "Data"
  } else {
    if(has_record(.design)) {
      rcrds <- rcrd_to_unit_dict(.design$graph)
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
       variables = list(names = createStyle(fontSize = 14,
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

write_data_sheet <- function(wb, sheet_names, cell_styles, .design, .data) {
  if(nrow(.data) && ncol(.data)) {
    if(length(sheet_names) > 1) {
      rcrds <- rcrd_to_unit_dict(.design$graph)
      units <- unique(unname(rcrds))
      vgraph <- subset_vars(.design$graph)

      for(aunit in units) {
        vertex_delete <- neighbors(vgraph, var_index(vgraph, aunit), mode = "out")
        if(length(vertex_delete) == 0) {
          data <- cbind(.rowNumber = 1:nrow(.data), as_data_frame(.data))
        } else {
          units_to_delete <- var_names(vgraph, vertex_delete)
          subdesign <- .design$clone()
          subdesign$delete_variable(units_to_delete)
          vars_degree <- degree(subset_vars(subdesign$graph))
          others_to_delete <- names(vars_degree)[vars_degree==0]
          subdesign$delete_variable(others_to_delete)
          res <- subdesign$table
          data <- cbind(.rowNumber = 1:nrow(res),
                        as_data_frame(res))
        }
        writeData(wb, sheet = data_sheet_name(aunit),
                  x = data, startCol = 1,
                  headerStyle = cell_styles$header,
                  name = data_sheet_name(aunit))
      }
    } else {
      data <- cbind(.rowNumber = 1:nrow(.data), as_data_frame(.data))
      writeData(wb, sheet = sheet_names, x = data, startCol = 1,
                headerStyle = cell_styles$header,
                name = "data")
      addStyle(wb, sheet = sheet_names, rows = 2:(nrow(data) + 1),
               cols = 1:ncol(data), gridExpand = TRUE, stack = TRUE,
               style = cell_styles$body)
    }
  }

}


write_variables_sheet <- function(wb, sheet_name, cell_styles, .design) {
  #if(!is_null(.design$validation)) {
    # TO DO
   # dataValidation(wb, sheet = sheet_name, ...)
  #}

}


#' Export the design to xlsx
#'
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
#' @importFrom openxlsx addStyle createNamedRegion createStyle createWorkbook addWorksheet writeData dataValidation saveWorkbook
#' @export
export_design <- function(.data, file, author, date = Sys.Date(), overwrite = FALSE) {
  if(is_edibble(.data)) {
    .design <- attr(.data, "design")
  } else if(is_edibble_design(.data)) {
    .design <- .data
    .data <- .design$table
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
                        cell_styles_list$variables, .design)

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


as_data_frame <- function(.data) {
  rcrd_names <- names(.data)[map_lgl(.data, function(x) "edbl_rcrd" %in% class(x))]
  .data[rcrd_names] <- ""
  structure(lapply(.data, as.character),
            names = names(.data),
            class = "data.frame",
            row.names = 1:nrow(.data))
}
