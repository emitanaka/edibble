


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
export_design <- function(.data,
                          file,
                          author = NULL,
                          date = Sys.Date(),
                          theme = NULL,
                          overwrite = FALSE,
                          subject = NULL,
                          category = NULL) {

  if(!is_edibble_table(.data)) {
    abort("The input is not an edibble table.")
  }

  if(!requireNamespace("openxlsx2")) {
    abort("Please install the `openxlsx2` package to use this function.")
  }

  prov <- activate_provenance(.data)

  title <- prov$get_title()
  sheet_names <- make_sheet_names(prov)

  wb <- openxlsx2::wb_workbook(creator = author,
                               title = title,
                               subject = subject,
                               category = category,
                               datetime_created = Sys.time(),
                               theme = theme)

  # some large number to make it full size
  # there's no way to set it to 100% or alike
  # so this is a hack
  wb$set_bookview(window_height = 10000000,
                  window_width = 10000000)


  add_worksheets(wb, sheet_names, title)
  add_creator(wb, author)

  write_title_sheet(wb,
                    sheet_names[1],
                    title,
                    author,
                    date)


  # FIXME: producing some recovery error - idk why
  write_data_sheet(wb,
                   sheet_names[-c(1, length(sheet_names))],
                   prov,
                   .data)
  # FIXME: validation not implemented yet
  # write_variables_sheet(wb,
  #                       sheet_names[length(sheet_names)],
  #                       cell_styles_list$variables,
  #                       prov,
  #                       .data)

  save_workbook(wb, file, overwrite, title)

  invisible(.data)
}

data_sheet_name <- function(name) {
  paste0("Data.", name)
}

make_sheet_names <- function(prov) {
  if(!prov$rcrd_exists()) {
    # if no record is supplied, then use the smallest unit
    uname <- prov$fct_name(id = prov$fct_id_leaves(role = "edbl_unit"))
    data_sheet_names <- data_sheet_name(uname)
    names(data_sheet_names) <- uname
  } else {
    map_rcrd_to_unit <- prov$mapping("edbl_rcrd", "edbl_unit")
    unames <- prov$fct_names(id = unique(map_rcrd_to_unit))
    data_sheet_names <- data_sheet_name(unames)
    names(data_sheet_names) <- unames
  }
  c("Context", data_sheet_names, "Variables")
}


add_worksheets <- function(wb, sheet_names, title) {
  for(asheet in sheet_names) {
    wb$add_worksheet(asheet,
                     zoom = ifelse(asheet==sheet_names[1], 200, 100),
                     header = c(paste0("Created on ", Sys.Date()), title, "&[Page] / &[Pages]"),
                     footer = c("&[File]", "&[Tab]", "Printed on &[Date]"),
                     grid_lines = ifelse(asheet==sheet_names[1], FALSE, TRUE),
                     visible = ifelse(asheet=="Variables", "hidden", "visible"))
  }
}


save_workbook <- function(wb, file, overwrite, title) {
  tryCatch(wb$save(file = file, overwrite = overwrite),
           error = function(e) {
             cli::cli_alert_warning("Something went wrong. {.emph {title}} failed to be exported.")
           })
  cli::cli_alert_success("{.emph {title}} has been written to {.file {file}}")
}


write_title_sheet <- function(wb, sheet_name, title, author, date) {
  # title
  title_pos <- openxlsx2::wb_dims(from_row = 1, from_col = 1)
  wb$set_col_widths(sheet = sheet_name,
                    cols = 1,
                    widths = 100) # 250 is max
  wb$add_data(sheet = sheet_name,
              x = title,
              dims = title_pos,
              name = "title",
              col_names = FALSE)
  wb$add_font(sheet = sheet_name,
              dims = title_pos,
              bold = TRUE,
              size = 30)
  wb$add_cell_style(sheet = sheet_name,
                    dims = title_pos,
                    wrap_text = TRUE)

  # date
  date_pos <- openxlsx2::wb_dims(from_row = 2, from_col = 1)
  wb$add_data(sheet = sheet_name,
              x = date,
              dims = date_pos,
              name = "date",
              col_names = FALSE)
  wb$add_font(sheet = sheet_name,
              dims = date_pos,
              size = 25)
  wb$add_cell_style(sheet = sheet_name,
                    dims = date_pos,
                    horizontal = "left")

  # author
  if(!is_null(author)) {
    author_pos <- openxlsx2::wb_dims(from_row = 3, from_col = 1)
    wb$add_data(sheet = sheet_name,
                x = author,
                dims = author_pos,
                name = "author",
                col_names = FALSE)
    wb$add_font(sheet = sheet_name,
                dims = author_pos,
                size = 25)
  }

}



add_creator <- function(wb, authors) {
  wb$add_creators(paste0("Created with edibble (version ",
                         utils::packageVersion("edibble"),
                         ") using R"))
  if(!is_null(authors)) {
    for(author in authors) {
      wb$add_creators(author)
    }
  }
}



write_data_sheet <- function(wb, sheet_names, prov, data) {
  if(!prov$rcrd_exists()) {
    wb$add_data_table(sheet = sheet_names,
                      x = data,
                      table_name = sheet_names)
  } else {
    rids <- prov$rcrd_ids
    rcrds2unit <- prov$mapping("edbl_rcrd", "edbl_unit")
    uids <- unique(rcrds2unit)
    for(uid in uids) {
      uname <- prov$fct_names(id = uid)
      rids <- names(rcrds2unit)[rcrds2unit==uid]
      data <- as_tibble.edbl_table(prov$serve_units(id = uid, return = "value"))
      for(rid in rids) {
        data[[prov$fct_names(id = rid)]] <- NA
      }
      wb$add_data_table(sheet = sheet_names[uname],
                        x = data,
                        table_name = sheet_names[uname])
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

  # FIXME
  valids <- prov$get_validation("rcrds")
  if(!is_null(valids)) {
    data$record <- ""
    data$value <- ""
    valid_names <- names(valids)
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
        openxlsx::writeData(wb,
                            sheet = sheet_name,
                            x = data.frame(t(values), stringsAsFactors = FALSE),

                            startCol = k,
                            startRow = j + 1, colNames = FALSE)
        openxlsx::dataValidation(wb,
                                 sheet = data_sheet,
                                 rows = 1:nrow(dat) + 1,
                                 cols = j,
                                 type = "list",
                                 operator = NULL,
                                 value = paste0("'", sheet_name, "'!$",
                                                L[1], "$", j + 1, ":$", L[2], "$", j + 1))
      }

    }
  }
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = data,
                      startCol = 1,
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

#' Convert an edibble data frame to normal data frame
#'
#' A patch function where there is an issue with edbl factors
#'
#' @param .data can be a list or data frame
#' @return A data.frame.
#' @importFrom tibble as_tibble
#' @export
as_tibble.edbl_table <- function(.data) {
  rcrd_names <- names(.data)[map_lgl(.data, function(x) "edbl_rcrd" %in% class(x))]
  .data[rcrd_names] <- lapply(.data[rcrd_names], unclass)
  structure(lapply(.data, function(x) {
    class(x) <- setdiff(class(x), c("edbl_unit", "edbl_rcrd", "edbl_trt", "edbl_fct", "vctrs_vctr"))
    attr(x, "levels") <- NULL
    return(x) }),
            names = names(.data),
            class = c("tbl_df", "tbl", "data.frame"),
            row.names = 1:vec_size_common(!!!.data))
}
