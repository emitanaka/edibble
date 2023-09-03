


#' Export the design to xlsx
#'
#' @description
#' This function is designed to export the design made using edibble to an
#' external xlsx file.
#'
#' @param .data An edibble table to export.
#' @param file File, including the path, to export the data to.
#' @param author (Optional) name of the author in character. A vector of character is supported
#'  for where there are multiple authors.
#' @param date The date to be inserted in header (defaults to today).
#' @param overwrite A logical value indicating whether to overwrite existing file or not.
#' @param hide_treatments A logical value indicating whether treatments should be included in the data entry sheet.
#'  Default is true.
#' @param theme The Excel theme to use (optional). One of "Atlas", "Badge", "Berlin", "Celestial", "Crop", "Depth", "Droplet", "Facet", "Feathered", "Gallery", "Headlines", "Integral", "Ion", "Ion Boardroom", "Madison", "Main Event", "Mesh", "Office Theme", "Old Office Theme", "Organic", "Parallax", "Parcel", "Retrospect", "Savon", "Slice", "Vapor Trail", "View", "Wisp", "Wood Type".
#' @param subject The subject of the workbook (optional).
#' @param category The category of the workbook (optional).
#' @param table_style The table style to apply to the exported data (default: "TableStyleMedium9").
#' @family user-facing functions
#' @return The input data object.
#' @export
export_design <- function(.data,
                          file,
                          author = NULL,
                          date = Sys.Date(),
                          overwrite = FALSE,
                          hide_treatments = FALSE,
                          theme = NULL,
                          subject = NULL,
                          category = NULL,
                          table_style = "TableStyleMedium9") {

  if(!is_edibble_table(.data)) {
    abort("The input is not an edibble table.")
  }

  if(!requireNamespace("openxlsx2")) {
    abort("Please install the `openxlsx2` package to use this function.")
  }

  prov <- activate_provenance(.data)

  title <- prov$get_title() %||% "An edibble experiment"
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

  write_title_sheet(wb, sheet_names[1], title, author, date)

  write_data_sheet(wb, sheet_names[-c(1, 2, length(sheet_names))], prov,
                   as_tibble(.data), table_style, hide_treatments)


  write_grand_data_sheet(wb, sheet_names[2], prov,
                         as_tibble(.data), table_style)

  wb$protect_worksheet(sheet = sheet_names[2],
                       protect = TRUE,
                       properties = c("formatCells",
                                      "formatColumns",
                                      "formatRows",
                                      "insertRows",
                                      "deleteColumns",
                                      "deleteRows",
                                      "sort",
                                      "autoFilter",
                                      "pivotTables",
                                      "objects",
                                      "scenarios"))

  write_variables_sheet(wb, sheet_names[length(sheet_names)], prov, .data)

  save_workbook(wb, file, overwrite, title)

  invisible(.data)
}

data_sheet_name <- function(name) {
  paste0("Data.", name)
}

make_sheet_names <- function(prov) {
  rexists <- prov$rcrd_exists(abort = FALSE)
  texists <- prov$trt_exists(abort = FALSE)
  if(!rexists & !texists) {
    # if no record and treatment is supplied, then use the smallest unit
    uname <- prov$fct_names(id = prov$fct_id_leaves(role = "edbl_unit"))
  } else {
    map_rcrd_to_unit <- prov$mapping("edbl_rcrd", "edbl_unit")
    map_trt_to_unit <- prov$mapping("edbl_trt", "edbl_unit")
    uname <- prov$fct_names(id = unique(c(map_rcrd_to_unit)))
  }
  data_sheet_names <- data_sheet_name(uname)
  names(data_sheet_names) <- uname
  c("Context", "Data", data_sheet_names, "Variables")
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



write_data_sheet <- function(wb, sheet_names, prov, data, table_style, hide_treatments) {
  for(iunit in seq_along(sheet_names)) {

    if(prov$rcrd_exists(abort = FALSE)) {
      uid <- prov$fct_id(name = names(sheet_names)[iunit])
      data <- as_tibble.edbl_table(prov$serve_units(id = uid, return = "value"))
      if(prov$trt_exists(abort = FALSE) && !hide_treatments) {
        trts <- as_tibble.edbl_table(prov$serve_trts(id = prov$fct_id_parent(id = uid, role = "edbl_trt"),
                                                     return = "value"))
        data <- cbind(data, trts)
      }
      rids <- prov$fct_id_parent(id = uid, role = "edbl_rcrd")
      for(rid in rids) {
        data[[prov$fct_names(id = rid)]] <- NA
      }
    }

    write_data_table(wb, sheet_names[iunit], data, table_style)

  }
}

write_data_table <- function(wb, sheet, data, table_style) {
  wb$add_data_table(sheet = sheet,
                    x = data,
                    table_style = table_style,
                    na.strings = "",
                    with_filter = FALSE)

  col_width <- vapply(data, function(x) max(nchar(format(x))),
                      NA_real_)

  wb$set_col_widths(sheet = sheet,
                    cols = 1:ncol(data),
                    widths = max(col_width))
}


write_grand_data_sheet <- function(wb, sheet_name, prov, data, table_style)  {
  if(prov$rcrd_exists(abort = FALSE)) {
    map_rcrd_to_unit <- prov$mapping("edbl_rcrd", "edbl_unit")
    for(rid in as.numeric(names(map_rcrd_to_unit))) {
      rname <- prov$fct_names(id = rid)
      uname <- prov$fct_names(id = map_rcrd_to_unit[as.character(rid)])
      dname <- data_sheet_name(uname)
      df <- wb$to_df(sheet = dname)
      col <- match(rname, names(df))
      rows <- match(data[[uname]], df[[uname]])
      data[[rname]] <- paste0(dname, "!", map_chr(rows, function(row) openxlsx2::wb_dims(row + 1L, col)))
      class(data[[rname]]) <- c(class(data[[rname]]), "formula")
    }

  }
  write_data_table(wb, sheet_name, data, table_style)
}



write_variables_sheet <- function(wb, sheet_name, prov, data) {
  type <- map_chr(data, function(var) {
    if(inherits(var, "edbl_unit")) return("unit")
    if(inherits(var, "edbl_trt")) return("trt")
    if(inherits(var, "edbl_rcrd")) return("rcrd")
    "fct"
  })
  vardf <- data.frame(variable = names(data),
                   type = unname(type),
                   nlevels = map_int(names(data), function(var) {
                     id <- prov$fct_id(name = var)
                     role <- prov$fct_role(id = id)
                     if(role == "edbl_rcrd") {
                       uid <- prov$mapping_to_unit(id = id)
                       var <- prov$fct_names(id = uid)
                     }
                     length(unique(data[[var]]))
                   }),
                   stringsAsFactors = FALSE)

  valids <- prov$get_validation("rcrds")
  if(!is_null(valids)) {
    vardf$record <- ""
    vardf$value <- ""
    valid_names <- names(valids)
    for(ivalid in seq_along(valids)) {
      valid <- valids[[ivalid]]
      rname <- valid_names[ivalid]
      rid <- prov$fct_id(name = rname)
      uid <- prov$mapping_to_unit(id = rid)
      uname <- prov$fct_names(id = uid)
      data_sheet <- data_sheet_name(uname)

      dat <- wb$to_df(sheet = data_sheet)
      i <- which(vardf$variable == rname)
      jdata <- which(names(dat) == rname)
      vardf$record[i] <- valid$record

      if(valid$type != "list") {
        vardf$value[i] <- restriction_for_human(valid$operator, valid$value)
        wb$add_data_validation(sheet = data_sheet,
                               dims = openxlsx2::wb_dims(1:nrow(dat) + 1L, jdata),
                               type = valid$type,
                               operator = valid$operator,
                               value = valid$value)
      } else {
        j <- which(names(vardf) == "value")

        values <- valid$values
        vardf$value[i] <- values[1]
        dim_list <- openxlsx2::wb_dims(i + 1L, j:(j + length(values) - 1))
        wb$add_data(sheet = sheet_name,
                    x = t(data.frame(x = values)),
                    dims = dim_list,
                    col_names = FALSE)
        L <- gsub("[0-9]+", "", strsplit(dim_list, ":")[[1]])
        wb$add_data_validation(sheet = data_sheet,
                               dims = openxlsx2::wb_dims(1:nrow(dat) + 1L, jdata),
                               type = "list",
                               value = paste0("'", sheet_name, "'!$",
                                      L[1], "$", i + 1L, ":$", L[2], "$", i + 1L))
      }

    }
  }
  wb$add_data(sheet = sheet_name,
              x = vardf)

  wb$add_font(sheet = sheet_name,
              dims = openxlsx2::wb_dims(1, 1:ncol(vardf)),
              bold = TRUE)
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
#' @param ... Not currently used.
#' @return A data.frame.
#' @importFrom tibble as_tibble
#' @export
as_tibble.edbl_table <- function(x, ...) {
  rcrd_names <- names(x)[map_lgl(x, function(.x) "edbl_rcrd" %in% class(.x))]
  x[rcrd_names] <- lapply(x[rcrd_names], unclass)
  structure(lapply(x, function(.x) {
    class(.x) <- setdiff(class(.x), c("edbl_unit", "edbl_rcrd", "edbl_trt", "edbl_fct", "vctrs_vctr"))
    attr(.x, "levels") <- NULL
    attr(.x, "name") <- NULL
    return(.x) }),
            names = names(x),
            class = c("tbl_df", "tbl", "data.frame"),
            row.names = 1:vec_size_common(!!!x))
}
