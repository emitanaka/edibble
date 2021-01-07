#' @importFrom cli cli_alert_success
#' @importFrom openxlsx addStyle createStyle createWorkbook addWorksheet writeData dataValidation saveWorkbook
export_design <- function(.data, file, overwrite = FALSE) {
  .design <- attr(.data, "design")
  wb <- createWorkbook()
  addWorksheet(wb, .design$name)
  writeData(wb, sheet = .design$name, x = .design$name,
            startCol = 1, startRow = 1)
  writeData(wb, sheet = .design$name,
            x = unlist(.design$context),
            startCol = 2, startRow = 3)
  writeData(wb, sheet = .design$name,
            x = names(.design$context),
            startCol = 1, startRow = 3)
  addWorksheet(wb, "Data")
  hs <- createStyle(
    fgFill = "#DCE6F1", halign = "left", textDecoration = "bold",
    border = "Bottom"
  )
  writeData(wb, sheet = "data", x = as_data_frame(.data), startCol = 1,
            headerStyle = hs)

  if(!is_null(.design$validation)) {
    # TO DO
    addWorksheet(wb, "validation values")
    dataValidation(wb, sheet = "data", ...)
  }
  success <- saveWorkbook(wb, file, overwrite = overwrite, returnValue = TRUE)
  if(success) {
    cli_alert_success("{.emph {.design$name}} has been written to {.file {file}}")
  } else {
    cli_alert_warning("Something went wrong. {.emph {.design$name}} failed to be exported.")
  }
  invisible(.data)
}


as_data_frame <- function(.data) {
  structure(lapply(.data, as.character),
            names = names(.data),
            class = "data.frame",
            row.names = 1:nrow(.data))
}
