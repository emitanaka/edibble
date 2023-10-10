op.edibble <- list(
  edibble.tree.decorate.trts = cli::col_blue,
  edibble.tree.decorate.units = cli::combine_ansi_styles(cli::bg_black, cli::col_yellow),
  edibble.tree.decorate.rcrds = cli::col_green,
  edibble.tree.decorate.levels = function(x) cli::col_grey(cli::pluralize("({x} level{?s})")),
  edibble.tree.decorate.title = cli::combine_ansi_styles("cyan", "italic"),
  # vector labels
  edibble.levels.label.leading_zero = TRUE,
  edibble.levels.label.min_ndigits = 0,
  edibble.levels.label.sep = ""
)

edibble_opt <- function(x, prefix = "edibble.") {
  if(missing(x)) {
    op.edibble
  } else {
    opt_name <- paste0(prefix, x)
    res <- getOption(opt_name)
    if(!is_null(res)) return(res)
    op.edibble[[opt_name]]
  }
}

edibble_decorate <- function(x) {
  edibble_opt(x, "edibble.tree.decorate.")
}

edibble_labels_opt <- function(x) {
  edibble_opt(x, "edibble.levels.label.")
}

