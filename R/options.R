op.edibble <- list(
  edibble.tree.decorate.trts = cli::col_blue,
  edibble.tree.decorate.units = cli::combine_ansi_styles(cli::bg_black, cli::col_yellow),
  edibble.tree.decorate.resp = cli::col_green,
  edibble.tree.decorate.levels = function(x) cli::col_grey(cli::pluralize("({x} level{?s})")),
  edibble.tree.decorate.main = cli::combine_ansi_styles("cyan", "italic"),
  # vector labels
  edibble.levels.label.leading_zero = FALSE,
  edibble.levels.label.min_ndigits = 0,
  # vertex
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  edibble.vertex.label.color.default = "black",
  edibble.vertex.label.color.unit = "#E69F00", # orange
  edibble.vertex.label.color.trt = "#56B4E9", # blue
  edibble.vertex.label.color.resp = "#009E73", # green
  edibble.vertex.shape.default = "none",
  edibble.vertex.shape.unit = "none",
  edibble.vertex.shape.trt = "none",
  edibble.vertex.shape.resp = "none",
  edibble.vertex.fill.default = NA,
  edibble.vertex.fill.unit = NA,
  edibble.vertex.fill.trt = NA,
  edibble.vertex.fill.resp = NA,
  edibble.vertex.label.family.default = "mono",
  edibble.vertex.label.family.unit = "mono",
  edibble.vertex.label.family.trt = "mono",
  edibble.vertex.label.family.resp = "mono",
  edibble.vertex.label.font.default = 2,
  edibble.vertex.label.font.unit = 2,
  edibble.vertex.label.font.trt = 2,
  edibble.vertex.label.font.resp = 2,
  edibble.vertex.label.cex.default = 1,
  edibble.vertex.label.cex.unit = 1,
  edibble.vertex.label.cex.trt = 1,
  edibble.vertex.label.cex.resp = 1,
  # edge
  edibble.edge.color.v2l = "gray",
  edibble.edge.color.l2lseq = "gray",
  edibble.edge.color.v2v = "black",
  edibble.edge.color.l2l = "black",
  edibble.edge.color.t2v = "#56B4E9", # blue
  edibble.edge.color.t2vmay = "#56B4E9",
  edibble.edge.arrow.mode.v2l = "-",
  edibble.edge.arrow.mode.l2lseq = "-",
  edibble.edge.arrow.mode.v2v = "->",
  edibble.edge.arrow.mode.l2l = "->",
  edibble.edge.arrow.mode.t2v = "->",
  edibble.edge.arrow.mode.t2vmay = "->",
  edibble.edge.arrow.size.v2l = 0.3,
  edibble.edge.arrow.size.l2lseq = 0.3,
  edibble.edge.arrow.size.v2v = 0.3,
  edibble.edge.arrow.size.l2l = 0.3,
  edibble.edge.arrow.size.t2v = 0.3,
  edibble.edge.arrow.size.t2vmay = 0.3,
  edibble.edge.width.v2l = 1,
  edibble.edge.width.l2lseq = 1,
  edibble.edge.width.v2v = 1,
  edibble.edge.width.l2l = 1,
  edibble.edge.width.t2v = 1,
  edibble.edge.width.t2vmay = 1,
  edibble.edge.lty.v2l = 3, # vertex to level
  edibble.edge.lty.l2lseq = 3, # level to level sequence
  edibble.edge.lty.v2v = 1, # vertex to vertex nesting
  edibble.edge.lty.l2l = 1, # level to level nesting
  edibble.edge.lty.t2v = 1, # treatment to variable
  edibble.edge.lty.t2vmay = 2 # treatment level to variable level potential
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

edibble_vertex_opt <- function(x) {
  edibble_opt(x, "edibble.vertex.")
}

edibble_labels_opt <- function(x) {
  edibble_opt(x, "edibble.levels.label.")
}


vertex_attr_opt <- function(vtype) {
  type <- ifelse(vtype=="var", "default", vtype)
  attr <- list(class = paste0("edbl_", type),
               color = edibble_opt(paste0("vertex.fill.", type)),
               shape = edibble_opt(paste0("vertex.shape.", type)),
               label.color = edibble_opt(paste0("vertex.label.color.", type)),
               label.family = edibble_opt(paste0("vertex.label.family.", type)),
               label.font = edibble_opt(paste0("vertex.label.font.", type)),
               label.cex = edibble_opt(paste0("vertex.label.cex.", type)))
  attr
}

edge_attr_opt <- function(type = c("v2l", "l2lseq", "v2v", "l2l", "t2v", "t2vmay")) {
  type <- match.arg(type)
  attr <- list(etype = type,
               color = edibble_opt(paste0("edge.color.", type)),
               arrow.mode = edibble_opt(paste0("edge.arrow.mode.", type)),
               arrow.size = edibble_opt(paste0("edge.arrow.size.", type)),
               width = edibble_opt(paste0("edge.width.", type)),
               lty = edibble_opt(paste0("edge.lty.", type)),
               curved = FALSE)
  attr
}
