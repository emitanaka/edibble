

new_recipe_design <- function(name, name_full = name, code = NULL) {
  dname <- edibble_decorate("title")(paste0('"', paste(name_full, collapse = " | "), '"'))
  structure(list(name = name,
                 name_full = name_full,
                 code = code %||% paste0("start_design(", dname, ")")),
            class = "recipe_design")
}

recipe_design_add_code <- function(.named, ...) {
  dots <- list2(...)
  for(x in dots) {
    .named$code <- sprintf('%s %%>%%
  %s', .named$code, x)
  }
  .named
}

#' @export
print.recipe_design <- function(x, ...) {
  #name_full <- edibble_decorate("title")(x$name_full)
  #cat(cli::style_italic(paste(name_full, collapse = " | ")), "\n")
  cat(x$code)
  cat("\n")
}

random_integer_small <- function(min = 1) min + sample(10, 1)
random_integer_medium <- function(min = 1) min + sample(10:25, 1)
random_seed_number <- function() sample(1000, 1)

#' Prepare a randomised complete block design
#'
#' @param t The number of treatments.
#' @param r The number of replicate blocks.
#' @param seed A scalar value for computational reproducibility.
#' @family recipe-designs
#' @export
menu_rcbd <- function(t = random_integer_small(),
                      r = random_integer_small(),
                      seed = random_seed_number()) {

  des <- new_recipe_design(name = "rcbd",
                          name_full = "Randomised Complete Block Design")
  block <- edibble_decorate("units")("block")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")
  des <- recipe_design_add_code(des,
        sprintf('set_units(%s = %d,
            %s = nested_in(%s, %d))', block, r, unit, block, t),
        sprintf('set_trts(%s = %d)', trt, t),
        sprintf('allot_trts(%s ~ %s)', trt, unit),
        sprintf('assign_trts("random", seed = %d)', seed),
        'serve_table()')
  des
}



#' Graeco-Latin Square Design
#'
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @export
menu_graeco <- function(t = random_integer_small(),
                        seed = random_seed_number()) {
  des <- new_recipe_design(name = "graeco",
                          name_full = "Graeco-Latin Square Design")

  row <- edibble_decorate("units")("row")
  col <- edibble_decorate("units")("col")
  unit <- edibble_decorate("units")("unit")
  trt1 <- edibble_decorate("trts")("trt1")
  trt2 <- edibble_decorate("trts")("trt2")

  des <- recipe_design_add_code(des,
                              sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, col, t, unit, row, col),
            sprintf('set_trts(%s = %d,
           %s = %d)', trt1, t, trt2, t),
            sprintf('allot_trts(%s ~ %s,
             %s ~ %s)', trt1, unit, trt2, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}




#' Completely randomised design
#'
#' @param t The number of treatment levels
#' @param n The number of experimental units
#' @param r (Optional) The number of replicates.
#' @family recipe-designs
#' @inheritParams menu_rcbd
#' @export
menu_crd <- function(t = random_integer_small(),
                               n = random_integer_medium(min = t),
                               r = NULL,
                               seed = random_seed_number()) {

  # checks
  if(!missing(n) & !is_null(r)) {
    abort("You cannot define both `n` and `r`.")
  }
  if(missing(n) & !is_null(r)) {
    n <- r * t
  }

  des <- new_recipe_design(name = "crd",
                          name_full = "Completely Randomised Design")

  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- recipe_design_add_code(des,
                               sprintf('set_units(%s = %d)', unit, n),
                               sprintf('set_trts(%s = %d)', trt, t),
                               sprintf('allot_trts(%s ~ %s)', trt, unit),
                               sprintf('assign_trts("random", seed = %d)', seed),
                               'serve_table()')

  des
}





#' Prepare a factorial design
#'
#' @param trt A vector of the number of levels for each treatment factor.
#' @param design The unit structure: "crd" or "rcbd". The default is "crd".
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @export
menu_factorial <- function(trt = c(random_integer_small(),
                                             random_integer_small()),
                                     r = random_integer_small(),
                                     design = c("crd", "rcbd"),
                                     seed = random_seed_number()) {
  design <- match.arg(design)
  des <- new_recipe_design(name = "factorial",
                          name_full = paste0("Factorial Design",
                                             switch(design,
                                                    "crd" = "",
                                                    "rcbd" = " with RCBD structure")))
  unit <- edibble_decorate("units")("unit")
  block <- edibble_decorate("units")("block")
  ntrt <- prod(trt)
  unit_str <- switch(design,
                     "crd" = sprintf('%s = %d', unit, ntrt * r),
                     "rcbd" = sprintf('%s = %d,
             %s = nested_in(%s, %d)',
                                      block, r,
                                      unit, block, ntrt))
  trt_str <- paste0(map_chr(seq_along(trt), function(i)
    paste0(edibble_decorate("trts")(paste0("trt", i)),
           " = ", trt[i])),
    collapse = ",\n           ")

  des <- recipe_design_add_code(des,
                                sprintf('set_units(%s)', unit_str),
                                sprintf('set_trts(%s)', trt_str),
                                sprintf('allot_trts(~%s)', unit),
                                sprintf('assign_trts("random", seed = %d)', seed),
                                'serve_table()')

  des
}




#' Split-unit design
#'
#' Originally referred to as split-plot design when it was first used.
#'
#' @param t1 The number of treatment levels for the main plots.
#' @param t2 The number of treatment levels for the subplots.
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @importFrom cli style_italic
#' @export
menu_split <- function(t1 = random_integer_small(),
                                 t2 = random_integer_small(),
                                 r = random_integer_small(),
                                 seed = random_seed_number()) {

  n <- t1 * t2 * r
  des <- new_recipe_design(name = "split",
                          name_full = c("Split-Plot Design",
                                        "Split-Unit Design"))

  mainplot <- edibble_decorate("units")("mainplot")
  subplot <- edibble_decorate("units")("subplot")
  trt1 <- edibble_decorate("trts")("trt1")
  trt2 <- edibble_decorate("trts")("trt2")

  des <- recipe_design_add_code(des,
                               sprintf('set_units(%s = %d,
             %s = nested_in(%s, %d))', mainplot, t1 * r, subplot, mainplot, t2),
              sprintf('set_trts(%s = %d,
           %s = %d)', trt1, t1, trt2, t2),
              sprintf('allot_trts(%s ~ %s,
             %s ~ %s)', trt1, mainplot, trt2, subplot),
              sprintf('assign_trts("random", seed = %d)', seed),
              'serve_table()')

  des
}


#' Strip-unit design
#'
#'
#' @inheritParams menu_split
#' @family recipe-designs
#' @export
menu_strip <- function(t1 = random_integer_small(),
                       t2 = random_integer_small(),
                       r = random_integer_small(),
                       seed = random_seed_number()) {
  des <- new_recipe_design(name = "strip",
                           name_full = c("Strip-Plot Design",
                                         "Strip-Unit Design"))
  block <- edibble_decorate("units")("block")
  unit <- edibble_decorate("units")("unit")
  row <- edibble_decorate("units")("row")
  col <- edibble_decorate("units")("col")
  trt1 <- edibble_decorate("trts")("trt1")
  trt2 <- edibble_decorate("trts")("trt2")

  des <- recipe_design_add_code(des,
                                sprintf('set_units(%s = %d,
            %s = %d,
            %s = nested_in(%s, %d),
            %s = ~%s:%s)', block, r, row, t1, col, block, t2, unit, row, col),
            sprintf('set_trts(%s = %d,
           %s = %d)', trt1, t1, trt2, t2),
           sprintf('allot_trts(%s ~ %s,
             %s ~ %s)', trt1, row, trt2, col),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}




#' Youden square design
#'
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @importFrom cli style_italic
#' @export
menu_youden <- function(nc = random_integer_small(),
                                  t = random_integer_small(min = nc + 1),
                               seed = random_seed_number()) {
  des <- new_recipe_design(name = "youden",
                          name_full = "Youden Square Design")

  row <- edibble_decorate("units")("row")
  col <- edibble_decorate("units")("col")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- recipe_design_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, col, nc, unit, row, col),
            sprintf('set_trts(%s = %d)', trt, t),
            sprintf('allot_trts(%s ~ %s)', trt, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}





#' Prepare classical Latin square design
#'
#' @param t The number of treatments
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @importFrom cli style_italic
#' @export
menu_lsd <- function(t = random_integer_small(),
                               seed = random_seed_number()) {
  des <- new_recipe_design(name = "lsd",
                          name_full = "Latin Square Design")

  row <- edibble_decorate("units")("row")
  col <- edibble_decorate("units")("col")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- recipe_design_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = ~%s:%s)', row, t, col, t, unit, row, col),
             sprintf('set_trts(%s = %d)', trt, t),
           sprintf('allot_trts(%s ~ %s)', trt, unit),
           sprintf('assign_trts("random", seed = %d)', seed),
           'serve_table()')

  des
}

#' Hyper-Graeco-Latin Square Design
#'
#' @param t The number of treatments
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @export
menu_hyper_graeco <- function(t = random_integer_small(),
                     seed = random_seed_number()) {
  des <- new_recipe_design(name = "hyper_graeco",
                           name_full = "Hyper-Graeco-Latin Square Design")

  block1 <- edibble_decorate("units")("block1")
  block2 <- edibble_decorate("units")("block2")
  block3 <- edibble_decorate("units")("block3")
  block4 <- edibble_decorate("units")("block4")
  unit <- edibble_decorate("units")("unit")
  trt <- edibble_decorate("trts")("trt")

  des <- recipe_design_add_code(des,
                                sprintf('set_units(%s = %d,
            %s = %d,
            %s = %d,
            %s = %d,
            %s = ~%s:%s:%s:%s)', block1, t, block2, t, block3, t, block4, t, unit,
            block1, block2, block3, block4),
            sprintf('set_trts(%s = %d)', trt, t),
            sprintf('allot_trts(%s ~ %s)', trt, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}





#' Find the short names of the named designs
#'
#' @param pkgs A character vector containing the package names to search
#' named designs from. By default it will search edibble and other packages loaded.
#'
#' @export
scan_menu <- function(pkgs = NULL) {
  # ignore searching in base pkgs
  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets",
                 "methods", "base")
  pkgs <- pkgs %||% setdiff(.packages(), base_pkgs)
  pkgs <- unique(c(pkgs, "edibble")) # always add edibble whether it is loaded or not

  ls_fns <- lapply(pkgs, function(pkg) {
    fns <- unclass(lsf.str(envir = asNamespace(pkg), all = TRUE))
    fns[grep("^menu_", fns)]
  })
  names(ls_fns) <- pkgs
  ls_fns <- compact(ls_fns)

  pkg_names <- names(ls_fns)
  short_names <- NULL
  for(i in seq_along(ls_fns)) {
    cli_h2(pkg_names[i])
    for(prep_fn in ls_fns[[i]]) {
      args <- as.list(formals(prep_fn))
      des <- do.call(prep_fn, list())
      short_names <- c(short_names, set_names(des$name, pkg_names[i]))
      cli_li("{.pkg {des$name}} with the arguments {.field {names(args)}}
             for a {.combine_words(des$name_full, fun = style_bold, and = ' / ')}.")
    }
  }
  invisible(short_names)
}




#' Create a named experimental design
#'
#' @description
#'
#' This function generates a named experimental
#' design by supplying the selected menu named design and prints out by default
#; the code to create the design using the fundamental system.
#'
#' You can find the available recipes with `scan_menu()`.
#'
#' @param recipe A named design object. This should be typically generated from a
#'   function with prefix `menu_`. If nothing is supplied, it will randomly select one.
#' @param show A logical value to indicate whether the code should be shown or not.
#'   Default is TRUE.
#'
#' @examples
#' takeout(menu_crd(n = 50, t = 5))
#' # if you omit the design parameters then it will use the default
#' # (which may be random)
#' takeout(menu_crd())
#' # if you don't give any short names then it will generate a random one
#' takeout()
#' @seealso See [scan_menu()] for finding the short names of the
#'  named experimental designs.
#'
#' @importFrom cli cli_h1 cli_ul cli_end cli_h2 col_grey style_italic ansi_strip
#' @export
takeout <- function(recipe = NULL, show = TRUE) {
  if(is.null(recipe)) {
    cli_alert("No name was supplied so selecting a random named experimental design...")
    name <- sample(suppressMessages(scan_menu()), 1L)
    recipe <- do.call(paste0("menu_", name), list())
    cli_alert(sprintf("Selected %s", recipe$name_full))
  }
  df <- eval(parse(text = ansi_strip(recipe$code)))

  res <- structure(df,
                   class = c("takeout", class(df)),
                   recipe = recipe,
                   show = show)

  return(res)
}

#' @export
print.takeout <- function(x, show = NULL, ...) {
  recipe <- attr(x, "recipe")
  show <- show %||% attr(x, "show")


  if(show) {
    cat(recipe$code, "\n")
    cat("\n")
  }

  NextMethod()
  invisible(x)
}

#' Check the recipe code
#'
#' @param x An edibble design, edibble, or takeout object.
#' @param ... Not used.
#' @export
examine_recipe <- function(x, ...) {
  UseMethod("examine_recipe")
}

#' @export
examine_recipe.default <- function(x) {
  abort(sprintf("`examine_recipe` is not implemented for class %s.", .combine_words(class(x))))
}

#' @export
examine_recipe.edbl_design <- function(x) {
  recipe <- new_recipe_design(name = x$name, code = x$recipe[1])
  code <- map_chr(x$recipe[-1], function(.x) {
    line <- str2lang(.x)
    lline <- as.list(line)
    iarg <- which(map_lgl(lline, function(a) is_symbol(a, ".")))
    if(length(iarg)) {
      lline[iarg] <- NULL
    } else {
      lline[2] <- NULL # deletes first argument
    }
    # change this so "," starts a new line
    deparse(as.call(lline))
  })
  recipe_design_add_code(recipe, !!!as.list(code))
}

#' @export
examine_recipe.edbl_table <- function(x) {
  examine_recipe(edbl_design(x))
}

#' @export
examine_recipe.takeout <- function(x) {
  attr(x, "recipe")
}

is_takeout <- function(x) {
  inherits(x, "takeout")
}



