

new_recipe_design <- function(name, name_full = name, code = NULL) {
  dname <- edibble_decorate("title")(paste0('"', paste(name_full, collapse = " | "), '"'))
  structure(list(name = name,
                 name_full = name_full,
                 code = code %||% paste0("design(", dname, ")")),
            class = "recipe_design")
}

object_add_code <- function(.object, ...) {
  dots <- list2(...)
  for(x in dots) {
    .object$code <- sprintf('%s %%>%%
  %s', .object$code, x)
  }
  .object
}

#' @export
print.recipe_design <- function(x, ...) {
  #name_full <- edibble_decorate("title")(x$name_full)
  #cat(cli::style_italic(paste(name_full, collapse = " | ")), "\n")
  cat(x$code)
  cat("\n")
}

random_integer_small <- function(min = 1, max = 10) min + sample(max - min, 1)
random_integer_medium <- function(min = 1) min + sample(10:25, 1)
random_seed_number <- function() sample(1000, 1)
random_true_false <- function() sample(c(TRUE, FALSE), 1)

#' Prepare a randomised complete block design
#'
#' @param t The number of treatments.
#' @param r The number of replications for each treatment level.
#' @param seed A scalar value for computational reproducibility.
#' @param name_title The name of the design or title.
#' @param name_block The name of the block factor.
#' @param name_unit The name of the unit factor
#' @param name_trt The name of the treatment factor.
#' @family recipe-designs
#' @examples
#' menu_rcbd(t = 3, r = 2)
#' @return A recipe for randomised complete block design.
#' @export
menu_rcbd <- function(t = random_integer_small(),
                      r = random_integer_small(),
                      seed = random_seed_number(),
                      name_title = "Randomised Complete Block Design",
                      name_block = "block",
                      name_unit = "unit",
                      name_trt = "trt") {

  des <- new_recipe_design(name = "rcbd",
                          name_full = name_title)
  block <- edibble_decorate("units")(name_block)
  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)
  des <- object_add_code(des,
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
#' @param name_row,name_col The names for the row and column factors.
#' @param name_trt1,name_trt2 The name of the treatment factors.
#' @examples
#' menu_graeco(t = 3)
#' @return A recipe for Graeco-Latin square design.
#' @export
menu_graeco <- function(t = random_integer_small(),
                        seed = random_seed_number(),
                        name_title = "Graeco-Latin Square Design",
                        name_row = "row",
                        name_col = "col",
                        name_unit = "unit",
                        name_trt1 = "trt1",
                        name_trt2 = "trt2") {
  des <- new_recipe_design(name = "graeco",
                           name_full = name_title)

  row <- edibble_decorate("units")(name_row)
  col <- edibble_decorate("units")(name_col)
  unit <- edibble_decorate("units")(name_unit)
  trt1 <- edibble_decorate("trts")(name_trt1)
  trt2 <- edibble_decorate("trts")(name_trt2)

  des <- object_add_code(des,
                              sprintf('set_units(%s = %d,
            %s = %d,
            %s = crossed_by(%s, %s))', row, t, col, t, unit, row, col),
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
#' @examples
#' menu_crd(t = 3, n = 10)
#' @return A recipe for completely randomised design.
#' @export
menu_crd <- function(t = random_integer_small(),
                     n = random_integer_medium(min = t),
                     r = NULL,
                     seed = random_seed_number(),
                     name_title = "Completely Randomised Design",
                     name_unit = "unit",
                     name_trt = "trt") {


  # checks
  if(!missing(n) & !is_null(r)) {
    abort("You cannot define both `n` and `r`.")
  }
  if(missing(n) & !is_null(r)) {
    n <- r * t
  }

  des <- new_recipe_design(name = "crd",
                           name_full = name_title)

  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)

  des <- object_add_code(des,
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
#' @examples
#' menu_factorial(trt = c(3, 2), r = 2, design = "crd")
#' @return A recipe for factorial design.
#' @export
menu_factorial <- function(trt = c(random_integer_small(),
                                   random_integer_small()),
                           r = random_integer_small(),
                           design = c("crd", "rcbd"),
                           seed = random_seed_number(),
                           name_title = NULL,
                           name_unit = "unit",
                           name_block = "block") {
  design <- match.arg(design)
  des <- new_recipe_design(name = "factorial",
                          name_full = name_title %||% paste0("Factorial Design",
                                             switch(design,
                                                    "crd" = "",
                                                    "rcbd" = " with RCBD structure")))
  unit <- edibble_decorate("units")(name_unit)
  block <- edibble_decorate("units")(name_block)
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

  des <- object_add_code(des,
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
#' @examples
#' menu_split(t1 = 3, t2 = 2, r = 4)
#' @return A recipe split-plot design.
#' @export
menu_split <- function(t1 = random_integer_small(),
                       t2 = random_integer_small(),
                       r = random_integer_small(),
                       seed = random_seed_number(),
                       name_title = c("Split-Plot Design",
                                      "Split-Unit Design"),
                       name_mainplot = "mainplot",
                       name_subplot = "subplot",
                       name_trt1 = "trt1",
                       name_trt2 = "trt2") {

  n <- t1 * t2 * r
  des <- new_recipe_design(name = "split",
                           name_full = name_title)

  mainplot <- edibble_decorate("units")(name_mainplot)
  subplot <- edibble_decorate("units")(name_subplot)
  trt1 <- edibble_decorate("trts")(name_trt1)
  trt2 <- edibble_decorate("trts")(name_trt2)

  des <- object_add_code(des,
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

#' Balance incomplete block design
#'
#' Some combinations of parameter values cannot create a balanced incomplete
#' block design.
#'
#' @inheritParams menu_rcbd
#' @param k The size of the block. This should be less than the number of
#'   treatments.
#' @family recipe-designs
#' @examples
#' menu_bibd(t = 3, k = 2, r = 4)
#' @return A recipe for balance incomplete block design.
#' @export
menu_bibd <- function(t = random_integer_small(min = 3),
                      k = random_integer_small(max = t - 1),
                      r = random_integer_small(),
                      seed = random_seed_number(),
                      name_title = "Balanced Incomplete Block Design",
                      name_block = "block",
                      name_unit = "unit",
                      name_trt = "trt") {
  if(k >= t) abort("The size of the block `k` must be smaller than `t`.")

  b <- r * t / k
  lambda <- r * (k - 1) / (t - 1)
  # since I derive b myself, below isn't necessary
  # if(lambda %% 1 != 0 &
  #    b %% 1 != 0 &
  #    r <= lambda &
  #    lambda * (t - 1) != r * (k - 1)) {
  #   abort("The chosen parameters cannot create a balanced incomplete block design.")
  # }
  b <- as.integer(b)

  des <- new_recipe_design(name = "bibd",
                           name_full = name_title)
  block <- edibble_decorate("units")(name_block)
  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)
  des <- object_add_code(des,
                                sprintf('set_units(%s = %d,
            %s = nested_in(%s, %d))', block, b, unit, block, k),
            sprintf('set_trts(%s = %d)', trt, t),
            sprintf('allot_trts(%s ~ %s)', trt, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')
  des

}

#' Strip-unit design
#'
#'
#' @inheritParams menu_split
#' @inheritParams menu_graeco
#' @family recipe-designs
#' @examples
#' menu_strip(t1 = 3, t2 = 3, r = 2)
#' @return A recipe strip-unit design.
#' @export
menu_strip <- function(t1 = random_integer_small(),
                       t2 = random_integer_small(),
                       r = random_integer_small(),
                       seed = random_seed_number(),
                       name_title = c("Strip-Plot Design", "Strip-Unit Design"),
                       name_block = "block",
                       name_unit = "unit",
                       name_row = "row",
                       name_col = "col",
                       name_trt1 = "trt1",
                       name_trt2 = "trt2") {
  des <- new_recipe_design(name = "strip",
                           name_full = title)
  block <- edibble_decorate("units")(name_block)
  unit <- edibble_decorate("units")(name_unit)
  row <- edibble_decorate("units")(name_row)
  col <- edibble_decorate("units")(name_col)
  trt1 <- edibble_decorate("trts")(name_trt1)
  trt2 <- edibble_decorate("trts")(name_trt2)

  des <- object_add_code(des,
                                sprintf('set_units(%s = %d,
            %s = nested_in(%s, %d),
            %s = nested_in(%s, %d),
            %s = nested_in(%s, crossed_by(%s, %s)))', block, r, row, block, t1, col, block, t2, unit, block, row, col),
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
#' @param nc The number of columns.
#' @family recipe-designs
#' @importFrom cli style_italic
#' @examples
#' menu_youden(nc = 4, t = 5)
#' @return A recipe Youden square design.
#' @export
menu_youden <- function(nc = random_integer_small(),
                        t = random_integer_small(min = nc + 1),
                        seed = random_seed_number(),
                        name_title = "Youden Square Design",
                        name_row = "row",
                        name_col = "col",
                        name_unit = "unit",
                        name_trt = "trt") {
  des <- new_recipe_design(name = "youden",
                           name_full = name_title)

  row <- edibble_decorate("units")(name_row)
  col <- edibble_decorate("units")(name_col)
  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)

  des <- object_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = crossed_by(%s, %s))', row, t, col, nc, unit, row, col),
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
#' @inheritParams menu_graeco
#' @family recipe-designs
#' @importFrom cli style_italic
#' @examples
#' menu_lsd(t = 3)
#' @return A recipe Latin square design.
#' @export
menu_lsd <- function(t = random_integer_small(),
                     seed = random_seed_number(),
                     name_title = "Latin Square Design",
                     name_row = "row",
                     name_col = "col",
                     name_unit = "unit",
                     name_trt = "trt") {
  des <- new_recipe_design(name = "lsd",
                          name_full = name_title)

  row <- edibble_decorate("units")(name_row)
  col <- edibble_decorate("units")(name_col)
  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)

  des <- object_add_code(des,
                               sprintf('set_units(%s = %d,
            %s = %d,
            %s = crossed_by(%s, %s))', row, t, col, t, unit, row, col),
             sprintf('set_trts(%s = %d)', trt, t),
           sprintf('allot_trts(%s ~ %s)', trt, unit),
           sprintf('assign_trts("random", seed = %d)', seed),
           'serve_table()')

  des
}

#' Hyper-Graeco-Latin Square Design
#'
#' @param t The number of treatments
#' @param block1,block2,block3,block4 The names of the block factors.
#' @inheritParams menu_rcbd
#' @family recipe-designs
#' @examples
#' menu_hyper_graeco(t = 3)
#' @return A recipe Hyper-Graeco-Latin square design.
#' @export
menu_hyper_graeco <- function(t = random_integer_small(),
                              seed = random_seed_number(),
                              name_title = "Hyper-Graeco-Latin Square Design",
                              name_block1 = "block1",
                              name_block2 = "block2",
                              name_block3 = "block3",
                              name_block4 = "block4",
                              name_unit = "name_unit",
                              name_trt = "name_trt") {
  des <- new_recipe_design(name = "hyper_graeco",
                           name_full = name_title)

  block1 <- edibble_decorate("units")(name_block1)
  block2 <- edibble_decorate("units")(name_block2)
  block3 <- edibble_decorate("units")(name_block3)
  block4 <- edibble_decorate("units")(name_block4)
  unit <- edibble_decorate("units")(name_unit)
  trt <- edibble_decorate("trts")(name_trt)

  des <- object_add_code(des,
                                sprintf('set_units(%s = %d,
            %s = %d,
            %s = %d,
            %s = %d,
            %s = crossed_by(%s, %s, %s, %s))', block1, t, block2, t, block3, t, block4, t, unit,
            block1, block2, block3, block4),
            sprintf('set_trts(%s = %d)', trt, t),
            sprintf('allot_trts(%s ~ %s)', trt, unit),
            sprintf('assign_trts("random", seed = %d)', seed),
            'serve_table()')

  des
}





#' Find the short names of the named designs
#'
#' @param packages A character vector containing the package names to search
#' named designs from. By default it will search edibble and other packages loaded.
#' @param exclude A character vector denoting the packages to exclude search from.
#' @return A data.frame with package, name, arguments, and full name.
#' @examples
#' scan_menu()
#' @export
scan_menu <- function(packages = NULL, exclude = NULL) {
  # ignore searching in base pkgs
  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets",
                 "methods", "base")
  packages <- packages %||% setdiff(.packages(), base_pkgs)
  packages <- setdiff(packages, exclude)
  packages <- unique(c(packages, "edibble")) # always add edibble whether it is loaded or not

  ls_fns <- lapply(packages, function(pkg) {
    fns <- unclass(utils::lsf.str(envir = asNamespace(pkg), all = TRUE))
    fns[grep("^menu_", fns)]
  })
  names(ls_fns) <- packages
  ls_fns <- compact(ls_fns)

  pkg_names <- names(ls_fns)
  ret <- tibble::tibble(package = character(),
                        name = character(),
                        args = character(),
                        name_full = character())
  for(i in seq_along(ls_fns)) {
    for(menu_fn in ls_fns[[i]]) {
      args <- as.list(formals(menu_fn))
      des <- do.call(menu_fn, list())
      tryCatch({
        ret <- tibble::add_row(ret,
                               package = pkg_names[i],
                               name = des$name,
                               args = paste0(names(args), collapse = ", "),
                               name_full = paste0(des$name_full, collapse = ", "))
      }, error = function(x)  {
        cli_li("{.pkg {gsub('menu_', '', menu_fn)}} seems to be {cli::col_red('unavailable')}.")
      })
    }
  }
  ret
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
#' @return A recipe design.
#' @importFrom cli cli_h1 cli_ul cli_end cli_h2 col_grey style_italic ansi_strip
#' @export
takeout <- function(recipe = NULL, show = TRUE) {
  if(is.null(recipe)) {
    cli::cli_alert("No name was supplied so selecting a random named experimental design...")
    name <- sample(scan_menu()$name, 1L)
    recipe <- do.call(paste0("menu_", name), list())
    cli::cli_alert(sprintf("Selected %s", recipe$name_full))
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
#' @examples
#' examine_recipe(takeout())
#' @return The recipe code.
#' @export
examine_recipe <- function(x, ...) {
  UseMethod("examine_recipe")
}

#' @export
examine_recipe.default <- function(x, ...) {
  abort(sprintf("`examine_recipe` is not implemented for class %s.", .combine_words(class(x))))
}

#' @export
examine_recipe.edbl_design <- function(x, ...) {
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
    paste0(deparse(as.call(lline)), collapse = "\n")
  })
  object_add_code(recipe, !!!as.list(code))
}

#' @export
examine_recipe.edbl_table <- function(x, ...) {
  examine_recipe(edbl_design(x))
}

#' @export
examine_recipe.takeout <- function(x, ...) {
  attr(x, "recipe")
}

#' A function to check if the output is a takeout design
#'
#' The function returns `TRUE` if the input is a takeout design.
#'
#' @param x An object.
#' @return A logical value.
#' @examples
#' is_takeout(takeout())
#' @export
is_takeout <- function(x) {
  inherits(x, "takeout")
}



