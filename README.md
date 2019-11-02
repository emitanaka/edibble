
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edibble

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The main aims of `edibble` R-package are:

1.  facilitate statistical thinking of adapting experimental designs to
    different conditions by implementing the **grammar of experimental
    design** to generate new designs, and
2.  lubricate the project flow of generating experimental data.

The name origin of `edibble` is a play on `tibble` (a modern take on
data frames) with motivation as `tibble` for experimental design.

The grammatical expressions to generate design can draw parallel to
`dplyr` so some functions may be decoupled to a package called `edplyr`
🤔

## Experimental data

The focus of `tidyverse` R-package is well suited for the data science
project workflow as illustrated below in (B) (from [Grolemund and
Wickham 2017](https://r4ds.had.co.nz/introduction.html)). For
experimental data, statististical project begin before obtaining data as
depicted below in (A). The focus of `edibble` is to facilitate work in
(A).

<img src="man/figures/design-analysis-flow.png" width = "100%"/>

## Statistical thinking

The central idea of the *grammar of experimental design* can be
illustrated by drawing analogy to the (layered) grammar of graphics
implemented in `ggplot2`.

Grammar of graphics defines independent graphical components that make
possible to graph complex plots and make it extensible. For example, in
figure (B) below, a stacked barplot is transformed to a pie chart by
transforming the coordinate system from Cartersian to polar coordinates.
The corresponding functions to generate these figures in `base` R are
`barplot` and `pie`. These singular purpose graphic functions can only
generate the corresponding “named plots”. Hence why `ggplot2` is
powerful as the functions are no longer limiting the creation of
different plots.

In experimental design, `agricolae` is by far the most popular R package
on the CRAN task view of experimental design. Just as there are “named
plots”, there are “named experimental designs”. E.g. completely
randomised design (CRD), randomised complete block design (CRBD),
balanced incomplete block design (BIBD) and so on. The functions to
generate designs in `agricolae` are motivated by these named designs
(e.g. `design.crd()` and `design.rcbd()`). These functions are limiting
the adaptation of experimental designs to different conditions in a
similar manner to the `base` plots. Figure (A) below shows how CRD is
tranformed to RCBD by imposing a blocking structure on the units
(provided the units and treatment number are conformable).

<img src="man/figures/grammar-intro.png" width="100%"/>

Below are connections between different named experimental designs.

<img src="man/figures/design-connection.png" width="100%"/>

Below show examples where the **elements of experimental designs** are
identified.

<img src="man/figures/expdesign1.png" width="100%"/>
<img src="man/figures/expdesign2.png" width="100%"/>
<img src="man/figures/expdesign3.png" width="100%"/>

## Project Flow

  - `edibble` will integrate an easy way to export the design in a
    rectangular tabular format so there is a smooth translation from the
    generation of experimental design to data collection.

  - `edibble` will allow easy visualisation of experimental designs that
    serve as a communication medium in a number scenarios. For example,
    an easy way to visualise the experimental design would ensure that
    all parties involved can digest the design quickly and correct any
    mistake as needed before conducting the experiment.

## Installation

Currently the package is not available for
installation.

<!-- And the development version from [GitHub](https://github.com/) with: -->

<!-- ``` r -->

<!-- # install.packages("devtools") -->

<!-- devtools::install_github("emitanaka/edibble") -->

<!-- ``` -->

## Related Work

  - `DeclareDesign` although motivation is different to `edibble`.

## Acknowlegement

Thanks to Francis Hui, Di Cook, Rob Hyndman and Nick Tierney for their
feedback on the proposed idea.
