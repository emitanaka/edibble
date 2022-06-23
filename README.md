
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edibble <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/edibble)](https://CRAN.R-project.org/package=edibble)
<!-- badges: end -->

## Installation

You can install the package from CRAN:

``` r
install.packages("edibble")
```

You can install the development version with:

``` r
# install.packages("remotes")
remotes::install_github("emitanaka/edibble")
```

## Overview

The goal of `edibble` R-package is to aid in the plan, design and
simulation of experiments by mapping fundamental components of
experiments to an object oriented system. The `edibble` system is built
on the principle that the system must make it easy to recover
experimental context by encouraging the user to be explicit about
experimental details in fundamental terms.

## Examples

Consider an experiment where you want to know what is an effective way
of teaching (flipped or traditional style) for teaching a particular
subject and how different forms of exams (take-home, open-book or
closed-book) affect student’s marks.

There are four classes for this subject with each class holding 30
students. The teaching style can only be applied to the whole class but
exam can be different for individual students.

``` r
library(edibble)

set.seed(2020)

des <- design(name = "Effective teaching") %>%
    set_units(class = 4,
              student = nested_in(class, 30)) %>%
    set_trts(style = c("flipped", "traditional"),
             exam = c("take-home", "open-book", "closed-book")) %>%
    allot_trts(style ~ class,
               exam ~ student) %>%
    assign_trts("random")

serve_table(des)
#> # Effective teaching 
#> # An edibble: 120 x 4
#>        class     student       style        exam
#>    <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>
#>  1    class1   student1  traditional closed-book
#>  2    class1   student2  traditional open-book  
#>  3    class1   student3  traditional take-home  
#>  4    class1   student4  traditional closed-book
#>  5    class1   student5  traditional take-home  
#>  6    class1   student6  traditional take-home  
#>  7    class1   student7  traditional open-book  
#>  8    class1   student8  traditional open-book  
#>  9    class1   student9  traditional closed-book
#> 10    class1   student10 traditional closed-book
#> # … with 110 more rows
```

Before constructing the experiment, you might want to think about what
you are recording for which level of unit and what values these
variables can be recorded as.

``` r
out <- des %>% 
    set_rcrds_of(student = c("exam_mark",
                             "quiz1_mark",
                             "quiz2_mark",
                             "gender"),
              class = c("room",
                        "teacher")) %>%
    expect_rcrds(exam_mark <= 100,
                 exam_mark >= 0,
                 quiz1_mark <= 15L,
                 quiz1_mark >= 0L,
                 quiz2_mark <= 30L,
                 quiz2_mark >= 0L,
                 factor(gender, levels = c("female", "male", "non-binary", "unknown"))) %>%
    serve_table()

out
#> # Effective teaching 
#> # An edibble: 120 x 10
#>        class     student       style        exam exam_mark quiz1_mark quiz2_mark
#>    <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd>     <rcrd>     <rcrd>
#>  1    class1   student1  traditional closed-book         o          o          o
#>  2    class1   student2  traditional open-book           o          o          o
#>  3    class1   student3  traditional take-home           o          o          o
#>  4    class1   student4  traditional closed-book         o          o          o
#>  5    class1   student5  traditional take-home           o          o          o
#>  6    class1   student6  traditional take-home           o          o          o
#>  7    class1   student7  traditional open-book           o          o          o
#>  8    class1   student8  traditional open-book           o          o          o
#>  9    class1   student9  traditional closed-book         o          o          o
#> 10    class1   student10 traditional closed-book         o          o          o
#> # … with 110 more rows, and 3 more variables: gender <rcrd>, room <rcrd>,
#> #   teacher <rcrd>
```

When you export the above edibble design using the `export_design`
function, the variables you are recording are constraint to the values
you expect, e.g. for factors, the cells have a drop-down menu to select
from possible values.

``` r
export_design(out, file = "/PATH/TO/FILE.xlsx")
```

![](man/figures/README-excel_factor_output.png)<!-- -->

In addition, there is a spreadsheet for every observational level. E.g.
here `room` and `teacher` is the same for all students in one class so
rather than entering duplicate information, these are exported to
another sheet for data entry.

<img src="man/figures/README-excel_sheet_output.png" width="400px" />

There is also support for more complex nesting structures. You can
always make the structure using edibble and take the resulting data
frame to use in other experimental design software. It’s also possible
to bring existing data frame into edibble if you want to take advantage
of the exporting feature in edibble.

``` r
design("nesting structure") %>% 
  # there are 3 sites labelled A, B, C
  set_units(site = c("A", "B", "C"),
            # each site has 2 blocks except B with 3 sites
            block = nested_in(site, 
                              "B" ~ 3,
                                . ~ 2),
            # levels can be specified by their number instead
            # so for below "block1" has 30 plots, 
            # "block2" and "block3" has 40 plots,
            # the rest of blocks have 20 plots.
            plot = nested_in(block, 
                              1 ~ 30,
                        c(2, 3) ~ 40,
                              . ~ 20)) %>% 
  serve_table()
#> # nesting structure 
#> # An edibble: 190 x 3
#>         site     block        plot
#>    <unit(3)> <unit(7)> <unit(190)>
#>  1         A    block1      plot1 
#>  2         A    block1      plot2 
#>  3         A    block1      plot3 
#>  4         A    block1      plot4 
#>  5         A    block1      plot5 
#>  6         A    block1      plot6 
#>  7         A    block1      plot7 
#>  8         A    block1      plot8 
#>  9         A    block1      plot9 
#> 10         A    block1      plot10
#> # … with 180 more rows
```

## Experimental data

tidyverse is well suited for the data science project workflow as
illustrated below in (B) (from [Grolemund and Wickham
2017](https://r4ds.had.co.nz/introduction.html)). For experimental data,
the statistical aspect begins before obtaining data as depicted below in
(A). The focus of `edibble` is to facilitate work in (A).

<img src="man/figures/design-analysis-flow.png">

The edibble R-package differ considerably to other packages for
constructing experimental design with a focus on the whole process and
less on the randomisation process (which the other software generally
focus and do well on). Some features include:

-   declaratively create experimental designs based on experimental
    components (e.g. units and treatments),
-   explicitly specify variables that are to be recorded
    (e.g. response), and
-   set expected values for variables to be recorded which restrict the
    data entry when the design is exported as an xlsx file,
-   simulate values for record variables,
-   make classical named designs see [Cookbook
    chapter](https://emitanaka.org/edibble-book/cookbook.html).

Work-in-progress book on this package can be found
[here](https://emitanaka.org/edibble-book/).

## Limitations

Currently, edibble:

-   expects you to know the number of units available from the start.
    Unknown numbers will be supported in future versions.
-   in theory, edibble should support experiments that are not
    comparative experiments but this is not tested.
-   does not do enough testing so design should be diagnosed after
    construction (which should be done regardless of how much testing
    edibble implements).

## Related Work

The way that edibble specifies experimental design is largely novel (if
I say so myself) and there are no work that resembles it. I’m
concurrently working on two extension packages:

-   `deggust` - to visualise the designs constructed from edibble as
    ggplot2 objects (WIP).
-   `sizzled` - for experiments that require sample size calculation
    (WIP).

Below are some other related work. You can also have a look at the [CRAN
Task View for Design of Experiment and Analysis of Experimental
Data](https://CRAN.R-project.org/view=ExperimentalDesign) for a whole
collection.

-   `DeclareDesign` for survey or sampling designs
-   `designr` for balanced factorial designs with crossed and nested
    random and fixed effect to data frame
-   `dae` for functions useful in the design and ANOVA of experiments
    (this is in fact powering the randomisation in edibble)
-   `plotdesignr` for designing agronomic field experiments

## Acknowledgement

edibble is hugely inspired by the work of [Tidyverse
Team](https://joss.theoj.org/papers/10.21105/joss.01686). I’m grateful
for the dedication and work by the Tidyverse Team, as well as [R
Development Core Team](https://www.r-project.org/contributors.html) that
supports the core R ecosystem, that made developing this package
possible.

## Tidyverse familiarity

The implementation in edibble adopt a similar nomenclature and design
philosophy as tidyverse (and where it does not, it’s likely my
shortcoming) so that tidyverse users can leverage their familiarity of
the tidyverse language when using edibble. Specifically, edibble follows
the philosophy:

-   main functions do one thing and have a consistent form of
    `<verb>_<noun>` (e.g. `set_units` and `set_rcrds`) where the nouns
    are generally plural. Exceptions are when the subject matter is
    clearly singular (e.g. `design` and `set_context`);
-   pipable functions;
-   all dots arguments are [dynamic
    dots](https://rlang.r-lib.org/reference/dyn-dots.html);
-   duplicate names repaired with same option as `tibble` for additions
    to edibble graph;
-   ability for developers to extend certain components. Currently only
    supported for others to contribute their own classical named
    experimental designs via `prep_classical_`;
-   the specification of complex nested structure drawing similarity to
    `dplyr::case_when` (LHS is character or integer for edibble
    however).

## Code of Conduct

Please note that the edibble project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
