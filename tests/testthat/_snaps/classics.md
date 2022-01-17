# classics

    Code
      prep_classical_crd(n = 20, t = 3, seed = 3)
    Output
      start_design("Completely Randomised Design") %>%
        set_units(unit = 20) %>%
        set_trts(trt = 3) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_rcbd(r = 4, t = 3, seed = 3)
    Output
      start_design("Randomised Complete Block Design") %>%
        set_units(block = 4,
                  unit = nested_in(block, 3)) %>%
        set_trts(trt = 3) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_factorial(trt = c(3, 5, 6), r = 2, seed = 3)
    Output
      start_design("Factorial Design") %>%
        set_units(unit = 180) %>%
        set_trts(trt1 = 3,
                 trt2 = 5,
                 trt3 = 6) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_factorial(trt = c(3, 5, 6), r = 2, design = "rcbd", seed = 3)
    Output
      start_design("Factorial Design with RCBD structure") %>%
        set_units(block = 2,
                   unit = nested_in(block, 90)) %>%
        set_trts(trt1 = 3,
                 trt2 = 5,
                 trt3 = 6) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_factorial(trt = 4, r = 2, design = "rcbd", seed = 3)
    Output
      start_design("Factorial Design with RCBD structure") %>%
        set_units(block = 2,
                   unit = nested_in(block, 4)) %>%
        set_trts(trt1 = 4) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_split(t1 = 3, t2 = 2, r = 2, seed = 3)
    Output
      start_design("Split-Plot Design | Split-Unit Design") %>%
        set_units(mainplot = 6,
                   subplot = nested_in(mainplot, 2)) %>%
        set_trts(trt1 = 3,
                 trt2 = 2) %>%
        allot_trts(trt1 ~ mainplot,
                   trt2 ~ subplot) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      prep_classical_lsd(t = 5, seed = 3)
    Output
      start_design("Latin Square Design") %>%
        set_units(row = 5,
                  column = 5,
                  unit = ~row:column) %>%
        set_trts(trt = 5) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 3) %>%
        serve_table()

---

    Code
      find_classical_designs()
    Message <cliMessage>
      
      -- edibble --
      
      * crd with the arguments t, n, r, and seed for a Completely Randomised Design.
      * factorial with the arguments trt, r, design, and seed for a Factorial Design.
      * lsd with the arguments t and seed for a Latin Square Design.
      * rcbd with the arguments t, r, and seed for a Randomised Complete Block
      Design.
      * split with the arguments t1, t2, r, and seed for a Split-Plot Design or a
      Split-Unit Design.

---

    Code
      code_classical("crd", t = 4, n = 20, seed = 1)
    Output
      start_design("Completely Randomised Design") %>%
        set_units(unit = 20) %>%
        set_trts(trt = 4) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table()

---

    Code
      make_classical("rcbd", t = 4, r = 20, seed = 1)
    Message <cliMessage>
      
      -- experimental design details -------------------------------------------------
        * This experimental design is often called Randomised Complete Block Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code ----------------------------------------------------------------
    Output
      start_design("Randomised Complete Block Design") %>%
        set_units(block = 20,
                  unit = nested_in(block, 4)) %>%
        set_trts(trt = 4) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
    Message <cliMessage>
      
      -- edibble data frame ----------------------------------------------------------
    Output
      # Randomised Complete Block Design 
      # An edibble: 80 x 3
              block       unit      trt
         <unit(20)> <unit(80)> <trt(4)>
       1     block1     unit1      trt1
       2     block1     unit2      trt4
       3     block1     unit3      trt3
       4     block1     unit4      trt2
       5     block2     unit5      trt2
       6     block2     unit6      trt4
       7     block2     unit7      trt3
       8     block2     unit8      trt1
       9     block3     unit9      trt3
      10     block3     unit10     trt4
      # ... with 70 more rows

