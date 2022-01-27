# crd

    Code
      crd <- takeout(menu_crd(n = 24, t = 4, seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Completely Randomised Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code --
      
    Output
      start_design("Completely Randomised Design") %>%
        set_units(unit = 24) %>%
        set_trts(trt = 4) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Completely Randomised Design 
      # An edibble: 24 x 2
               unit      trt
         <unit(24)> <trt(4)>
       1     unit1      trt4
       2     unit2      trt4
       3     unit3      trt3
       4     unit4      trt3
       5     unit5      trt4
       6     unit6      trt1
       7     unit7      trt3
       8     unit8      trt3
       9     unit9      trt1
      10     unit10     trt4
      # ... with 14 more rows

# rcbd

    Code
      rcbd <- takeout(menu_rcbd(r = 3, t = 5, seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Randomised Complete Block Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code --
      
    Output
      start_design("Randomised Complete Block Design") %>%
        set_units(block = 3,
                  unit = nested_in(block, 5)) %>%
        set_trts(trt = 5) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Randomised Complete Block Design 
      # An edibble: 15 x 3
             block       unit      trt
         <unit(3)> <unit(15)> <trt(5)>
       1    block1     unit1      trt5
       2    block1     unit2      trt3
       3    block1     unit3      trt4
       4    block1     unit4      trt2
       5    block1     unit5      trt1
       6    block2     unit6      trt3
       7    block2     unit7      trt5
       8    block2     unit8      trt1
       9    block2     unit9      trt4
      10    block2     unit10     trt2
      11    block3     unit11     trt2
      12    block3     unit12     trt5
      13    block3     unit13     trt4
      14    block3     unit14     trt3
      15    block3     unit15     trt1

# split

    Code
      split <- takeout(menu_split(t1 = 3, t2 = 2, r = 2, seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Split-Plot Design or Split-Unit
        Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
        * This design is balanced.
      
      -- edibble code --
      
    Output
      start_design("Split-Plot Design | Split-Unit Design") %>%
        set_units(mainplot = 6,
                   subplot = nested_in(mainplot, 2)) %>%
        set_trts(trt1 = 3,
                 trt2 = 2) %>%
        allot_trts(trt1 ~ mainplot,
                   trt2 ~ subplot) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Split-Plot Design | Split-Unit Design 
      # An edibble: 12 x 4
          mainplot    subplot     trt1     trt2
         <unit(6)> <unit(12)> <trt(3)> <trt(2)>
       1 mainplot1  subplot1     trt11    trt22
       2 mainplot1  subplot2     trt11    trt21
       3 mainplot2  subplot3     trt12    trt22
       4 mainplot2  subplot4     trt12    trt21
       5 mainplot3  subplot5     trt13    trt21
       6 mainplot3  subplot6     trt13    trt22
       7 mainplot4  subplot7     trt13    trt21
       8 mainplot4  subplot8     trt13    trt22
       9 mainplot5  subplot9     trt11    trt21
      10 mainplot5  subplot10    trt11    trt22
      11 mainplot6  subplot11    trt12    trt22
      12 mainplot6  subplot12    trt12    trt21

# factorial

    Code
      fac_crd <- takeout(menu_factorial(trt = c(2, 3, 4), design = "crd", r = 2,
      seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Factorial Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code --
      
    Output
      start_design("Factorial Design") %>%
        set_units(unit = 48) %>%
        set_trts(trt1 = 2,
                 trt2 = 3,
                 trt3 = 4) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Factorial Design 
      # An edibble: 48 x 4
               unit     trt1     trt2     trt3
         <unit(48)> <trt(2)> <trt(3)> <trt(4)>
       1     unit1     trt12    trt22    trt31
       2     unit2     trt12    trt21    trt32
       3     unit3     trt11    trt22    trt32
       4     unit4     trt11    trt23    trt31
       5     unit5     trt12    trt21    trt32
       6     unit6     trt12    trt22    trt32
       7     unit7     trt12    trt21    trt33
       8     unit8     trt12    trt22    trt33
       9     unit9     trt11    trt23    trt34
      10     unit10    trt11    trt22    trt31
      # ... with 38 more rows
    Code
      fac_rcbd <- takeout(menu_factorial(trt = c(2, 3, 4), design = "rcbd", r = 2,
      seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Factorial Design with RCBD
        structure.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code --
      
    Output
      start_design("Factorial Design with RCBD structure") %>%
        set_units(block = 2,
                   unit = nested_in(block, 24)) %>%
        set_trts(trt1 = 2,
                 trt2 = 3,
                 trt3 = 4) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Factorial Design with RCBD structure 
      # An edibble: 48 x 5
             block       unit     trt1     trt2     trt3
         <unit(2)> <unit(48)> <trt(2)> <trt(3)> <trt(4)>
       1    block1     unit1     trt11    trt21    trt31
       2    block1     unit2     trt12    trt21    trt31
       3    block1     unit3     trt11    trt23    trt32
       4    block1     unit4     trt12    trt21    trt33
       5    block1     unit5     trt12    trt23    trt33
       6    block1     unit6     trt11    trt21    trt34
       7    block1     unit7     trt12    trt23    trt34
       8    block1     unit8     trt12    trt22    trt32
       9    block1     unit9     trt12    trt23    trt31
      10    block1     unit10    trt11    trt22    trt34
      # ... with 38 more rows

# lsd

    Code
      lsd <- takeout(menu_lsd(t = 10, seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Latin Square Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
        * This design is balanced.
      
      -- edibble code --
      
    Output
      start_design("Latin Square Design") %>%
        set_units(row = 10,
                  column = 10,
                  unit = ~row:column) %>%
        set_trts(trt = 10) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Latin Square Design 
      # An edibble: 100 x 4
                row     column        unit       trt
         <unit(10)> <unit(10)> <unit(100)> <trt(10)>
       1      row1     column1      unit1      trt3 
       2      row2     column1      unit2      trt9 
       3      row3     column1      unit3      trt7 
       4      row4     column1      unit4      trt10
       5      row5     column1      unit5      trt4 
       6      row6     column1      unit6      trt6 
       7      row7     column1      unit7      trt1 
       8      row8     column1      unit8      trt5 
       9      row9     column1      unit9      trt2 
      10      row10    column1      unit10     trt8 
      # ... with 90 more rows

# youden

    Code
      youden <- takeout(menu_youden(nc = 7, t = 10, seed = 1))
    Message <cliMessage>
      -- experimental design details --
      
        * This experimental design is often called Youden Square Design.
        * You can change the number in `seed` to get another random instance of the
        same design.
      
      -- edibble code --
      
    Output
      start_design("Youden Square Design") %>%
        set_units(row = 10,
                  column = 7,
                  unit = ~row:column) %>%
        set_trts(trt = 10) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
    Message <cliMessage>
      -- edibble data frame --
      
    Output
      # Youden Square Design 
      # An edibble: 70 x 4
                row    column       unit       trt
         <unit(10)> <unit(7)> <unit(70)> <trt(10)>
       1      row1    column1     unit1      trt3 
       2      row2    column1     unit2      trt9 
       3      row3    column1     unit3      trt7 
       4      row4    column1     unit4      trt10
       5      row5    column1     unit5      trt4 
       6      row6    column1     unit6      trt6 
       7      row7    column1     unit7      trt1 
       8      row8    column1     unit8      trt5 
       9      row9    column1     unit9      trt2 
      10      row10   column1     unit10     trt8 
      # ... with 60 more rows

