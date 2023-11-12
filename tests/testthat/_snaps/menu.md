# crd

    Code
      crd <- takeout(menu_crd(n = 24, t = 4, seed = 1))
      crd
    Output
      design("Completely Randomised Design") %>%
        set_units(unit = 24) %>%
        set_trts(trt = 4) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Completely Randomised Design 
      # An edibble: 24 x 2
            unit    trt
       * <U(24)> <T(4)>
           <chr>  <chr>
       1  unit01   trt4
       2  unit02   trt4
       3  unit03   trt3
       4  unit04   trt3
       5  unit05   trt4
       6  unit06   trt1
       7  unit07   trt3
       8  unit08   trt3
       9  unit09   trt1
      10  unit10   trt4
      # i 14 more rows

# rcbd

    Code
      rcbd <- takeout(menu_rcbd(r = 3, t = 5, seed = 1))
      rcbd
    Output
      design("Randomised Complete Block Design") %>%
        set_units(block = 3,
                  unit = nested_in(block, 5)) %>%
        set_trts(trt = 5) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Randomised Complete Block Design 
      # An edibble: 15 x 3
          block    unit    trt
       * <U(3)> <U(15)> <T(5)>
          <chr>   <chr>  <chr>
       1 block1  unit01   trt3
       2 block1  unit02   trt2
       3 block1  unit03   trt4
       4 block1  unit04   trt5
       5 block1  unit05   trt1
       6 block2  unit06   trt2
       7 block2  unit07   trt1
       8 block2  unit08   trt5
       9 block2  unit09   trt3
      10 block2  unit10   trt4
      11 block3  unit11   trt1
      12 block3  unit12   trt3
      13 block3  unit13   trt4
      14 block3  unit14   trt2
      15 block3  unit15   trt5

# split

    Code
      split <- takeout(menu_split(t1 = 3, t2 = 2, r = 2, seed = 1))
      split
    Output
      design("Split-Plot Design | Split-Unit Design") %>%
        set_units(mainplot = 6,
                   subplot = nested_in(mainplot, 2)) %>%
        set_trts(trt1 = 3,
                 trt2 = 2) %>%
        allot_trts(trt1 ~ mainplot,
                   trt2 ~ subplot) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Split-Plot Design | Split-Unit Design 
      # An edibble: 12 x 4
          mainplot   subplot   trt1   trt2
       *    <U(6)>   <U(12)> <T(3)> <T(2)>
             <chr>     <chr>  <chr>  <chr>
       1 mainplot1 subplot01  trt11  trt22
       2 mainplot1 subplot02  trt11  trt21
       3 mainplot2 subplot03  trt12  trt22
       4 mainplot2 subplot04  trt12  trt21
       5 mainplot3 subplot05  trt13  trt22
       6 mainplot3 subplot06  trt13  trt21
       7 mainplot4 subplot07  trt13  trt22
       8 mainplot4 subplot08  trt13  trt21
       9 mainplot5 subplot09  trt11  trt22
      10 mainplot5 subplot10  trt11  trt21
      11 mainplot6 subplot11  trt12  trt21
      12 mainplot6 subplot12  trt12  trt22

# strip

    Code
      strip <- takeout(menu_strip(t1 = 3, t2 = 2, r = 4, seed = 1))
      strip
    Output
      design("Strip-Plot Design | Strip-Unit Design") %>%
        set_units(block = 4,
                  row = nested_in(block, 3),
                  col = nested_in(block, 2),
                  unit = nested_in(block, crossed_by(row, col))) %>%
        set_trts(trt1 = 3,
                 trt2 = 2) %>%
        allot_trts(trt1 ~ row,
                   trt2 ~ col) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Strip-Plot Design | Strip-Unit Design 
      # An edibble: 24 x 6
          block     row    col    unit   trt1   trt2
       * <U(4)> <U(12)> <U(8)> <U(24)> <T(3)> <T(2)>
          <chr>   <chr>  <chr>   <chr>  <chr>  <chr>
       1 block1   row01   col1  unit01  trt12  trt21
       2 block1   row02   col1  unit02  trt13  trt21
       3 block1   row03   col1  unit03  trt11  trt21
       4 block1   row01   col2  unit04  trt12  trt22
       5 block1   row02   col2  unit05  trt13  trt22
       6 block1   row03   col2  unit06  trt11  trt22
       7 block2   row04   col3  unit07  trt12  trt22
       8 block2   row05   col3  unit08  trt13  trt22
       9 block2   row06   col3  unit09  trt11  trt22
      10 block2   row04   col4  unit10  trt12  trt21
      # i 14 more rows

# factorial

    Code
      fac_crd <- takeout(menu_factorial(trt = c(2, 3, 4), design = "crd", r = 2,
      seed = 1))
      fac_crd
    Output
      design("Factorial Design") %>%
        set_units(unit = 48) %>%
        set_trts(trt1 = 2,
                 trt2 = 3,
                 trt3 = 4) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Factorial Design 
      # An edibble: 48 x 4
            unit   trt1   trt2   trt3
       * <U(48)> <T(2)> <T(3)> <T(4)>
           <chr>  <chr>  <chr>  <chr>
       1  unit01  trt12  trt22  trt31
       2  unit02  trt12  trt21  trt32
       3  unit03  trt11  trt22  trt32
       4  unit04  trt11  trt23  trt31
       5  unit05  trt12  trt21  trt32
       6  unit06  trt12  trt22  trt32
       7  unit07  trt12  trt21  trt33
       8  unit08  trt12  trt22  trt33
       9  unit09  trt11  trt23  trt34
      10  unit10  trt11  trt22  trt31
      # i 38 more rows
    Code
      fac_rcbd <- takeout(menu_factorial(trt = c(2, 3, 4), design = "rcbd", r = 2,
      seed = 1))
      fac_rcbd
    Output
      design("Factorial Design with RCBD structure") %>%
        set_units(block = 2,
                   unit = nested_in(block, 24)) %>%
        set_trts(trt1 = 2,
                 trt2 = 3,
                 trt3 = 4) %>%
        allot_trts(~unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Factorial Design with RCBD structure 
      # An edibble: 48 x 5
          block    unit   trt1   trt2   trt3
       * <U(2)> <U(48)> <T(2)> <T(3)> <T(4)>
          <chr>   <chr>  <chr>  <chr>  <chr>
       1 block1  unit01  trt11  trt23  trt31
       2 block1  unit02  trt11  trt21  trt32
       3 block1  unit03  trt12  trt23  trt34
       4 block1  unit04  trt12  trt23  trt32
       5 block1  unit05  trt12  trt21  trt33
       6 block1  unit06  trt12  trt23  trt31
       7 block1  unit07  trt12  trt22  trt33
       8 block1  unit08  trt11  trt22  trt33
       9 block1  unit09  trt12  trt21  trt31
      10 block1  unit10  trt11  trt21  trt31
      # i 38 more rows

# lsd

    Code
      lsd <- takeout(menu_lsd(t = 10, seed = 1))
      lsd
    Output
      design("Latin Square Design") %>%
        set_units(row = 10,
                  col = 10,
                  unit = crossed_by(row, col)) %>%
        set_trts(trt = 10) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Latin Square Design 
      # An edibble: 100 x 4
             row     col     unit     trt
       * <U(10)> <U(10)> <U(100)> <T(10)>
           <chr>   <chr>    <chr>   <chr>
       1   row01   col01  unit001   trt03
       2   row02   col01  unit002   trt09
       3   row03   col01  unit003   trt07
       4   row04   col01  unit004   trt10
       5   row05   col01  unit005   trt04
       6   row06   col01  unit006   trt06
       7   row07   col01  unit007   trt01
       8   row08   col01  unit008   trt05
       9   row09   col01  unit009   trt02
      10   row10   col01  unit010   trt08
      # i 90 more rows

# youden

    Code
      youden <- takeout(menu_youden(nc = 7, t = 10, seed = 1))
      youden
    Output
      design("Youden Square Design") %>%
        set_units(row = 10,
                  col = 7,
                  unit = crossed_by(row, col)) %>%
        set_trts(trt = 10) %>%
        allot_trts(trt ~ unit) %>%
        assign_trts("random", seed = 1) %>%
        serve_table() 
      
      # Youden Square Design 
      # An edibble: 70 x 4
             row    col    unit     trt
       * <U(10)> <U(7)> <U(70)> <T(10)>
           <chr>  <chr>   <chr>   <chr>
       1   row01   col1  unit01   trt03
       2   row02   col1  unit02   trt09
       3   row03   col1  unit03   trt07
       4   row04   col1  unit04   trt10
       5   row05   col1  unit05   trt04
       6   row06   col1  unit06   trt06
       7   row07   col1  unit07   trt01
       8   row08   col1  unit08   trt05
       9   row09   col1  unit09   trt02
      10   row10   col1  unit10   trt08
      # i 60 more rows

