# serve

    Code
      start_design(name = "unlinked units with table - the title cut-off seems to be only for testthat????????????????????? ?????? ??????") %>%
        set_units(block = 3, plot = 2) %>% serve_table()
    Output
      # unlinked units with table - the title cut-off seems to be only for testthat????????????????????? ?????? ?????? 
      # An edibble: 0 x 2
      # ... with 2 variables: block <unit(3)>, plot <unit(2)>

---

    Code
      start_design(name = "one unit") %>% set_units(block = 3) %>% serve_table()
    Output
      # one unit 
      # An edibble: 3 x 1
            block
        <unit(3)>
      1    block1
      2    block2
      3    block3

---

    Code
      start_design(name = "serve nested units") %>% set_units(block = 3, plot = nested_in(
        block, 2)) %>% serve_table()
    Output
      # serve nested units 
      # An edibble: 6 x 2
            block      plot
        <unit(3)> <unit(6)>
      1    block1     plot1
      2    block1     plot2
      3    block2     plot3
      4    block2     plot4
      5    block3     plot5
      6    block3     plot6

---

    Code
      start_design() %>% set_trts(vaccine = c("AZ", "M", "P")) %>% serve_table()
    Output
      # An edibble: 3 x 1
         vaccine
        <trt(3)>
      1       AZ
      2       M 
      3       P 

