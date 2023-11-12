# set_units

    Code
      design(title = "unlinked units") %>% set_units(block = 3, plot = 2)
    Output
      An edibble design
      +-block (3 levels)
      \-plot (2 levels)

---

    Code
      design() %>% set_units(row = 3, col = 4, plot = ~ row:col) %>% serve_table()
    Output
      # An edibble: 12 x 3
            row    col    plot
         <U(3)> <U(4)> <U(12)>
          <chr>  <chr>   <chr>
       1   row1   col1  plot01
       2   row2   col1  plot02
       3   row3   col1  plot03
       4   row1   col2  plot04
       5   row2   col2  plot05
       6   row3   col2  plot06
       7   row1   col3  plot07
       8   row2   col3  plot08
       9   row3   col3  plot09
      10   row1   col4  plot10
      11   row2   col4  plot11
      12   row3   col4  plot12

---

    Code
      design() %>% set_units(row = 3, col = 4, site = 4, plot = ~ site:row:col) %>%
        serve_table()
    Output
      # An edibble: 48 x 4
            row    col   site    plot
         <U(3)> <U(4)> <U(4)> <U(48)>
          <chr>  <chr>  <chr>   <chr>
       1   row1   col1  site1  plot01
       2   row1   col1  site2  plot02
       3   row1   col1  site3  plot03
       4   row1   col1  site4  plot04
       5   row2   col1  site1  plot05
       6   row2   col1  site2  plot06
       7   row2   col1  site3  plot07
       8   row2   col1  site4  plot08
       9   row3   col1  site1  plot09
      10   row3   col1  site2  plot10
      # i 38 more rows

---

    Code
      design() %>% set_units(site = 2, row = nested_in(site, 1 ~ 2, 2 ~ 3), col = nested_in(
        site, 3), plot = ~ site:row:col) %>% serve_table()
    Output
      # An edibble: 60 x 4
           site    row    col    plot
         <U(2)> <U(5)> <U(6)> <U(60)>
          <chr>  <chr>  <chr>   <chr>
       1  site1   row1   col1  plot01
       2  site1   row1   col1  plot02
       3  site1   row2   col1  plot03
       4  site1   row2   col1  plot04
       5  site2   row3   col1  plot05
       6  site2   row3   col1  plot06
       7  site2   row4   col1  plot07
       8  site2   row4   col1  plot08
       9  site2   row5   col1  plot09
      10  site2   row5   col1  plot10
      # i 50 more rows

