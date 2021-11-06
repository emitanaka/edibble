# set_units

    Code
      start_design(name = "unlinked units") %>% set_units(block = 3, plot = 2)
    Output
      unlinked units
      +-block (3 levels)
      \-plot (2 levels)

---

    Code
      start_design() %>% set_units(row = 3, col = 4, plot = ~row:col) %>% serve_table()
    Output
      # An edibble: 12 x 3
               row       col       plot
         <unit(3)> <unit(4)> <unit(12)>
       1      row1      col1     plot1 
       2      row2      col1     plot2 
       3      row3      col1     plot3 
       4      row1      col2     plot4 
       5      row2      col2     plot5 
       6      row3      col2     plot6 
       7      row1      col3     plot7 
       8      row2      col3     plot8 
       9      row3      col3     plot9 
      10      row1      col4     plot10
      11      row2      col4     plot11
      12      row3      col4     plot12

---

    Code
      start_design() %>% set_units(row = 3, col = 4, site = 4, plot = ~site:row:col) %>%
        serve_table()
    Output
      # An edibble: 48 x 4
               row       col      site       plot
         <unit(3)> <unit(4)> <unit(4)> <unit(48)>
       1      row1      col1     site1     plot1 
       2      row1      col1     site2     plot2 
       3      row1      col1     site3     plot3 
       4      row1      col1     site4     plot4 
       5      row2      col1     site1     plot5 
       6      row2      col1     site2     plot6 
       7      row2      col1     site3     plot7 
       8      row2      col1     site4     plot8 
       9      row3      col1     site1     plot9 
      10      row3      col1     site2     plot10
      # ... with 38 more rows

---

    Code
      start_design() %>% set_units(site = 2, row = nested_in(site, 1 ~ 2, 2 ~ 3),
      col = nested_in(site, 3), plot = ~site:row:col) %>% serve_table()
    Output
      # An edibble: 60 x 4
               row       col       plot
         <unit(5)> <unit(6)> <unit(60)>
       1      row1      col1     plot1 
       2      row1      col1     plot2 
       3      row2      col1     plot3 
       4      row2      col1     plot4 
       5      row3      col1     plot5 
       6      row3      col1     plot6 
       7      row4      col1     plot7 
       8      row4      col1     plot8 
       9      row5      col1     plot9 
      10      row5      col1     plot10
      # ... with 50 more rows

