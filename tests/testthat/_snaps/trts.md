# treatments

    Code
      start_design() %>% set_trts(vaccine = 2)
    Output
      An edibble design
      \-vaccine (2 levels)

---

    Code
      start_design() %>% set_trts(vaccine = 2, sex = 2)
    Output
      An edibble design
      +-vaccine (2 levels)
      \-sex (2 levels)

---

    Code
      start_design() %>% set_units(person = 5) %>% set_trts(vaccine = 2, sex = 2)
    Output
      An edibble design
      +-person (5 levels)
      +-vaccine (2 levels)
      \-sex (2 levels)

---

    Code
      start_design() %>% set_trts(vaccine = 2, sex = 2) %>% set_units(person = 5)
    Output
      An edibble design
      +-vaccine (2 levels)
      +-sex (2 levels)
      \-person (5 levels)

---

    Code
      start_design() %>% set_trts(vaccine = 3, sex = 2) %>% set_units(person = 30) %>%
        allot_trts(~person) %>% assign_trts("systematic") %>% serve_table()
    Output
      # An edibble: 30 x 3
          vaccine      sex     person
         <trt(3)> <trt(2)> <unit(30)>
       1 vaccine1     sex1   person1 
       2 vaccine1     sex2   person2 
       3 vaccine2     sex1   person3 
       4 vaccine2     sex2   person4 
       5 vaccine3     sex1   person5 
       6 vaccine3     sex2   person6 
       7 vaccine1     sex1   person7 
       8 vaccine1     sex2   person8 
       9 vaccine2     sex1   person9 
      10 vaccine2     sex2   person10
      # ... with 20 more rows

---

    Code
      start_design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(
        person = 30) %>% allot_trts(vaccine:sex ~ person) %>% assign_trts(
        "systematic") %>% serve_table()
    Output
      # An edibble: 30 x 3
          vaccine      sex     person
         <trt(3)> <trt(2)> <unit(30)>
       1 vaccine1        F   person1 
       2 vaccine1        M   person2 
       3 vaccine2        F   person3 
       4 vaccine2        M   person4 
       5 vaccine3        F   person5 
       6 vaccine3        M   person6 
       7 vaccine1        F   person7 
       8 vaccine1        M   person8 
       9 vaccine2        F   person9 
      10 vaccine2        M   person10
      # ... with 20 more rows

---

    Code
      start_design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(
        person = 30) %>% allot_trts(vaccine ~ person, sex ~ person) %>% assign_trts(
        "systematic") %>% serve_table()
    Output
      # An edibble: 30 x 3
          vaccine      sex     person
         <trt(3)> <trt(2)> <unit(30)>
       1 vaccine1        F   person1 
       2 vaccine2        M   person2 
       3 vaccine3        F   person3 
       4 vaccine1        M   person4 
       5 vaccine2        F   person5 
       6 vaccine3        M   person6 
       7 vaccine1        F   person7 
       8 vaccine2        M   person8 
       9 vaccine3        F   person9 
      10 vaccine1        M   person10
      # ... with 20 more rows

---

    Code
      start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 30) %>%
        allot_trts(vaccine ~ person) %>% assign_trts("systematic") %>% serve_table()
    Output
      # An edibble: 30 x 2
          vaccine     person
         <trt(3)> <unit(30)>
       1 vaccine1   person1 
       2 vaccine2   person2 
       3 vaccine3   person3 
       4 vaccine1   person4 
       5 vaccine2   person5 
       6 vaccine3   person6 
       7 vaccine1   person7 
       8 vaccine2   person8 
       9 vaccine3   person9 
      10 vaccine1   person10
      # ... with 20 more rows

---

    Code
      start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>%
        allot_trts(vaccine ~ person) %>% assign_trts("systematic-random", seed = 2) %>%
        serve_table()
    Output
      # An edibble: 5 x 2
         vaccine    person
        <trt(3)> <unit(5)>
      1 vaccine1   person1
      2 vaccine3   person2
      3 vaccine2   person3
      4 vaccine1   person4
      5 vaccine3   person5

---

    Code
      start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>%
        allot_trts(vaccine ~ person) %>% assign_trts("random", seed = 3) %>%
        serve_table()
    Output
      # An edibble: 5 x 2
         vaccine    person
        <trt(3)> <unit(5)>
      1 vaccine1   person1
      2 vaccine2   person2
      3 vaccine3   person3
      4 vaccine1   person4
      5 vaccine2   person5

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 3)) %>% allot_trts(vaccine ~ person) %>%
        assign_trts("random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       0        3        0        0        3        0        3
        vaccine2       3        0        3        0        0        3        0
        vaccine3       0        0        0        3        0        0        0
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        3        3        0        3       0        0       0       0
        vaccine2        0        0        0        0       0        0       0       3
        vaccine3        0        0        3        0       3        3       3       0
                
                 person5 person6 person7 person8 person9
        vaccine1       0       3       0       0       0
        vaccine2       3       0       3       0       0
        vaccine3       0       0       0       3       3

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2, constrain = NULL) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       0        1        1        2        1        2        2
        vaccine2       1        1        1        0        0        1        0
        vaccine3       2        1        1        1        2        0        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        0        1        2        1       0        1       0       1
        vaccine2        1        2        0        2       1        1       1       1
        vaccine3        2        0        1        0       2        1       2       1
                
                 person5 person6 person7 person8 person9
        vaccine1       2       0       1       0       2
        vaccine2       0       2       1       3       1
        vaccine3       1       1       1       0       0

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       1        1        1        1        1        1        1
        vaccine2       1        1        1        1        1        1        1
        vaccine3       1        1        1        1        1        1        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        1        1        1        1       1        1       1       1
        vaccine2        1        1        1        1       1        1       1       1
        vaccine3        1        1        1        1       1        1       1       1
                
                 person5 person6 person7 person8 person9
        vaccine1       1       1       1       1       1
        vaccine2       1       1       1       1       1
        vaccine3       1       1       1       1       1

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 2)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       1        0        0        1        1        1        1
        vaccine2       0        1        1        0        0        1        0
        vaccine3       1        1        1        1        1        0        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        0        0        1        1       0        1       1       1
        vaccine2        1        1        1        0       1        1       0       1
        vaccine3        1        1        0        1       1        0       1       0
                
                 person5 person6 person7 person8 person9
        vaccine1       1       1       0       1       0
        vaccine2       1       1       1       1       1
        vaccine3       0       0       1       0       1

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 8)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       3        2        2        3        3        3        3
        vaccine2       2        3        3        2        2        3        2
        vaccine3       3        3        3        3        3        2        3
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        2        2        3        3       2        3       3       3
        vaccine2        3        3        3        2       3        3       2       3
        vaccine3        3        3        2        3       3        2       3       2
                
                 person5 person6 person7 person8 person9
        vaccine1       3       3       2       3       2
        vaccine2       3       3       3       3       3
        vaccine3       2       2       3       2       3

---

    Code
      tab <- start_design() %>% set_trts(vaccine = 3) %>% set_units(person = 20,
        blood = nested_in(person, 1 ~ 8, 2 ~ 3, . ~ 4)) %>% allot_trts(vaccine ~
      blood) %>% assign_trts("random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       3        1        1        1        1        2        1
        vaccine2       2        1        1        2        2        1        2
        vaccine3       3        2        2        1        1        1        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        2        1        2        2       1        2       1       1
        vaccine2        1        1        1        1       1        1       1       1
        vaccine3        1        2        1        1       1        1       2       2
                
                 person5 person6 person7 person8 person9
        vaccine1       1       1       1       1       2
        vaccine2       2       2       1       2       1
        vaccine3       1       1       2       1       1

---

    Code
      tab <- start_design() %>% set_trts(fert = 8) %>% set_units(site = 10, plot = nested_in(
        site, 10), sample = nested_in(plot, 1 ~ 8, 2 ~ 3, . ~ 4)) %>% allot_trts(
        fert ~ sample) %>% assign_trts("random", seed = 2) %>% serve_table()
      table(tab$fert, tab$plot)
    Output
             
              plot1 plot10 plot100 plot11 plot12 plot13 plot14 plot15 plot16 plot17
        fert1     1      1       1      0      1      1      1      0      1      1
        fert2     1      1       1      0      0      0      0      1      0      1
        fert3     1      0       1      1      0      1      1      1      1      0
        fert4     1      1       0      1      1      1      0      0      0      0
        fert5     1      0       0      0      0      0      1      0      0      0
        fert6     1      0       0      0      1      0      1      0      1      0
        fert7     1      1       1      1      0      0      0      1      0      1
        fert8     1      0       0      1      1      1      0      1      1      1
             
              plot18 plot19 plot2 plot20 plot21 plot22 plot23 plot24 plot25 plot26
        fert1      0      0     1      0      1      0      0      1      0      0
        fert2      1      0     0      1      0      1      0      0      0      0
        fert3      0      1     0      0      1      0      1      0      0      1
        fert4      1      1     0      0      1      0      0      1      1      0
        fert5      0      1     1      1      0      1      1      0      1      1
        fert6      1      0     0      1      0      0      1      1      1      1
        fert7      0      1     0      1      1      1      1      1      1      0
        fert8      1      0     1      0      0      1      0      0      0      1
             
              plot27 plot28 plot29 plot3 plot30 plot31 plot32 plot33 plot34 plot35
        fert1      0      1      0     0      0      1      1      1      0      1
        fert2      0      1      1     0      1      1      0      1      1      1
        fert3      1      0      0     1      0      1      1      1      1      0
        fert4      1      0      1     1      1      1      1      0      1      0
        fert5      1      1      1     1      1      0      1      0      0      0
        fert6      1      0      0     0      1      0      0      0      0      1
        fert7      0      1      0     0      0      0      0      1      0      1
        fert8      0      0      1     1      0      0      0      0      1      0
             
              plot36 plot37 plot38 plot39 plot4 plot40 plot41 plot42 plot43 plot44
        fert1      0      0      0      1     1      0      1      0      0      0
        fert2      1      1      1      1     0      0      1      1      1      0
        fert3      1      1      0      1     1      1      1      0      1      1
        fert4      0      1      1      0     0      0      0      1      0      1
        fert5      1      0      0      0     0      1      0      1      0      0
        fert6      0      0      1      1     0      0      0      0      1      1
        fert7      0      1      0      0     1      1      0      1      1      1
        fert8      1      0      1      0     1      1      1      0      0      0
             
              plot45 plot46 plot47 plot48 plot49 plot5 plot50 plot51 plot52 plot53
        fert1      1      0      1      0      1     1      0      0      1      1
        fert2      1      1      1      0      0     0      1      0      0      1
        fert3      0      1      1      0      1     1      1      0      1      1
        fert4      1      1      0      1      0     0      1      1      0      1
        fert5      0      1      1      1      0     0      0      0      1      0
        fert6      0      0      0      1      1     1      1      1      0      0
        fert7      0      0      0      0      0     1      0      1      1      0
        fert8      1      0      0      1      1     0      0      1      0      0
             
              plot54 plot55 plot56 plot57 plot58 plot59 plot6 plot60 plot61 plot62
        fert1      1      0      1      1      0      1     1      0      1      1
        fert2      0      1      0      1      0      0     1      1      0      1
        fert3      1      1      1      0      1      1     0      0      0      0
        fert4      0      0      0      0      0      0     0      0      0      1
        fert5      1      0      0      1      1      1     0      1      1      0
        fert6      1      1      1      0      1      0     1      1      0      1
        fert7      0      1      1      0      0      0     1      0      1      0
        fert8      0      0      0      1      1      1     0      1      1      0
             
              plot63 plot64 plot65 plot66 plot67 plot68 plot69 plot7 plot70 plot71
        fert1      1      0      1      1      0      0      1     1      1      0
        fert2      0      1      0      1      0      0      1     0      0      0
        fert3      0      1      0      0      1      0      0     1      1      1
        fert4      1      0      0      1      1      1      1     0      0      0
        fert5      0      0      1      0      0      1      0     1      1      1
        fert6      0      1      1      1      1      1      0     0      0      1
        fert7      1      0      1      0      0      0      0     0      1      1
        fert8      1      1      0      0      1      1      1     1      0      0
             
              plot72 plot73 plot74 plot75 plot76 plot77 plot78 plot79 plot8 plot80
        fert1      0      1      0      1      1      1      0      1     0      1
        fert2      0      0      1      1      1      0      0      0     1      0
        fert3      1      0      0      0      0      0      1      1     0      0
        fert4      0      1      1      0      0      1      0      0     1      0
        fert5      0      1      0      1      0      0      1      0     1      1
        fert6      1      0      0      1      1      1      0      0     0      1
        fert7      1      0      1      0      0      1      1      1     0      0
        fert8      1      1      1      0      1      0      1      1     1      1
             
              plot81 plot82 plot83 plot84 plot85 plot86 plot87 plot88 plot89 plot9
        fert1      0      0      0      0      0      1      1      1      0     1
        fert2      1      1      0      0      1      0      0      1      0     1
        fert3      1      1      0      0      0      0      0      1      0     0
        fert4      1      0      1      0      1      0      1      0      0     1
        fert5      0      1      1      1      1      0      1      0      1     1
        fert6      0      1      0      1      1      1      0      0      1     0
        fert7      1      0      1      1      0      1      1      0      1     0
        fert8      0      0      1      1      0      1      0      1      1     0
             
              plot90 plot91 plot92 plot93 plot94 plot95 plot96 plot97 plot98 plot99
        fert1      0      0      0      0      1      1      1      0      1      1
        fert2      1      1      0      1      0      0      0      1      1      0
        fert3      1      0      1      1      0      1      1      0      0      0
        fert4      0      0      1      1      1      1      1      1      0      1
        fert5      1      0      0      0      1      0      0      0      1      1
        fert6      0      1      0      1      1      1      0      1      0      0
        fert7      1      1      1      0      0      0      0      1      1      0
        fert8      0      1      1      0      0      0      1      0      0      1

---

    Code
      tab <- start_design() %>% set_trts(fert = 2, irr = 2) %>% set_units(block = 10,
        wplot = nested_in(block, 3), splot = nested_in(wplot, 4)) %>% allot_trts(
        fert ~ splot, irr ~ wplot) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$fert, tab$irr, tab$wplot)
    Output
      , ,  = wplot1
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot10
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot11
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot12
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot13
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot14
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot15
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot16
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot17
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot18
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot19
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot2
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot20
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot21
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot22
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot23
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot24
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot25
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot26
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot27
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot28
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot29
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot3
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot30
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot4
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot5
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot6
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot7
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot8
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot9
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      

