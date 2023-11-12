# treatments

    Code
      design(seed = 1) %>% set_trts(vaccine = 2)
    Output
      An edibble design
      \-vaccine (2 levels)

---

    Code
      design(seed = 1) %>% set_trts(vaccine = 2, sex = 2)
    Output
      An edibble design
      +-vaccine (2 levels)
      \-sex (2 levels)

---

    Code
      design(seed = 1) %>% set_units(person = 5) %>% set_trts(vaccine = 2, sex = 2)
    Output
      An edibble design
      +-person (5 levels)
      +-vaccine (2 levels)
      \-sex (2 levels)

---

    Code
      design(seed = 1) %>% set_trts(vaccine = 2, sex = 2) %>% set_units(person = 5)
    Output
      An edibble design
      +-vaccine (2 levels)
      +-sex (2 levels)
      \-person (5 levels)

---

    Code
      design() %>% set_trts(vaccine = 3, sex = 2) %>% set_units(person = 30) %>%
        allot_trts(~person) %>% assign_trts("systematic") %>% serve_table()
    Output
      # An edibble: 30 x 3
          vaccine    sex   person
           <T(3)> <T(2)>  <U(30)>
            <chr>  <chr>    <chr>
       1 vaccine1   sex1 person01
       2 vaccine2   sex1 person02
       3 vaccine3   sex1 person03
       4 vaccine1   sex2 person04
       5 vaccine2   sex2 person05
       6 vaccine3   sex2 person06
       7 vaccine1   sex1 person07
       8 vaccine2   sex1 person08
       9 vaccine3   sex1 person09
      10 vaccine1   sex2 person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(person = 30) %>%
        allot_trts(vaccine:sex ~ person) %>% assign_trts("systematic") %>%
        serve_table()
    Output
      # An edibble: 30 x 3
          vaccine    sex   person
           <T(3)> <T(2)>  <U(30)>
            <chr>  <chr>    <chr>
       1 vaccine1      F person01
       2 vaccine2      F person02
       3 vaccine3      F person03
       4 vaccine1      M person04
       5 vaccine2      M person05
       6 vaccine3      M person06
       7 vaccine1      F person07
       8 vaccine2      F person08
       9 vaccine3      F person09
      10 vaccine1      M person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(person = 30) %>%
        allot_trts(vaccine ~ person, sex ~ person) %>% assign_trts("systematic") %>%
        serve_table()
    Output
      # An edibble: 30 x 3
          vaccine    sex   person
           <T(3)> <T(2)>  <U(30)>
            <chr>  <chr>    <chr>
       1 vaccine1      F person01
       2 vaccine2      M person02
       3 vaccine3      F person03
       4 vaccine1      M person04
       5 vaccine2      F person05
       6 vaccine3      M person06
       7 vaccine1      F person07
       8 vaccine2      M person08
       9 vaccine3      F person09
      10 vaccine1      M person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 30) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("systematic") %>% serve_table()
    Output
      # An edibble: 30 x 2
          vaccine   person
           <T(3)>  <U(30)>
            <chr>    <chr>
       1 vaccine1 person01
       2 vaccine2 person02
       3 vaccine3 person03
       4 vaccine1 person04
       5 vaccine2 person05
       6 vaccine3 person06
       7 vaccine1 person07
       8 vaccine2 person08
       9 vaccine3 person09
      10 vaccine1 person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("systematic-random", seed = 2) %>%
        serve_table()
    Output
      # An edibble: 5 x 2
         vaccine  person
          <T(3)>  <U(5)>
           <chr>   <chr>
      1 vaccine1 person1
      2 vaccine3 person2
      3 vaccine2 person3
      4 vaccine1 person4
      5 vaccine3 person5

---

    Code
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("random", seed = 3) %>% serve_table()
    Output
      # An edibble: 5 x 2
         vaccine  person
          <T(3)>  <U(5)>
           <chr>   <chr>
      1 vaccine1 person1
      2 vaccine2 person2
      3 vaccine3 person3
      4 vaccine1 person4
      5 vaccine2 person5

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ person) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        0        0        0        0        0        3        0
        vaccine2        3        0        0        3        3        0        3
        vaccine3        0        3        3        0        0        0        0
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        0        0        3        0        0        3        0
        vaccine2        0        0        0        3        0        0        3
        vaccine3        3        3        0        0        3        0        0
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        3        3        3        0        3        0
        vaccine2        0        0        0        0        0        0
        vaccine3        0        0        0        3        0        3

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2,
        constrain = NULL) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        0        0        0        1        2        0        1
        vaccine2        1        1        1        1        0        2        1
        vaccine3        2        2        2        1        1        1        1
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        0        2        1        1        2        1        2
        vaccine2        3        1        1        1        0        0        1
        vaccine3        0        0        1        1        1        2        0
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        2        0        1        2        1        1
        vaccine2        0        1        2        0        2        1
        vaccine3        1        2        0        1        0        1

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        1        1        1        1        1        1        1
        vaccine2        1        1        1        1        1        1        1
        vaccine3        1        1        1        1        1        1        1
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        1        1        1        1        1        1        1
        vaccine2        1        1        1        1        1        1        1
        vaccine3        1        1        1        1        1        1        1
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        1        1        1        1        1        1
        vaccine2        1        1        1        1        1        1
        vaccine3        1        1        1        1        1        1

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 2)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        1        1        0        0        1        1        0
        vaccine2        1        0        1        1        0        1        1
        vaccine3        0        1        1        1        1        0        1
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        1        1        0        1        1        1        0
        vaccine2        1        0        1        0        1        1        1
        vaccine3        0        1        1        1        0        0        1
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        1        0        1        1        1        0
        vaccine2        0        1        1        0        0        1
        vaccine3        1        1        0        1        1        1

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 8)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        3        3        2        2        3        3        2
        vaccine2        3        2        3        3        2        3        3
        vaccine3        2        3        3        3        3        2        3
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        3        3        2        3        3        3        2
        vaccine2        3        2        3        2        3        3        3
        vaccine3        2        3        3        3        2        2        3
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        3        2        3        3        3        2
        vaccine2        2        3        3        2        2        3
        vaccine3        3        3        2        3        3        3

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 1 ~ 8, 2 ~ 3, . ~ 4)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person01 person02 person03 person04 person05 person06 person07
        vaccine1        3        1        1        2        1        1        2
        vaccine2        3        1        1        1        2        1        1
        vaccine3        2        1        2        1        1        2        1
                
                 person08 person09 person10 person11 person12 person13 person14
        vaccine1        1        1        2        1        1        2        1
        vaccine2        2        1        1        2        1        1        2
        vaccine3        1        2        1        1        2        1        1
                
                 person15 person16 person17 person18 person19 person20
        vaccine1        1        2        1        1        2        1
        vaccine2        1        1        2        1        1        2
        vaccine3        2        1        1        2        1        1

---

    Code
      tab <- design() %>% set_trts(fert = 8) %>% set_units(site = 10, plot = nested_in(
        site, 10), sample = nested_in(plot, 1 ~ 8, 2 ~ 3, . ~ 4)) %>% allot_trts(
        fert ~ sample) %>% assign_trts("random", seed = 2) %>% serve_table()
      table(tab$fert, tab$plot)
    Output
             
              plot001 plot002 plot003 plot004 plot005 plot006 plot007 plot008 plot009
        fert1       1       1       0       0       0       0       1       1       1
        fert2       1       0       1       1       1       0       0       1       0
        fert3       1       0       0       1       1       0       0       1       1
        fert4       1       1       1       0       0       1       0       0       1
        fert5       1       0       0       0       0       1       1       0       0
        fert6       1       1       1       0       1       1       1       0       1
        fert7       1       0       0       1       0       0       1       1       0
        fert8       1       0       1       1       1       1       0       0       0
             
              plot010 plot011 plot012 plot013 plot014 plot015 plot016 plot017 plot018
        fert1       1       1       1       0       1       0       0       0       1
        fert2       0       0       1       1       1       0       1       0       1
        fert3       1       0       0       0       0       1       0       0       1
        fert4       0       1       1       0       1       0       1       1       0
        fert5       1       0       0       1       0       1       1       1       0
        fert6       1       1       0       0       0       1       1       1       0
        fert7       0       1       1       1       1       1       0       0       0
        fert8       0       0       0       1       0       0       0       1       1
             
              plot019 plot020 plot021 plot022 plot023 plot024 plot025 plot026 plot027
        fert1       0       1       1       0       1       0       1       1       0
        fert2       0       1       0       0       1       0       1       0       1
        fert3       0       1       1       0       0       1       1       0       1
        fert4       1       0       1       0       1       1       0       0       0
        fert5       0       0       0       1       0       0       0       1       1
        fert6       1       1       1       1       0       0       0       1       0
        fert7       1       0       0       1       1       1       1       1       0
        fert8       1       0       0       1       0       1       0       0       1
             
              plot028 plot029 plot030 plot031 plot032 plot033 plot034 plot035 plot036
        fert1       1       1       1       0       0       1       0       0       1
        fert2       1       0       1       0       1       0       1       1       0
        fert3       1       0       0       1       1       0       1       0       0
        fert4       1       1       0       1       0       0       1       1       0
        fert5       0       1       0       0       1       1       1       0       1
        fert6       0       0       1       0       1       0       0       0       0
        fert7       0       1       0       1       0       1       0       1       1
        fert8       0       0       1       1       0       1       0       1       1
             
              plot037 plot038 plot039 plot040 plot041 plot042 plot043 plot044 plot045
        fert1       1       0       1       0       1       0       1       1       0
        fert2       1       1       0       1       0       1       0       1       0
        fert3       1       1       0       1       0       0       1       0       1
        fert4       0       1       0       0       1       0       1       1       1
        fert5       0       1       0       0       1       0       0       1       0
        fert6       1       0       1       0       1       1       0       0       0
        fert7       0       0       1       1       0       1       0       0       1
        fert8       0       0       1       1       0       1       1       0       1
             
              plot046 plot047 plot048 plot049 plot050 plot051 plot052 plot053 plot054
        fert1       0       1       0       0       1       0       1       0       1
        fert2       1       0       0       0       1       1       0       1       1
        fert3       0       1       1       0       0       0       1       1       0
        fert4       0       1       0       1       0       1       1       0       0
        fert5       1       1       1       1       0       1       0       1       0
        fert6       1       0       0       1       1       0       0       1       1
        fert7       0       0       1       0       1       0       1       0       0
        fert8       1       0       1       1       0       1       0       0       1
             
              plot055 plot056 plot057 plot058 plot059 plot060 plot061 plot062 plot063
        fert1       0       0       1       0       1       1       0       0       1
        fert2       0       1       0       0       0       0       1       0       0
        fert3       1       0       1       1       0       0       1       1       1
        fert4       1       0       0       0       0       1       0       1       0
        fert5       0       1       1       1       0       1       0       0       1
        fert6       0       0       1       1       1       0       1       1       0
        fert7       1       1       0       1       1       0       1       1       0
        fert8       1       1       0       0       1       1       0       0       1
             
              plot064 plot065 plot066 plot067 plot068 plot069 plot070 plot071 plot072
        fert1       0       1       1       0       0       0       1       1       0
        fert2       1       0       0       1       1       0       1       0       0
        fert3       0       1       0       1       0       1       0       0       1
        fert4       1       1       1       0       0       1       0       1       0
        fert5       0       0       1       1       1       0       0       1       0
        fert6       1       0       0       0       1       1       1       0       1
        fert7       1       0       0       1       1       0       0       1       1
        fert8       0       1       1       0       0       1       1       0       1
             
              plot073 plot074 plot075 plot076 plot077 plot078 plot079 plot080 plot081
        fert1       1       0       1       1       0       0       1       1       0
        fert2       0       1       1       1       0       1       0       1       1
        fert3       1       0       1       0       1       0       1       0       1
        fert4       0       1       0       1       1       1       0       0       1
        fert5       1       1       0       1       0       1       0       1       0
        fert6       0       0       0       0       1       1       1       0       1
        fert7       0       1       0       0       1       0       0       1       0
        fert8       1       0       1       0       0       0       1       0       0
             
              plot082 plot083 plot084 plot085 plot086 plot087 plot088 plot089 plot090
        fert1       1       0       0       1       0       0       1       1       0
        fert2       1       0       1       0       1       0       0       0       1
        fert3       0       1       1       0       0       0       1       1       1
        fert4       0       1       1       0       1       1       1       0       0
        fert5       0       1       0       1       1       0       0       1       1
        fert6       0       0       1       1       0       1       0       0       0
        fert7       1       0       0       1       0       1       1       0       1
        fert8       1       1       0       0       1       1       0       1       0
             
              plot091 plot092 plot093 plot094 plot095 plot096 plot097 plot098 plot099
        fert1       1       1       0       0       0       1       0       0       1
        fert2       0       1       1       0       0       1       1       0       1
        fert3       0       0       0       1       1       1       0       1       0
        fert4       0       0       1       0       1       0       1       0       1
        fert5       1       0       1       1       1       1       0       1       0
        fert6       1       1       0       0       1       0       1       1       0
        fert7       0       0       1       1       0       0       0       1       0
        fert8       1       1       0       1       0       0       1       0       1
             
              plot100
        fert1       1
        fert2       0
        fert3       0
        fert4       1
        fert5       0
        fert6       1
        fert7       1
        fert8       0

---

    Code
      tab <- design() %>% set_trts(fert = 2, irr = 2) %>% set_units(block = 10,
        wplot = nested_in(block, 3), splot = nested_in(wplot, 4)) %>% allot_trts(
        fert ~ splot, irr ~ wplot) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$fert, tab$irr, tab$wplot)
    Output
      , ,  = wplot01
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot02
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot03
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot04
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot05
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot06
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot07
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot08
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot09
      
             
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
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot14
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot15
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot16
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot17
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot18
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot19
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot20
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot21
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot22
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot23
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot24
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot25
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot26
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      
      , ,  = wplot27
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot28
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot29
      
             
              irr1 irr2
        fert1    2    0
        fert2    2    0
      
      , ,  = wplot30
      
             
              irr1 irr2
        fert1    0    2
        fert2    0    2
      

