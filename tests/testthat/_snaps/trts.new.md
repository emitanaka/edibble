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
      [38;5;246m# An edibble design[39m 
      # An edibble: 30 x 3
          vaccine      sex     person
         <trt(3)> <trt(2)> <unit(30)>
       1 vaccine1     sex1   person1 
       2 vaccine2     sex1   person2 
       3 vaccine3     sex1   person3 
       4 vaccine1     sex2   person4 
       5 vaccine2     sex2   person5 
       6 vaccine3     sex2   person6 
       7 vaccine1     sex1   person7 
       8 vaccine2     sex1   person8 
       9 vaccine3     sex1   person9 
      10 vaccine1     sex2   person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(person = 30) %>%
        allot_trts(vaccine:sex ~ person) %>% assign_trts("systematic") %>%
        serve_table()
    Output
      # An edibble design 
      # An edibble: 30 x 3
          vaccine      sex     person
         <trt(3)> <trt(2)> <unit(30)>
       1 vaccine1        F   person1 
       2 vaccine2        F   person2 
       3 vaccine3        F   person3 
       4 vaccine1        M   person4 
       5 vaccine2        M   person5 
       6 vaccine3        M   person6 
       7 vaccine1        F   person7 
       8 vaccine2        F   person8 
       9 vaccine3        F   person9 
      10 vaccine1        M   person10
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3, sex = c("F", "M")) %>% set_units(person = 30) %>%
        allot_trts(vaccine ~ person, sex ~ person) %>% assign_trts("systematic") %>%
        serve_table()
    Output
      # An edibble design 
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
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 30) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("systematic") %>% serve_table()
    Output
      # An edibble design 
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
      # i 20 more rows

---

    Code
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("systematic-random", seed = 2) %>%
        serve_table()
    Output
      # An edibble design 
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
      design() %>% set_trts(vaccine = 3) %>% set_units(person = 5) %>% allot_trts(
        vaccine ~ person) %>% assign_trts("random", seed = 3) %>% serve_table()
    Output
      # An edibble design 
      # An edibble: 5 x 2
         vaccine    person
        <trt(3)> <unit(5)>
      1 vaccine3   person1
      2 vaccine2   person2
      3 vaccine2   person3
      4 vaccine1   person4
      5 vaccine3   person5

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ person) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       0        3        0        0        3        0        0
        vaccine2       0        0        0        3        0        3        0
        vaccine3       3        0        3        0        0        0        3
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        3        0        0        0       3        3       3       0
        vaccine2        0        0        3        3       0        0       0       3
        vaccine3        0        3        0        0       0        0       0       0
                
                 person5 person6 person7 person8 person9
        vaccine1       0       0       0       3       0
        vaccine2       3       3       0       0       0
        vaccine3       0       0       3       0       3

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2,
        constrain = NULL) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       1        2        0        1        2        2        0
        vaccine2       0        1        2        2        0        0        2
        vaccine3       2        0        1        0        1        1        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        0        2        3        0       1        1       1       1
        vaccine2        1        1        0        1       1        2       1       2
        vaccine3        2        0        0        2       1        0       1       0
                
                 person5 person6 person7 person8 person9
        vaccine1       1       2       0       0       0
        vaccine2       1       1       1       1       0
        vaccine3       1       0       2       2       3

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 3)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
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
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 2)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       1        0        1        1        1        0        1
        vaccine2       1        1        0        1        1        1        0
        vaccine3       0        1        1        0        0        1        1
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        0        1        1        1       1        0       0       0
        vaccine2        1        1        0        0       0        1       1       1
        vaccine3        1        0        1        1       1        1       1       1
                
                 person5 person6 person7 person8 person9
        vaccine1       1       1       0       1       1
        vaccine2       0       1       1       1       0
        vaccine3       1       0       1       0       1

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 8)) %>% allot_trts(vaccine ~ blood) %>% assign_trts("random", seed = 2) %>%
        serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       3        2        3        3        3        2        3
        vaccine2       3        3        2        3        3        3        2
        vaccine3       2        3        3        2        2        3        3
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        2        3        3        3       3        2       2       2
        vaccine2        3        3        2        2       2        3       3       3
        vaccine3        3        2        3        3       3        3       3       3
                
                 person5 person6 person7 person8 person9
        vaccine1       3       3       2       3       3
        vaccine2       2       3       3       3       2
        vaccine3       3       2       3       2       3

---

    Code
      tab <- design() %>% set_trts(vaccine = 3) %>% set_units(person = 20, blood = nested_in(
        person, 1 ~ 8, 2 ~ 3, . ~ 4)) %>% allot_trts(vaccine ~ blood) %>% assign_trts(
        "random", seed = 2) %>% serve_table()
      table(tab$vaccine, tab$person)
    Output
                
                 person1 person10 person11 person12 person13 person14 person15
        vaccine1       3        2        1        1        2        1        1
        vaccine2       3        1        2        1        1        2        1
        vaccine3       2        1        1        2        1        1        2
                
                 person16 person17 person18 person19 person2 person20 person3 person4
        vaccine1        2        1        1        2       1        1       1       2
        vaccine2        1        2        1        1       1        2       1       1
        vaccine3        1        1        2        1       1        1       2       1
                
                 person5 person6 person7 person8 person9
        vaccine1       1       1       2       1       1
        vaccine2       2       1       1       2       1
        vaccine3       1       2       1       1       2

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

