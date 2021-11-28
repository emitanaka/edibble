# simulate

    Code
      des %>% simulate_rcrds(response = sim_fixed(mean = ~30 + treat, effects = eff1))
    Output
      # An edibble: 12 x 3
            treat       unit response
         <trt(3)> <unit(12)>   <rcrd>
       1        A     unit1        33
       2        B     unit2        32
       3        A     unit3        33
       4        B     unit4        32
       5        C     unit5        30
       6        B     unit6        32
       7        C     unit7        30
       8        B     unit8        32
       9        C     unit9        30
      10        C     unit10       30
      11        A     unit11       33
      12        A     unit12       33
    Code
      des %>% simulate_rcrds(response = sim_fixed(mean = ~30 + treat, effects = eff2))
    Output
      # An edibble: 12 x 3
            treat       unit response
         <trt(3)> <unit(12)>   <rcrd>
       1        A     unit1        33
       2        B     unit2        30
       3        A     unit3        33
       4        B     unit4        30
       5        C     unit5        30
       6        B     unit6        30
       7        C     unit7        30
       8        B     unit8        30
       9        C     unit9        30
      10        C     unit10       30
      11        A     unit11       33
      12        A     unit12       33

---

    Code
      des %>% simulate_rcrds(response = sim_fixed(mean = ~30 + treat, effects = eff1))
    Output
      # An edibble: 12 x 3
            treat       unit response
         <trt(3)> <unit(12)>   <rcrd>
       1        A     unit1        33
       2        B     unit2        32
       3        A     unit3        33
       4        B     unit4        32
       5        C     unit5        30
       6        B     unit6        32
       7        C     unit7        30
       8        B     unit8        32
       9        C     unit9        30
      10        C     unit10       30
      11        A     unit11       33
      12        A     unit12       33
    Code
      des %>% simulate_rcrds(response = sim_fixed(mean = ~30 + treat, effects = eff2))
    Output
      # An edibble: 12 x 3
            treat       unit response
         <trt(3)> <unit(12)>   <rcrd>
       1        A     unit1        33
       2        B     unit2        30
       3        A     unit3        33
       4        B     unit4        30
       5        C     unit5        30
       6        B     unit6        30
       7        C     unit7        30
       8        B     unit8        30
       9        C     unit9        30
      10        C     unit10       30
      11        A     unit11       33
      12        A     unit12       33

