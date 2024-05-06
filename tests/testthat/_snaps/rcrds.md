# measure response

    Code
      des0 %>% set_rcrds(exam_mark = student, room = class) %>% serve_table()
    Output
      # Effective teaching 
      # An edibble: 120 x 6
          class    student   style        exam exam_mark   room
         <U(4)>   <U(120)>  <T(2)>      <T(3)>  <R(120)> <R(4)>
          <chr>      <chr>   <chr>       <chr>     <dbl>  <dbl>
       1 class1 student001 flipped closed-book         o      o
       2 class1 student002 flipped   take-home         o      x
       3 class1 student003 flipped   open-book         o      x
       4 class1 student004 flipped closed-book         o      x
       5 class1 student005 flipped closed-book         o      x
       6 class1 student006 flipped   open-book         o      x
       7 class1 student007 flipped closed-book         o      x
       8 class1 student008 flipped   open-book         o      x
       9 class1 student009 flipped   take-home         o      x
      10 class1 student010 flipped   take-home         o      x
      # i 110 more rows

---

    Code
      des1 <- des0 %>% set_rcrds(exam_mark = student, room = class)
      des1
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   \-exam_mark
      +-style (2 levels)
      \-room

---

    Code
      serve_table(des1)
    Output
      # Effective teaching 
      # An edibble: 120 x 6
          class    student   style        exam exam_mark   room
         <U(4)>   <U(120)>  <T(2)>      <T(3)>  <R(120)> <R(4)>
          <chr>      <chr>   <chr>       <chr>     <dbl>  <dbl>
       1 class1 student001 flipped closed-book         o      o
       2 class1 student002 flipped   take-home         o      x
       3 class1 student003 flipped   open-book         o      x
       4 class1 student004 flipped closed-book         o      x
       5 class1 student005 flipped closed-book         o      x
       6 class1 student006 flipped   open-book         o      x
       7 class1 student007 flipped closed-book         o      x
       8 class1 student008 flipped   open-book         o      x
       9 class1 student009 flipped   take-home         o      x
      10 class1 student010 flipped   take-home         o      x
      # i 110 more rows

---

    Code
      des2
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark
      |   +-quiz1_mark
      |   +-quiz2_mark
      |   \-gender
      +-style (2 levels)
      +-room
      \-teacher

---

    Code
      serve_table(des2)
    Output
      # Effective teaching 
      # An edibble: 120 x 10
          class    student   style       exam exam_mark quiz1_mark quiz2_mark   gender
         <U(4)>   <U(120)>  <T(2)>     <T(3)>  <R(120)>   <R(120)>   <R(120)> <R(120)>
          <chr>      <chr>   <chr>      <chr>     <dbl>      <dbl>      <dbl>    <dbl>
       1 class1 student001 flipped closed-bo~         o          o          o        o
       2 class1 student002 flipped  take-home         o          o          o        o
       3 class1 student003 flipped  open-book         o          o          o        o
       4 class1 student004 flipped closed-bo~         o          o          o        o
       5 class1 student005 flipped closed-bo~         o          o          o        o
       6 class1 student006 flipped  open-book         o          o          o        o
       7 class1 student007 flipped closed-bo~         o          o          o        o
       8 class1 student008 flipped  open-book         o          o          o        o
       9 class1 student009 flipped  take-home         o          o          o        o
      10 class1 student010 flipped  take-home         o          o          o        o
      # i 110 more rows
      # i 2 more variables: room <R(4)>, teacher <R(4)>

---

    Code
      des2 %>% expect_rcrds(exam_mark = to_be_numeric(with_value(between = c(0, 100))),
      quiz1_mark = to_be_integer(with_value(between = c(0, 15))), quiz2_mark = to_be_integer(
        with_value(between = c(0, 30))), gender = to_be_factor(levels = c("female",
        "male", "non-binary")), teacher = to_be_character(length = with_value("<=",
        100)), room = to_be_character(length = with_value(">=", 1)))
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark [0, 100]
      |   +-quiz1_mark [0, 15]
      |   +-quiz2_mark [0, 30]
      |   \-gender [female, male, non-binary]
      +-style (2 levels)
      +-room 
      \-teacher 

---

    Code
      des2 %>% expect_rcrds(exam_mark = to_be_numeric(with_value(between = c(0, 100))),
      quiz1_mark >= 0L, quiz1_mark <= 15L, quiz2_mark < 12, factor(gender, levels = c(
        "female", "male", "non-binary")))
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark [0, 100]
      |   +-quiz1_mark [0, 15]
      |   +-quiz2_mark [-Inf, 12)
      |   \-gender [female, male, non-binary]
      +-style (2 levels)
      +-room
      \-teacher

---

    Code
      des2 %>% expect_rcrds(exam_mark >= 0, exam_mark <= 100, factor(gender, levels = c(
        "female", "male", "non-binary")))
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark [0, 100]
      |   +-quiz1_mark
      |   +-quiz2_mark
      |   \-gender [female, male, non-binary]
      +-style (2 levels)
      +-room
      \-teacher

---

    Code
      des2 %>% expect_rcrds(exam_mark < -1)
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark [-Inf, -1)
      |   +-quiz1_mark
      |   +-quiz2_mark
      |   \-gender
      +-style (2 levels)
      +-room
      \-teacher

---

    Code
      des2 %>% expect_rcrds(0 < exam_mark)
    Output
      Effective teaching
      +-class (4 levels)
      | \-student (120 levels)
      |   +-exam (3 levels)
      |   +-exam_mark (0, Inf]
      |   +-quiz1_mark
      |   +-quiz2_mark
      |   \-gender
      +-style (2 levels)
      +-room
      \-teacher

