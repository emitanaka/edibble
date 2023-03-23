# measure response

    Code
      des0 %>% serve_table() %>% set_rcrds(exam_mark = student, room = class)
    Output
      # Effective teaching 
      # An edibble: 120 x 6
             class     student       style        exam exam_mark   room
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd> <rcrd>
       1    class1   student1  traditional closed-book         o      o
       2    class1   student2  traditional closed-book         o      x
       3    class1   student3  traditional take-home           o      x
       4    class1   student4  traditional take-home           o      x
       5    class1   student5  traditional open-book           o      x
       6    class1   student6  traditional take-home           o      x
       7    class1   student7  traditional take-home           o      x
       8    class1   student8  traditional closed-book         o      x
       9    class1   student9  traditional closed-book         o      x
      10    class1   student10 traditional open-book           o      x
      # ... with 110 more rows

---

    Code
      des1 <- des0 %>% set_rcrds(exam_mark = student, room = class)
      des1
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | \-exam_mark
      | \-room
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 

---

    Code
      serve_table(des1)
    Output
      # Effective teaching 
      # An edibble: 120 x 6
             class     student       style        exam exam_mark   room
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd> <rcrd>
       1    class1   student1  traditional closed-book         o      o
       2    class1   student2  traditional closed-book         o      x
       3    class1   student3  traditional take-home           o      x
       4    class1   student4  traditional take-home           o      x
       5    class1   student5  traditional open-book           o      x
       6    class1   student6  traditional take-home           o      x
       7    class1   student7  traditional take-home           o      x
       8    class1   student8  traditional closed-book         o      x
       9    class1   student9  traditional closed-book         o      x
      10    class1   student10 traditional open-book           o      x
      # ... with 110 more rows

---

    Code
      des2
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 

---

    Code
      serve_table(des2)
    Output
      # Effective teaching 
      # An edibble: 120 x 10
             class     student       style        exam exam_m~1 quiz1~2 quiz2~3 gender
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>   <rcrd>  <rcrd>  <rcrd> <rcrd>
       1    class1   student1  traditional closed-book        o       o       o      o
       2    class1   student2  traditional closed-book        o       o       o      o
       3    class1   student3  traditional take-home          o       o       o      o
       4    class1   student4  traditional take-home          o       o       o      o
       5    class1   student5  traditional open-book          o       o       o      o
       6    class1   student6  traditional take-home          o       o       o      o
       7    class1   student7  traditional take-home          o       o       o      o
       8    class1   student8  traditional closed-book        o       o       o      o
       9    class1   student9  traditional closed-book        o       o       o      o
      10    class1   student10 traditional open-book          o       o       o      o
      # ... with 110 more rows, 2 more variables: room <rcrd>, teacher <rcrd>, and
      #   abbreviated variable names 1: exam_mark, 2: quiz1_mark, 3: quiz2_mark

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
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 
      Validation:
    Message <cliMessage>
      * exam_mark: numeric [0, 100]
      * quiz1_mark: integer [0, 15]
      * quiz2_mark: integer [0, 30]
      * gender: factor [female, male, non-binary]
      * teacher: text
      * room: text

---

    Code
      des2 %>% expect_rcrds(exam_mark = to_be_numeric(with_value(between = c(0, 100))),
      quiz1_mark >= 0L, quiz1_mark <= 15L, quiz2_mark < 12, factor(gender, levels = c(
        "female", "male", "non-binary")))
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 
      Validation:
    Message <cliMessage>
      * exam_mark: numeric [0, 100]
      * quiz1_mark: integer [0, 15]
      * quiz2_mark: numeric [-Inf, 12)
      * gender: factor [female, male, non-binary]

---

    Code
      des2 %>% expect_rcrds(exam_mark >= 0, exam_mark <= 100, factor(gender, levels = c(
        "female", "male", "non-binary")))
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 
      Validation:
    Message <cliMessage>
      * exam_mark: numeric [0, 100]
      * gender: factor [female, male, non-binary]

---

    Code
      des2 %>% expect_rcrds(exam_mark < -1)
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 
      Validation:
    Message <cliMessage>
      * exam_mark: numeric [-Inf, -1)

---

    Code
      des2 %>% expect_rcrds(0 < exam_mark)
    Output
      Effective teaching
      +-class (4 levels)
      | +-student (120 levels)
      | | +-exam_mark
      | | +-quiz1_mark
      | | +-quiz2_mark
      | | \-gender
      | +-room
      | \-teacher
      +-style (2 levels)
      \-exam (3 levels)
      Allotment:
    Message <cliMessage>
      * style ~ class
      * exam ~ student
    Output
      Assignment: random, random 
      Validation:
    Message <cliMessage>
      * exam_mark: numeric (0, Inf]

