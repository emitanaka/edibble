# measure response

    Code
      des0 %>% set_rcrds(exam_mark = student, room = class) %>% serve_table()
    Output
      [38;5;246m# Effective teaching[39m 
      # An edibble: 120 x 6
             class     student       style        exam exam_mark   room
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd> <rcrd>
       1    class1  student001 traditional closed-book         o      o
       2    class1  student002 traditional closed-book         o      x
       3    class1  student003 traditional take-home           o      x
       4    class1  student004 traditional take-home           o      x
       5    class1  student005 traditional open-book           o      x
       6    class1  student006 traditional take-home           o      x
       7    class1  student007 traditional take-home           o      x
       8    class1  student008 traditional closed-book         o      x
       9    class1  student009 traditional closed-book         o      x
      10    class1  student010 traditional open-book           o      x
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
      Allotment:
    Message <cliMessage>
      * class ~ student
      * style ~ class
      * exam ~ student
      * exam_mark ~ student
      * room ~ class

---

    Code
      serve_table(des1)
    Output
      # Effective teaching 
      # An edibble: 120 x 6
             class     student       style        exam exam_mark   room
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd> <rcrd>
       1    class1  student001 traditional closed-book         o      o
       2    class1  student002 traditional closed-book         o      x
       3    class1  student003 traditional take-home           o      x
       4    class1  student004 traditional take-home           o      x
       5    class1  student005 traditional open-book           o      x
       6    class1  student006 traditional take-home           o      x
       7    class1  student007 traditional take-home           o      x
       8    class1  student008 traditional closed-book         o      x
       9    class1  student009 traditional closed-book         o      x
      10    class1  student010 traditional open-book           o      x
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
      Allotment:
    Message <cliMessage>
      * class ~ student
      * style ~ class
      * exam ~ student
      * exam_mark ~ student
      * quiz1_mark ~ student
      * quiz2_mark ~ student
      * gender ~ student
      * room ~ class
      * teacher ~ class

---

    Code
      serve_table(des2)
    Output
      # Effective teaching 
      # An edibble: 120 x 10
             class     student       style        exam exam_mark quiz1_mark quiz2_mark
         <unit(4)> <unit(120)>    <trt(2)>    <trt(3)>    <rcrd>     <rcrd>     <rcrd>
       1    class1  student001 traditional closed-book         o          o          o
       2    class1  student002 traditional closed-book         o          o          o
       3    class1  student003 traditional take-home           o          o          o
       4    class1  student004 traditional take-home           o          o          o
       5    class1  student005 traditional open-book           o          o          o
       6    class1  student006 traditional take-home           o          o          o
       7    class1  student007 traditional take-home           o          o          o
       8    class1  student008 traditional closed-book         o          o          o
       9    class1  student009 traditional closed-book         o          o          o
      10    class1  student010 traditional open-book           o          o          o
      # i 110 more rows
      # i 3 more variables: gender <rcrd>, room <rcrd>, teacher <rcrd>

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
      |   +-exam_mark
      |   +-quiz1_mark
      |   +-quiz2_mark
      |   \-gender
      +-style (2 levels)
      +-room
      \-teacher
      Allotment:
    Message <cliMessage>
      * class ~ student
      * style ~ class
      * exam ~ student
      * exam_mark ~ student
      * quiz1_mark ~ student
      * quiz2_mark ~ student
      * gender ~ student
      * room ~ class
      * teacher ~ class

