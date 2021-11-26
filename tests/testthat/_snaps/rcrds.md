# measure response

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
      Assignment: random 

---

    Code
      serve_table(des1)
    Output
      # An edibble: 120 x 6
             class     student    style        exam exam_mark   room
         <unit(4)> <unit(120)> <trt(2)>    <trt(3)>    <rcrd> <rcrd>
       1    class1   student1   flipped take-home           ■      ■
       2    class1   student2   flipped take-home           ■      x
       3    class1   student3   flipped closed-book         ■      x
       4    class1   student4   flipped take-home           ■      x
       5    class1   student5   flipped take-home           ■      x
       6    class1   student6   flipped take-home           ■      x
       7    class1   student7   flipped open-book           ■      x
       8    class1   student8   flipped open-book           ■      x
       9    class1   student9   flipped take-home           ■      x
      10    class1   student10  flipped closed-book         ■      x
      # ... with 110 more rows

---

    Code
      des2 <- des0 %>% set_rcrds_of(student = c("exam_mark", "quiz1_mark",
        "quiz2_mark", "gender"), class = c("room", "teacher"))

---

    Code
      serve_table(des2)
    Output
      # An edibble: 120 x 10
             class     student    style        exam exam_mark quiz1_mark quiz2_mark
         <unit(4)> <unit(120)> <trt(2)>    <trt(3)>    <rcrd>     <rcrd>     <rcrd>
       1    class1   student1   flipped take-home           ■          ■          ■
       2    class1   student2   flipped take-home           ■          ■          ■
       3    class1   student3   flipped closed-book         ■          ■          ■
       4    class1   student4   flipped take-home           ■          ■          ■
       5    class1   student5   flipped take-home           ■          ■          ■
       6    class1   student6   flipped take-home           ■          ■          ■
       7    class1   student7   flipped open-book           ■          ■          ■
       8    class1   student8   flipped open-book           ■          ■          ■
       9    class1   student9   flipped take-home           ■          ■          ■
      10    class1   student10  flipped closed-book         ■          ■          ■
      # ... with 110 more rows, and 3 more variables: gender <rcrd>, room <rcrd>,
      #   teacher <rcrd>

