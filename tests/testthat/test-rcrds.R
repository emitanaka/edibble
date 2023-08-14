test_that("measure response", {
  # FIXME

  des0 <- design(title = "Effective teaching") %>%
    set_units(class = 4,
              student = nested_in(class, 30)) %>%
    set_trts(style = c("flipped", "traditional"),
             exam = c("take-home", "open-book", "closed-book")) %>%
    allot_trts(style ~ class,
               exam ~ student) %>%
    assign_trts("random", seed = 1)

  expect_snapshot({
    des0 %>%
      set_rcrds(exam_mark = student,
                room = class) %>%
      serve_table()
  })


  expect_snapshot({
    des1 <- des0 %>%
      set_rcrds(exam_mark = student,
                room = class)
    des1
  })

  expect_snapshot({
    serve_table(des1)
  })
  des2 <- des0 %>%
    set_rcrds_of(student = c("exam_mark",
                             "quiz1_mark",
                             "quiz2_mark",
                             "gender"),
                 class = c("room",
                           "teacher"))
  expect_snapshot({
    des2
  })

  expect_snapshot({
    serve_table(des2)
  })


  expect_snapshot({
    des2 %>%
      expect_rcrds( exam_mark = to_be_numeric(with_value(between = c(0, 100))),
                    quiz1_mark = to_be_integer(with_value(between = c(0, 15))),
                    quiz2_mark = to_be_integer(with_value(between = c(0, 30))),
                    gender = to_be_factor(levels = c("female", "male", "non-binary")),
                    teacher = to_be_character(length = with_value("<=", 100)),
                    room = to_be_character(length = with_value(">=", 1)))
  })



  expect_snapshot({
    des2 %>%
      expect_rcrds( exam_mark = to_be_numeric(with_value(between = c(0, 100))),
                    quiz1_mark >= 0L,
                    quiz1_mark <= 15L,
                    quiz2_mark < 12,
                    factor(gender, levels = c("female", "male", "non-binary")))
  })

  expect_snapshot({
    des2 %>%
      expect_rcrds(exam_mark >= 0,
                   exam_mark <= 100,
                   factor(gender, levels = c("female", "male", "non-binary")))
  })

  expect_snapshot({
    des2 %>%
      expect_rcrds(exam_mark < -1)
  })


  expect_snapshot({
    des2 %>%
      expect_rcrds(0 < exam_mark)
  })

  #export_design(serve_table(des3), "~/Downloads/temp.xlsx", overwrite = TRUE)


})

test_that("as qualities", {

  skip("skip")


  to_be_integer()
  to_be_numeric()
  to_be_date()
  to_be_time()
  to_be_character()
  to_be_list()

  as_value(not_between = c(3, 4))
  as_value(between = c(3, 4))
  as_value("!=", 1)
  as_value("=", 1)
  as_value(">", 1)
  as_value(">=", 1)
  as_value("<", 1)
  as_value("<=", 3)
})
