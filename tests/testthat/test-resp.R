test_that("measure response", {

  skip("skip")

  des <-
  start_design(name = "Effective teaching") %>%
    set_units(class = 4,
              student = nested_in(class, 30)) %>%
    set_trts(style = c("flipped", "traditional"),
             exam = c("take-home", "open-book", "closed-book")) %>%
    allocate_trts(style ~ class,
                  exam ~ student) %>%
    randomise_trts() %>%
    record_vars(student = c(exam_mark,
                            quiz1_mark,
                            quiz2_mark,
                            gender),
                 class = c(room,
                           teacher)) %>%
    expect_vars( exam_mark = to_be_numeric(with_value(between = c(0, 100))),
                quiz1_mark = to_be_integer(with_value(between = c(0, 15))),
                quiz2_mark = to_be_integer(with_value(between = c(0, 30))),
                    gender = to_be_selected(from = c("female", "male", "non-binary")),
                   teacher = to_be_character(length = with_value("<=", 100)),
                      room = to_be_character(length = with_value(">=", 1)))

  export_design(des, "~/Downloads/temp.xlsx")


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
