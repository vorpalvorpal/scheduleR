testthat::test_that(".validate_task_name accepts valid names", {
  testthat::expect_true(.validate_task_name("MyTask"))
  testthat::expect_true(.validate_task_name("My Task With Spaces"))
  testthat::expect_true(.validate_task_name("Task123"))
  testthat::expect_true(.validate_task_name("\\Folder\\SubTask"))
  testthat::expect_true(.validate_task_name("a"))
})

testthat::test_that(".validate_task_name rejects empty names", {
  testthat::expect_error(
    .validate_task_name(""),
    class = "simpleError"
  )
})

testthat::test_that(".validate_task_name rejects NULL", {
  testthat::expect_error(
    .validate_task_name(NULL),
    class = "simpleError"
  )
})

testthat::test_that(".validate_task_name rejects names that are too long", {
  long_name <- paste(rep("a", 239), collapse = "")
  testthat::expect_error(
    .validate_task_name(long_name),
    "exceeds maximum length"
  )

  # Exactly 238 should be fine
  ok_name <- paste(rep("a", 238), collapse = "")
  testthat::expect_true(.validate_task_name(ok_name))
})

testthat::test_that(".validate_task_name rejects invalid characters", {
  testthat::expect_error(.validate_task_name("Task<Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task>Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task:Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task/Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task|Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task?Name"), "invalid characters")
  testthat::expect_error(.validate_task_name("Task*Name"), "invalid characters")
})

testthat::test_that(".validate_time accepts valid times", {
  testthat::expect_true(.validate_time("00:00"))
  testthat::expect_true(.validate_time("09:00"))
  testthat::expect_true(.validate_time("9:00"))
  testthat::expect_true(.validate_time("12:30"))
  testthat::expect_true(.validate_time("23:59"))
  testthat::expect_true(.validate_time("14:05"))
})

testthat::test_that(".validate_time rejects invalid times", {
  testthat::expect_error(.validate_time("24:00"), "Invalid time format")
  testthat::expect_error(.validate_time("12:60"), "Invalid time format")
  testthat::expect_error(.validate_time("12:5"), "Invalid time format")
  testthat::expect_error(.validate_time("1200"), "Invalid time format")
  testthat::expect_error(.validate_time("12-00"), "Invalid time format")
  testthat::expect_error(.validate_time("noon"), "Invalid time format")
  testthat::expect_error(.validate_time(""), "Invalid time format")
})

testthat::test_that(".validate_date accepts valid date strings", {
  testthat::expect_equal(.validate_date("2024/06/15"), "2024/06/15")
  testthat::expect_equal(.validate_date("15/06/2024"), "15/06/2024")
  testthat::expect_equal(.validate_date("2024-06-15"), "2024-06-15")
})
testthat::test_that(".validate_date converts Date objects", {
  d <- as.Date("2024-06-15")
  testthat::expect_equal(.validate_date(d), "2024/06/15")
})

testthat::test_that(".validate_date rejects invalid formats", {
  testthat::expect_error(.validate_date("2024-6-15"), "Invalid date format")
  testthat::expect_error(.validate_date("June 15, 2024"), "Invalid date format")
  testthat::expect_error(.validate_date("15-06-2024"), "Invalid date format")
  testthat::expect_error(.validate_date("tomorrow"), "Invalid date format")
})
