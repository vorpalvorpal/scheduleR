testthat::test_that(".quote_path wraps path in quotes", {
  testthat::expect_equal(
    .quote_path("C:\\Scripts\\backup.bat"),
    "\"C:\\Scripts\\backup.bat\""
  )
  testthat::expect_equal(
    .quote_path("C:\\Program Files\\App\\run.exe"),
    "\"C:\\Program Files\\App\\run.exe\""
  )
  testthat::expect_equal(
    .quote_path("simple.exe"),
    "\"simple.exe\""
  )
})

testthat::test_that(".quote_path rejects empty paths", {
  testthat::expect_error(.quote_path(""), class = "simpleError")
})

testthat::test_that(".quote_path rejects NULL", {
  testthat::expect_error(.quote_path(NULL), class = "simpleError")
})

testthat::test_that(".quote_task_name wraps name in quotes", {
  testthat::expect_equal(
    .quote_task_name("MyTask"),
    "\"MyTask\""
  )
  testthat::expect_equal(
    .quote_task_name("My Task With Spaces"),
    "\"My Task With Spaces\""
  )
  testthat::expect_equal(
    .quote_task_name("\\Folder\\SubTask"),
    "\"\\Folder\\SubTask\""
  )
})
