# Tests for schtask_query input validation and utility functions

testthat::test_that("schtask_query validates task_name when provided", {
  testthat::expect_error(
    schtask_query(task_name = "Task<Invalid"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_query(task_name = ""),
    class = "simpleError"
  )
})

testthat::test_that("schtask_query validates verbose flag", {
  testthat::expect_error(
    schtask_query(verbose = "yes"),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_query(verbose = 1),
    class = "simpleError"
  )
})

testthat::test_that(".to_snake_case_names converts column names", {
  df <- data.frame(
    `TaskName` = "Test",
    `Next Run Time` = "Never",
    `Last Result` = "0",
    check.names = FALSE
  )

  result <- .to_snake_case_names(df)

  testthat::expect_equal(names(result), c("task_name", "next_run_time", "last_result"))
})

testthat::test_that(".to_snake_case_names handles already snake_case names", {
  df <- data.frame(
    task_name = "Test",
    next_run = "Never"
  )

  result <- .to_snake_case_names(df)

  testthat::expect_equal(names(result), c("task_name", "next_run"))
})

testthat::test_that(".to_snake_case_names handles empty data frame", {
  df <- data.frame()

  result <- .to_snake_case_names(df)

  testthat::expect_equal(nrow(result), 0)
})
