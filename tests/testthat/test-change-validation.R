# Tests for schtask_change input validation

testthat::test_that("schtask_change validates task_name", {
  testthat::expect_error(
    schtask_change("Task<Invalid", task_run = "C:\\test.exe"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_change(""),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_change(NULL),
    class = "simpleError"
  )
})

testthat::test_that("schtask_change rejects both end_time and duration", {
  testthat::expect_error(
    schtask_change("Task", end_time = "17:00", duration = "0008:00"),
    "Cannot specify both"
  )
})

testthat::test_that("schtask_change validates start_time format", {
  testthat::expect_error(
    schtask_change("Task", start_time = "invalid"),
    "Invalid time format"
  )
  testthat::expect_error(
    schtask_change("Task", start_time = "25:00"),
    "Invalid time format"
  )
})

testthat::test_that("schtask_change validates end_time format", {
  testthat::expect_error(
    schtask_change("Task", end_time = "invalid"),
    "Invalid time format"
  )
})

testthat::test_that("schtask_change validates interval range", {
  testthat::expect_error(
    schtask_change("Task", interval = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_change("Task", interval = 600000),
    class = "simpleError"
  )
})

testthat::test_that("schtask_change validates run_level", {
  testthat::expect_error(
    schtask_change("Task", run_level = "INVALID"),
    class = "simpleError"
  )
})

testthat::test_that("schtask_change validates enable flag type", {
  testthat::expect_error(
    schtask_change("Task", enable = "yes"),
    class = "simpleError"
  )
})
