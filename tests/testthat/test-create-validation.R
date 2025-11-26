# Tests for input validation in schtask_create_* functions
# These tests do not execute schtasks.exe - they only test validation logic

testthat::test_that("schtask_create_minute validates every parameter", {
  testthat::expect_error(
    schtask_create_minute("Task", "test.R", every = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_minute("Task", "test.R", every = 1440),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_minute("Task", "test.R", every = -1),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_minute rejects both end_time and duration", {
  testthat::expect_error(
    schtask_create_minute(
      "Task", "test.R",
      end_time = "17:00",
      duration = "0008:00"
    ),
    "Cannot specify both"
  )
})

testthat::test_that("schtask_create_hourly validates every parameter", {
  testthat::expect_error(
    schtask_create_hourly("Task", "test.R", every = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_hourly("Task", "test.R", every = 24),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_daily validates every parameter", {
  testthat::expect_error(
    schtask_create_daily("Task", "test.R", every = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_daily("Task", "test.R", every = 366),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_weekly validates every parameter", {
  testthat::expect_error(
    schtask_create_weekly("Task", "test.R", every = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_weekly("Task", "test.R", every = 53),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_weekly validates days parameter", {
  testthat::expect_error(
    schtask_create_weekly("Task", "test.R", days = "INVALID"),
    "Invalid day of week"
  )
})

testthat::test_that("schtask_create_monthly validates numeric modifier", {
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = 13),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_monthly validates character modifier", {
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = "INVALID"),
    "Invalid"
  )
})

testthat::test_that("schtask_create_monthly requires day for week modifiers", {
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = "FIRST"),
    "day.*parameter is required"
  )
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = "SECOND"),
    "day.*parameter is required"
  )
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = "LAST"),
    "day.*parameter is required"
  )
})

testthat::test_that("schtask_create_monthly warns when day provided with LASTDAY", {
  testthat::expect_warning(
    # This will fail because schtasks isn't available, but the warning should
    # be issued before the command runs - so we wrap in try
    try(
      schtask_create_monthly(
        "Task", "test.R",
        modifier = "LASTDAY",
        day = 15
      ),
      silent = TRUE
    ),
    "day.*parameter is ignored"
  )
})

testthat::test_that("schtask_create_monthly validates day range for numeric modifier", {
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = 1, day = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", modifier = 1, day = 32),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_monthly validates months parameter", {
  testthat::expect_error(
    schtask_create_monthly("Task", "test.R", months = "INVALID"),
    "Invalid month"
  )
})

testthat::test_that("schtask_create_once requires start_time", {
  testthat::expect_error(
    schtask_create_once("Task", "test.R"),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_once validates start_time format", {
  testthat::expect_error(
    schtask_create_once("Task", "test.R", start_time = "invalid"),
    "Invalid time format"
  )
})

testthat::test_that("schtask_create_on_idle validates idle_time parameter", {
  testthat::expect_error(
    schtask_create_on_idle("Task", "test.R", idle_time = 0),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_create_on_idle("Task", "test.R", idle_time = 1000),
    class = "simpleError"
  )
})

testthat::test_that("schtask_create_on_idle requires idle_time", {
  testthat::expect_error(
    schtask_create_on_idle("Task", "test.R"),
    class = "simpleError"
  )
})

testthat::test_that("all create functions validate task_name", {
  # Test with invalid character in task name
  testthat::expect_error(
    schtask_create_minute("Task<Name", "test.R"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_create_hourly("Task>Name", "test.R"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_create_daily("Task:Name", "test.R"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_create_weekly("Task|Name", "test.R"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_create_monthly("Task?Name", "test.R"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_create_once("Task*Name", "test.R", start_time = "09:00"),
    "invalid characters"
  )
})
