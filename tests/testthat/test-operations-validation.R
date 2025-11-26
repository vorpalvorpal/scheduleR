# Tests for schtask_delete, schtask_run, schtask_end input validation

testthat::test_that("schtask_delete validates task_name", {
  testthat::expect_error(
    schtask_delete("Task<Invalid"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_delete(""),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_delete(NULL),
    class = "simpleError"
  )
})

testthat::test_that("schtask_delete validates confirm flag", {
  testthat::expect_error(
    schtask_delete("Task", confirm = "yes"),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_delete("Task", confirm = 1),
    class = "simpleError"
  )
})

testthat::test_that("schtask_run validates task_name", {
  testthat::expect_error(
    schtask_run("Task<Invalid"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_run(""),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_run(NULL),
    class = "simpleError"
  )
})

testthat::test_that("schtask_end validates task_name", {
  testthat::expect_error(
    schtask_end("Task<Invalid"),
    "invalid characters"
  )
  testthat::expect_error(
    schtask_end(""),
    class = "simpleError"
  )
  testthat::expect_error(
    schtask_end(NULL),
    class = "simpleError"
  )
})
