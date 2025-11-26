testthat::test_that(".build_create_args includes required arguments", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY"
  )

  testthat::expect_true("/create" %in% args)
  testthat::expect_true("/sc" %in% args)
  testthat::expect_true("DAILY" %in% args)
  testthat::expect_true("/tn" %in% args)
  testthat::expect_true("\"TestTask\"" %in% args)
  testthat::expect_true("/tr" %in% args)
  testthat::expect_true("\"C:\\test.exe\"" %in% args)
})

testthat::test_that(".build_create_args includes modifier when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    modifier = 5
  )

  testthat::expect_true("/mo" %in% args)
  testthat::expect_true("5" %in% args)
})

testthat::test_that(".build_create_args includes day when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "WEEKLY",
    day = "MON,WED,FRI"
  )

  testthat::expect_true("/d" %in% args)
  testthat::expect_true("MON,WED,FRI" %in% args)
})

testthat::test_that(".build_create_args includes months when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "MONTHLY",
    months = "JAN,APR,JUL,OCT"
  )

  testthat::expect_true("/m" %in% args)
  testthat::expect_true("JAN,APR,JUL,OCT" %in% args)
})

testthat::test_that(".build_create_args includes start_time when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    start_time = "09:00"
  )

  testthat::expect_true("/st" %in% args)
  testthat::expect_true("09:00" %in% args)
})

testthat::test_that(".build_create_args includes end_time when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "MINUTE",
    end_time = "17:00"
  )

  testthat::expect_true("/et" %in% args)
  testthat::expect_true("17:00" %in% args)
})

testthat::test_that(".build_create_args includes duration when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "HOURLY",
    duration = "0010:00"
  )

  testthat::expect_true("/du" %in% args)
  testthat::expect_true("0010:00" %in% args)
})

testthat::test_that(".build_create_args includes interval when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    interval = 30
  )

  testthat::expect_true("/ri" %in% args)
  testthat::expect_true("30" %in% args)
})

testthat::test_that(".build_create_args includes start_date when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    start_date = "2024/06/15"
  )

  testthat::expect_true("/sd" %in% args)
  testthat::expect_true("2024/06/15" %in% args)
})

testthat::test_that(".build_create_args includes end_date when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    end_date = "2024/12/31"
  )

  testthat::expect_true("/ed" %in% args)
  testthat::expect_true("2024/12/31" %in% args)
})

testthat::test_that(".build_create_args includes run_level when provided", {
  args_limited <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    run_level = "LIMITED"
  )

  testthat::expect_true("/rl" %in% args_limited)
  testthat::expect_true("LIMITED" %in% args_limited)

  args_highest <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    run_level = "highest"
  )

  testthat::expect_true("/rl" %in% args_highest)
  testthat::expect_true("HIGHEST" %in% args_highest)
})

testthat::test_that(".build_create_args includes run_as_user when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    run_as_user = "SYSTEM"
  )

  testthat::expect_true("/ru" %in% args)
  testthat::expect_true("SYSTEM" %in% args)
})

testthat::test_that(".build_create_args includes flags when TRUE", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    kill_on_end = TRUE,
    delete_when_done = TRUE,
    force = TRUE,
    interactive_only = TRUE
  )

  testthat::expect_true("/k" %in% args)
  testthat::expect_true("/z" %in% args)
  testthat::expect_true("/f" %in% args)
  testthat::expect_true("/it" %in% args)
})

testthat::test_that(".build_create_args excludes flags when FALSE", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "DAILY",
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
  )

  testthat::expect_false("/k" %in% args)
  testthat::expect_false("/z" %in% args)
  testthat::expect_false("/f" %in% args)
  testthat::expect_false("/it" %in% args)
})

testthat::test_that(".build_create_args includes idle_time when provided", {
  args <- .build_create_args(
    task_name = "TestTask",
    task_run = "C:\\test.exe",
    schedule_type = "ONIDLE",
    idle_time = 10
  )

  testthat::expect_true("/i" %in% args)
  testthat::expect_true("10" %in% args)
})

testthat::test_that(".build_create_args validates idle_time range", {
  testthat::expect_error(
    .build_create_args(
      task_name = "TestTask",
      task_run = "C:\\test.exe",
      schedule_type = "ONIDLE",
      idle_time = 0
    ),
    class = "simpleError"
  )

  testthat::expect_error(
    .build_create_args(
      task_name = "TestTask",
      task_run = "C:\\test.exe",
      schedule_type = "ONIDLE",
      idle_time = 1000
    ),
    class = "simpleError"
  )
})

testthat::test_that(".build_create_args validates interval range", {
  testthat::expect_error(
    .build_create_args(
      task_name = "TestTask",
      task_run = "C:\\test.exe",
      schedule_type = "DAILY",
      interval = 0
    ),
    class = "simpleError"
  )

  testthat::expect_error(
    .build_create_args(
      task_name = "TestTask",
      task_run = "C:\\test.exe",
      schedule_type = "DAILY",
      interval = 600000
    ),
    class = "simpleError"
  )
})
