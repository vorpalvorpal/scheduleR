#' Create a scheduled task that runs every N minutes
#'
#' Creates a task that runs repeatedly at a specified minute interval.
#'
#' @param task_name Character string. Name of the task (max 238 characters).
#' @param task_run Character string. The program, script, or command to run.
#'   Can be an absolute path (e.g., `"C:/Scripts/script.R"`), a relative path
#'   (e.g., `"script.R"` or `"subfolder/script.R"`, interpreted relative to `exec_path`),
#'   or an executable name on PATH (e.g., `"notepad.exe"`).
#' @param every Integer. Run the task every N minutes (1-1439). Default is 1.
#' @param script Character string or NULL. Path to script interpreter executable.
#'   Can be an absolute path (e.g., `"C:/Python311/python.exe"`), a relative path
#'   (interpreted relative to `exec_path`), or an executable name on PATH
#'   (e.g., `"python"`). If `NULL`, runs `task_run` directly via cmd.exe.
#'   Default is Rscript.exe from the current R installation.
#' @param exec_path Character string. Directory from which to execute the task.
#'   Relative paths in `task_run` and `script` are interpreted relative to this
#'   directory. The path is automatically expanded (e.g., `~` becomes the user's
#'   home directory). Default is `here::here()` (project root).
#' @param start_time Character string. Start time in HH:MM 24-hour format.
#'   If `NULL` (default), the task starts immediately.
#' @param end_time Character string. End time in HH:MM 24-hour format.
#'   Task won't start new instances after this time. Optional.
#' @param duration Character string. Maximum duration in HHHH:MM format.
#'   Task won't start new instances after this duration. Optional.
#'   Cannot be used with `end_time`.
#' @param start_date Date or character string. Date when the schedule starts.
#'   If `NULL`, uses current date.
#' @param end_date Date or character string. Date when the schedule ends. Optional.
#' @param run_level Character string. Either `"LIMITED"` (default, runs with
#'   standard user privileges) or `"HIGHEST"` (runs with elevated privileges).
#' @param run_as_user Character string. User account under which the task runs.
#'   If `NULL`, runs as the current user. Use `"SYSTEM"` for system account.
#' @param kill_on_end Logical. If `TRUE`, stops the program when `end_time` or
#'   `duration` is reached. Default is `FALSE`.
#' @param delete_when_done Logical. If `TRUE`, deletes the task after it completes.
#'   Default is `FALSE`.
#' @param force Logical. If `TRUE`, suppresses warnings if the task already exists
#'   and overwrites it. Default is `FALSE`.
#' @param interactive_only Logical. If `TRUE`, the task only runs when the
#'   `run_as_user` is logged on. Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run an R script every 30 minutes
#' schtask_create_minute(
#'   task_name = "DataRefresh",
#'   task_run = "refresh_data.R",
#'   every = 30
#' )
#'
#' # Run every 15 minutes between 9am and 5pm from a specific directory
#' schtask_create_minute(
#'   task_name = "WorkHoursTask",
#'   task_run = "check_status.R",
#'   exec_path = "C:/Projects/myproject",
#'   every = 15,
#'   start_time = "09:00",
#'   end_time = "17:00"
#' )
#'
#' # Run a Python script every hour
#' schtask_create_minute(
#'   task_name = "PythonTask",
#'   task_run = "script.py",
#'   script = "C:/Python311/python.exe",
#'   every = 60
#' )
#' }
schtask_create_minute <- function(
    task_name,
    task_run,
    every = 1L,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_time = NULL,
    end_time = NULL,
    duration = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_integerish(every, lower = 1L, upper = 1439L, len = 1L)

  if (!is.null(end_time) && !is.null(duration))
    cli::cli_abort("Cannot specify both {.arg end_time} and {.arg duration}.")

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "MINUTE",
    modifier = every,
    start_time = start_time,
    end_time = end_time,
    duration = duration,
    start_date = start_date,
    end_date = end_date,
    run_level = run_level,
    run_as_user = run_as_user,
    kill_on_end = kill_on_end,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs every N hours
#'
#' Creates a task that runs repeatedly at a specified hourly interval.
#'
#' @inheritParams schtask_create_minute
#' @param every Integer. Run the task every N hours (1-23). Default is 1.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run every 2 hours
#' schtask_create_hourly(
#'   task_name = "BiHourlyCheck",
#'   task_run = "check_status.R",
#'   every = 2
#' )
#' }
schtask_create_hourly <- function(
    task_name,
    task_run,
    every = 1L,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_time = NULL,
    end_time = NULL,
    duration = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_integerish(every, lower = 1L, upper = 23L, len = 1L)

  if (!is.null(end_time) && !is.null(duration))
    cli::cli_abort("Cannot specify both {.arg end_time} and {.arg duration}.")

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "HOURLY",
    modifier = every,
    start_time = start_time,
    end_time = end_time,
    duration = duration,
    start_date = start_date,
    end_date = end_date,
    run_level = run_level,
    run_as_user = run_as_user,
    kill_on_end = kill_on_end,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs every N days
#'
#' Creates a task that runs at a specified time every N days.
#'
#' @inheritParams schtask_create_minute
#' @param every Integer. Run the task every N days (1-365). Default is 1 (daily).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run daily at 3am
#' schtask_create_daily(
#'   task_name = "NightlyBackup",
#'   task_run = "backup.R",
#'   start_time = "03:00"
#' )
#'
#' # Run every 3 days
#' schtask_create_daily(
#'   task_name = "TriDailyReport",
#'   task_run = "report.R",
#'   every = 3,
#'   start_time = "08:00"
#' )
#' }
schtask_create_daily <- function(
    task_name,
    task_run,
    every = 1L,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_time = NULL,
    end_time = NULL,
    duration = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_integerish(every, lower = 1L, upper = 365L, len = 1L)

  if (!is.null(end_time) && !is.null(duration))
    cli::cli_abort("Cannot specify both {.arg end_time} and {.arg duration}.")

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "DAILY",
    modifier = every,
    start_time = start_time,
    end_time = end_time,
    duration = duration,
    start_date = start_date,
    end_date = end_date,
    run_level = run_level,
    run_as_user = run_as_user,
    kill_on_end = kill_on_end,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs on specific days of the week
#'
#' Creates a task that runs on specified days of the week at regular intervals.
#'
#' @inheritParams schtask_create_minute
#' @param every Integer. Run the task every N weeks (1-52). Default is 1 (weekly).
#' @param days Character vector. Days of the week to run the task.
#'   Valid values: `"MON"`, `"TUE"`, `"WED"`, `"THU"`, `"FRI"`, `"SAT"`, `"SUN"`,
#'   or `"*"` for every day. Full day names are also accepted.
#'   Default is `"MON"`.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run every Monday at 9am
#' schtask_create_weekly(
#'   task_name = "MondayReport",
#'   task_run = "report.R",
#'   start_time = "09:00"
#' )
#'
#' # Run every weekday
#' schtask_create_weekly(
#'   task_name = "WeekdaySync",
#'   task_run = "sync.R",
#'   days = c("MON", "TUE", "WED", "THU", "FRI"),
#'   start_time = "08:00"
#' )
#'
#' # Run every other Friday
#' schtask_create_weekly(
#'   task_name = "BiWeeklyPayroll",
#'   task_run = "payroll.R",
#'   every = 2,
#'   days = "FRI",
#'   start_time = "17:00"
#' )
#' }
schtask_create_weekly <- function(
    task_name,
    task_run,
    every = 1L,
    days = "MON",
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_time = NULL,
    end_time = NULL,
    duration = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_integerish(every, lower = 1L, upper = 52L, len = 1L)
  checkmate::assert_character(days, min.len = 1L)

  if (!is.null(end_time) && !is.null(duration))
    cli::cli_abort("Cannot specify both {.arg end_time} and {.arg duration}.")

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  # Handle wildcard
  if (identical(days, "*")) {
    day_arg <- "*"
  } else {
    day_arg <- .normalise_days_of_week(days)
  }

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "WEEKLY",
    modifier = every,
    day = day_arg,
    start_time = start_time,
    end_time = end_time,
    duration = duration,
    start_date = start_date,
    end_date = end_date,
    run_level = run_level,
    run_as_user = run_as_user,
    kill_on_end = kill_on_end,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs monthly
#'
#' Creates a task that runs on a monthly schedule. The schedule can be configured
#' in several ways:
#'
#' * **Every N months on a specific day of the month**: Set `modifier` to a number
#'   (1-12) and `day` to the day of the month (1-31).
#' * **On a specific week and day of the month**: Set `modifier` to `"FIRST"`,
#'   `"SECOND"`, `"THIRD"`, `"FOURTH"`, or `"LAST"`, and `day` to a day of the week.
#' * **On the last day of the month**: Set `modifier` to `"LASTDAY"`.
#'
#' @inheritParams schtask_create_minute
#' @param modifier Either an integer (1-12) specifying to run every N months,
#'   or a character string: `"FIRST"`, `"SECOND"`, `"THIRD"`, `"FOURTH"`,
#'   `"LAST"` (for week-of-month scheduling), or `"LASTDAY"` (for last day of month).
#'   Default is 1 (every month).
#' @param day Day specification. The meaning depends on `modifier`:
#'
#'   * If `modifier` is numeric: day of the month (1-31). Default is 1.
#'   * If `modifier` is `"FIRST"`, `"SECOND"`, `"THIRD"`, `"FOURTH"`, or `"LAST"`:
#'     day of the week (`"MON"`, `"TUE"`, etc.). Required in this case.
#'   * If `modifier` is `"LASTDAY"`: not used (a warning is issued if provided).
#'
#' @param months Character vector of months when the task should run.
#'   Valid values: `"JAN"`, `"FEB"`, `"MAR"`, `"APR"`, `"MAY"`, `"JUN"`,
#'   `"JUL"`, `"AUG"`, `"SEP"`, `"OCT"`, `"NOV"`, `"DEC"`, or `"*"` for all months.
#'   Full month names are also accepted. Default is `"*"` (all months).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run on the 15th of every month
#' schtask_create_monthly(
#'   task_name = "MidMonthReport",
#'   task_run = "report.R",
#'   day = 15,
#'   start_time = "09:00"
#' )
#'
#' # Run on the second Tuesday of every month
#' schtask_create_monthly(
#'   task_name = "SecondTuesday",
#'   task_run = "meeting.R",
#'   modifier = "SECOND",
#'   day = "TUE",
#'   start_time = "10:00"
#' )
#'
#' # Run on the last day of every month
#' schtask_create_monthly(
#'   task_name = "MonthEnd",
#'   task_run = "close.R",
#'   modifier = "LASTDAY",
#'   start_time = "18:00"
#' )
#'
#' # Run every 3 months on the 1st
#' schtask_create_monthly(
#'   task_name = "QuarterlyReview",
#'   task_run = "review.R",
#'   modifier = 3,
#'   day = 1,
#'   start_time = "09:00"
#' )
#'
#' # Run on the first Monday of March and September
#' schtask_create_monthly(
#'   task_name = "BiAnnualKickoff",
#'   task_run = "kickoff.R",
#'   modifier = "FIRST",
#'   day = "MON",
#'   months = c("MAR", "SEP"),
#'   start_time = "09:00"
#' )
#' }
schtask_create_monthly <- function(
    task_name,
    task_run,
    modifier = 1L,
    day = NULL,
    months = "*",
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_time = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  # Normalise modifier
  week_modifiers <- c("FIRST", "SECOND", "THIRD", "FOURTH", "LAST")

  if (is.numeric(modifier)) {
    checkmate::assert_integerish(modifier, lower = 1L, upper = 12L, len = 1L)
    mod_type <- "numeric"
  } else {
    checkmate::assert_string(modifier)
    modifier <- toupper(modifier)
    if (modifier == "LASTDAY") {
      mod_type <- "lastday"
    } else if (modifier %in% week_modifiers) {
      mod_type <- "week"
    } else {
      cli::cli_abort(c(
        "Invalid {.arg modifier} value.",
        "x" = "Expected a number (1-12), or one of: {.val {c(week_modifiers, 'LASTDAY')}}",
        "i" = "Got: {.val {modifier}}"
      ))
    }
  }

  # Handle day parameter based on modifier type
  day_arg <- NULL

  if (mod_type == "numeric") {
    # Day of month
    if (is.null(day)) {
      day <- 1L
    }
    checkmate::assert_integerish(day, lower = 1L, upper = 31L, len = 1L)
    day_arg <- as.character(day)

  } else if (mod_type == "week") {
    # Day of week - required
    if (is.null(day))
      cli::cli_abort(c(
        "The {.arg day} parameter is required when {.arg modifier} is {.val {modifier}}.",
        "i" = "Specify a day of the week, e.g., {.val MON}, {.val TUE}."
      ))

    day_arg <- .normalise_day_of_week(as.character(day))

  } else if (mod_type == "lastday") {
    # Day not used
    if (!is.null(day))
      cli::cli_warn(c(
        "The {.arg day} parameter is ignored when {.arg modifier} is {.val LASTDAY}.",
        "i" = "Task will run on the last day of each specified month."
      ))
  }

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  # Normalise months
  months_arg <- .normalise_months(months)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "MONTHLY",
    modifier = modifier,
    day = day_arg,
    months = months_arg,
    start_time = start_time,
    start_date = start_date,
    end_date = end_date,
    run_level = run_level,
    run_as_user = run_as_user,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs once
#'
#' Creates a task that runs exactly once at a specified date and time.
#'
#' @inheritParams schtask_create_minute
#' @param start_time Character string. Start time in HH:MM 24-hour format. Required.
#' @param start_date Date or character string. Date when the task should run.
#'   If `NULL`, uses current date.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run once at a specific date and time
#' schtask_create_once(
#'   task_name = "OneTimeSetup",
#'   task_run = "setup.R",
#'   start_time = "14:30",
#'   start_date = "2024/06/15"
#' )
#' }
schtask_create_once <- function(
    task_name,
    task_run,
    start_time,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_string(start_time)
  .validate_time(start_time, "start_time")

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "ONCE",
    start_time = start_time,
    start_date = start_date,
    run_level = run_level,
    run_as_user = run_as_user,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs at system startup
#'
#' Creates a task that runs every time the system starts.
#'
#' @inheritParams schtask_create_minute
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run at every system startup
#' schtask_create_on_start(
#'   task_name = "StartupService",
#'   task_run = "startup.R"
#' )
#'
#' # Run at startup with system privileges
#' schtask_create_on_start(
#'   task_name = "SystemStartup",
#'   task_run = "syscheck.R",
#'   run_as_user = "SYSTEM"
#' )
#' }
schtask_create_on_start <- function(
    task_name,
    task_run,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "ONSTART",
    start_date = start_date,
    run_level = run_level,
    run_as_user = run_as_user,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  # TODO: Add /delay parameter support

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs when a user logs on
#'
#' Creates a task that runs whenever any user logs on to the computer.
#'
#' @inheritParams schtask_create_minute
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run when any user logs on
#' schtask_create_on_logon(
#'   task_name = "LogonScript",
#'   task_run = "logon.R"
#' )
#' }
schtask_create_on_logon <- function(
    task_name,
    task_run,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "ONLOGON",
    start_date = start_date,
    run_level = run_level,
    run_as_user = run_as_user,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  # TODO: Add /delay parameter support

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


#' Create a scheduled task that runs when the system is idle
#'
#' Creates a task that runs when the computer has been idle for a specified
#' amount of time.
#'
#' @inheritParams schtask_create_minute
#' @param idle_time Integer. Number of minutes the computer must be idle before
#'   the task starts (1-999). Required.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run after 10 minutes of idle time
#' schtask_create_on_idle(
#'   task_name = "IdleMaintenance",
#'   task_run = "maintenance.R",
#'   idle_time = 10
#' )
#' }
schtask_create_on_idle <- function(
    task_name,
    task_run,
    idle_time,
    script = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
    exec_path = here::here(),
    start_date = NULL,
    run_level = "LIMITED",
    run_as_user = NULL,
    delete_when_done = FALSE,
    force = FALSE,
    interactive_only = FALSE
) {
  checkmate::assert_integerish(idle_time, lower = 1L, upper = 999L, len = 1L)

  # Build task command
  task_command <- .build_task_command(task_run, script, exec_path)

  args <- .build_create_args(
    task_name = task_name,
    task_run = task_command,
    schedule_type = "ONIDLE",
    idle_time = idle_time,
    start_date = start_date,
    run_level = run_level,
    run_as_user = run_as_user,
    delete_when_done = delete_when_done,
    force = force,
    interactive_only = interactive_only
  )

  .execute_schtasks(args, error_msg = "Failed to create scheduled task")
  cli::cli_alert_success("Created scheduled task: {.val {task_name}}")

  invisible(TRUE)
}


# TODO: Implement schtask_create_onevent() for ONEVENT schedule type
# This requires /ec (event channel) and potentially /mo (XPath event query)
# parameters which add complexity.
