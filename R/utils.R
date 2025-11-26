# Internal utility functions for scheduleR
# These are not exported

#' Execute schtasks command
#'
#' @param args Character vector of arguments to pass to schtasks.exe.
#' @param error_msg Error message prefix to use on failure.
#'
#' @return A list with `status` (exit code) and `output` (character vector of stdout).
#'
#' @noRd
.execute_schtasks <- function(args, error_msg = "schtasks command failed") {
  # TODO: Add /s, /u, /p parameters for remote computer support
  result <- system2(
    command = "schtasks",
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  exit_status <- attr(result, "status") %||% 0L

  if (exit_status != 0L) {
    error_output <- paste(result, collapse = "\n")
    cli::cli_abort(c(
      "{error_msg}",
      "x" = "Exit code: {exit_status}",
      "i" = "schtasks output:",
      " " = "{error_output}"
    ))
  }

  list(
    status = exit_status,
    output = result
  )
}


#' Validate task name
#'
#' Checks that a task name conforms to schtasks requirements:
#' - Not empty
#' - Maximum 238 characters
#' - Valid filename characters
#'
#' @param task_name Character string of the task name.
#'
#' @return Invisibly returns TRUE if valid, otherwise aborts with an error.
#'
#' @noRd
.validate_task_name <- function(task_name) {
  checkmate::assert_string(task_name, min.chars = 1L)

  if (nchar(task_name) > 238L) {
    cli::cli_abort(c(
      "Task name exceeds maximum length.",
      "x" = "Task name must be 238 characters or fewer (got {nchar(task_name)})."
    ))
  }

  # Invalid filename characters on Windows
  # Note: backslash is allowed as it's used for folder paths
  invalid_chars <- c("<", ">", ":", "\"", "/", "|", "?", "*")
  invalid_pattern <- paste0(
    "[",
    paste0("\\", invalid_chars, collapse = ""),
    "]"
  )

  if (stringr::str_detect(task_name, invalid_pattern)) {
    cli::cli_abort(c(
      "Task name contains invalid characters.",
      "x" = "Task names cannot contain: {.val {invalid_chars}}"
    ))
  }

  invisible(TRUE)
}


#' Quote a path for command line use
#'
#' Wraps a path in double quotes if it contains spaces or special characters.
#'
#' @param path Character string of the path.
#'
#' @return The path, quoted if necessary.
#'
#' @noRd
.quote_path <- function(path) {
  checkmate::assert_string(path, min.chars = 1L)

  # Always quote paths to be safe
  paste0("\"", path, "\"")
}


#' Build command to run a task with optional script interpreter
#'
#' Creates a command that optionally uses a script interpreter (like Rscript.exe)
#' to execute a task in a specified directory.
#'
#' @param task_run Character string. The program, script, or command to run.
#'   Can be an absolute path, a relative path (interpreted relative to `exec_path`),
#'   or an executable name on PATH.
#' @param script Character string or NULL. Path to the script interpreter executable
#'   (e.g., "C:/Program Files/R/R-4.3.0/bin/Rscript.exe"). Can be an absolute path,
#'   relative path, or executable name on PATH. If NULL, runs `task_run` directly
#'   via cmd.exe.
#' @param exec_path Character string. Directory from which to execute the task.
#'   All relative paths in `task_run` and `script` are interpreted relative to this
#'   directory.
#'
#' @return Character string command that can be passed to schtasks /tr.
#'
#' @noRd
.build_task_command <- function(task_run, script = NULL, exec_path = NULL) {
  checkmate::assert_string(task_run, min.chars = 1L)

  # Validate script and task_run extension compatibility
  if (!is.null(script)) {
    script_lower <- tolower(basename(script))
    task_ext <- tolower(tools::file_ext(task_run))

    # Define expected extensions for each interpreter
    expected_exts <- list(
      "rscript.exe" = c("r"),
      "python.exe" = c("py", "pyw"),
      "julia.exe" = c("jl"),
      "perl.exe" = c("pl", "pm"),
      "node.exe" = c("js", "mjs", "cjs"),
      "ts-node.exe" = c("ts"),
      "raku.exe" = c("raku", "rakumod", "pl6", "pm6"),
      "rakudo.exe" = c("raku", "rakumod", "pl6", "pm6")
    )

    # Check if we have validation rules for this interpreter
    if (script_lower %in% names(expected_exts)) {
      valid_exts <- expected_exts[[script_lower]]

      if (!task_ext %in% valid_exts) {
        interpreter_name <- sub("\\.exe$", "", script_lower)
        cli::cli_warn(c(
          "Using {interpreter_name} with a file that doesn't have an expected extension.",
          "i" = "Task file: {.path {task_run}}",
          "i" = "Expected extension(s): {.val {paste0('.', valid_exts)}}"
        ))
      }
    }
  }

  # Normalize and expand execution path
  if (!is.null(exec_path)) {
    # Expand ~ and make absolute, use backslashes for Windows
    exec_path <- normalizePath(exec_path, mustWork = FALSE, winslash = "\\")
  } else {
    # Default: use current working directory
    exec_path <- normalizePath(getwd(), winslash = "\\")
  }

  # Normalize paths for Windows (convert forward slashes to backslashes)
  task_run_normalized <- gsub("/", "\\\\", task_run)

  # Build the command based on whether a script interpreter is specified
  if (!is.null(script)) {
    # Normalize script path for Windows
    script_normalized <- gsub("/", "\\\\", script)

    # Build command: cmd /c "cd /d <exec_path> && <script> <task_run>"
    # Both script and task_run can be absolute or relative paths
    cmd <- sprintf(
      "cmd /c \"cd /d \"%s\" && %s \"%s\"\"",
      exec_path,
      script_normalized,
      task_run_normalized
    )
  } else {
    # No script interpreter - just change directory and run the task
    # task_run can be absolute path, relative path, or executable on PATH
    cmd <- sprintf(
      "cmd /c \"cd /d \"%s\" && \"%s\"\"",
      exec_path,
      task_run_normalized
    )
  }

  cmd
}


#' Quote a task name for command line use
#'
#' @param task_name Character string of the task name.
#'
#' @return The task name, quoted.
#'
#' @noRd
.quote_task_name <- function(task_name) {
  paste0("\"", task_name, "\"")
}


#' Convert column names to snake_case
#'
#' @param df A data frame.
#'
#' @return The data frame with snake_case column names.
#'
#' @noRd
.to_snake_case_names <- function(df) {
  names(df) <- snakecase::to_snake_case(names(df))
  df
}


#' Parse CSV output from schtasks query
#'
#' @param output Character vector of CSV output from schtasks.
#'
#' @return A tibble with parsed task data.
#'
#' @noRd
.parse_query_csv <- function(output) {
  # Join lines and parse as CSV
  csv_text <- paste(output, collapse = "\n")

  df <- readr::read_csv(
    csv_text,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  .to_snake_case_names(df)
}


#' Validate time format (HH:MM)
#'
#' @param time Character string in HH:MM format.
#' @param arg_name Name of the argument for error messages.
#'
#' @return Invisibly returns TRUE if valid, otherwise aborts.
#'
#' @noRd
.validate_time <- function(time, arg_name = "time") {
  checkmate::assert_string(time)

  if (!stringr::str_detect(time, "^([01]?[0-9]|2[0-3]):[0-5][0-9]$")) {
    cli::cli_abort(c(
      "Invalid time format for {.arg {arg_name}}.",
      "x" = "Expected HH:MM in 24-hour format (e.g., '09:00', '14:30').",
      "i" = "Got: {.val {time}}"
    ))
  }

  invisible(TRUE)
}


#' Validate date format
#'
#' Validates date in various formats accepted by schtasks.
#' The exact format depends on system locale, so we accept common formats.
#'
#' @param date Character string or Date object.
#' @param arg_name Name of the argument for error messages.
#'
#' @return Character string in the format expected by schtasks.
#'
#' @noRd
.validate_date <- function(date, arg_name = "date") {
  if (inherits(date, "Date")) {
    # Convert to locale-appropriate format
    # Using format that schtasks typically accepts
    return(format(date, "%Y/%m/%d"))
  }

  checkmate::assert_string(date)

  # Accept various date formats
  date_patterns <- c(
    "^\\d{4}/\\d{2}/\\d{2}$", # YYYY/MM/DD
    "^\\d{2}/\\d{2}/\\d{4}$", # DD/MM/YYYY or MM/DD/YYYY
    "^\\d{4}-\\d{2}-\\d{2}$" # YYYY-MM-DD (ISO)
  )

  if (!any(purrr::map_lgl(date_patterns, \(p) stringr::str_detect(date, p)))) {
    cli::cli_abort(c(
      "Invalid date format for {.arg {arg_name}}.",
      "x" = "Expected a date in YYYY/MM/DD, DD/MM/YYYY, or YYYY-MM-DD format.",
      "i" = "Got: {.val {date}}"
    ))
  }

  date
}


#' Prompt for password interactively
#'
#' @param prompt The prompt message to display.
#'
#' @return The entered password as a character string.
#'
#' @noRd
.prompt_password <- function(prompt = "Enter password: ") {
  if (!interactive()) {
    cli::cli_abort(c(
      "Password required but session is not interactive.",
      "i" = "Run this command in an interactive R session."
    ))
  }

  askpass::askpass(prompt)
}


#' Normalise day of week input
#'
#' @param day Character string of day of week.
#'
#' @return Uppercase three-letter day abbreviation.
#'
#' @noRd
.normalise_day_of_week <- function(day) {
  day <- toupper(day)

  valid_days <- c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN")

  # Also accept full names
  day_map <- c(
    "MONDAY" = "MON",
    "TUESDAY" = "TUE",
    "WEDNESDAY" = "WED",
    "THURSDAY" = "THU",
    "FRIDAY" = "FRI",
    "SATURDAY" = "SAT",
    "SUNDAY" = "SUN"
  )

  if (day %in% names(day_map)) {
    day <- day_map[[day]]
  }

  if (!(day %in% valid_days)) {
    cli::cli_abort(c(
      "Invalid day of week.",
      "x" = "Expected one of: {.val {valid_days}}",
      "i" = "Got: {.val {day}}"
    ))
  }

  day
}

#' Normalise multiple days of week
#'
#' @param days Character vector of days.
#'
#' @return Comma-separated uppercase day abbreviations.
#'
#' @noRd
.normalise_days_of_week <- function(days) {
  normalised <- purrr::map_chr(days, .normalise_day_of_week)
  paste(normalised, collapse = ",")
}


#' Normalise month input
#'
#' @param month Character string or vector of months.
#'
#' @return Comma-separated uppercase month abbreviations.
#'
#' @noRd
.normalise_months <- function(months) {
  valid_months <- c(
    "JAN",
    "FEB",
    "MAR",
    "APR",
    "MAY",
    "JUN",
    "JUL",
    "AUG",
    "SEP",
    "OCT",
    "NOV",
    "DEC"
  )

  month_map <- c(
    "JANUARY" = "JAN",
    "FEBRUARY" = "FEB",
    "MARCH" = "MAR",
    "APRIL" = "APR",
    "MAY" = "MAY",
    "JUNE" = "JUN",
    "JULY" = "JUL",
    "AUGUST" = "AUG",
    "SEPTEMBER" = "SEP",
    "OCTOBER" = "OCT",
    "NOVEMBER" = "NOV",
    "DECEMBER" = "DEC"
  )

  normalise_one <- function(m) {
    if (m == "*") {
      return("*")
    }

    m <- toupper(m)

    if (m %in% names(month_map)) {
      m <- month_map[[m]]
    }

    if (!(m %in% valid_months)) {
      cli::cli_abort(c(
        "Invalid month.",
        "x" = "Expected one of: {.val {valid_months}} or '*' for all months.",
        "i" = "Got: {.val {m}}"
      ))
    }

    m
  }

  normalised <- purrr::map_chr(months, normalise_one)
  paste(normalised, collapse = ",")
}


#' Build common create arguments
#'
#' Factory function to build the argument vector for schtasks /create.
#'
#' @param task_name Name of the task.
#' @param task_run Program/command to run.
#' @param schedule_type Schedule type (MINUTE, HOURLY, etc.).
#' @param modifier Optional modifier.
#' @param day Optional day specification.
#' @param months Optional month specification.
#' @param idle_time Optional idle time (for ONIDLE).
#' @param start_time Optional start time (HH:MM).
#' @param end_time Optional end time (HH:MM).
#' @param duration Optional duration (HHHH:MM).
#' @param interval Optional repetition interval in minutes.
#' @param start_date Optional start date.
#' @param end_date Optional end date.
#' @param run_level Run level (LIMITED or HIGHEST).
#' @param run_as_user User account to run the task as.
#' @param kill_on_end Whether to kill the task at end time.
#' @param delete_when_done Whether to delete the task after completion.
#' @param force Whether to suppress warnings if task exists.
#' @param interactive_only Whether task runs only when user is logged on.
#'
#' @return Character vector of arguments for schtasks.
#'
#' @noRd
.build_create_args <- function(
  task_name,
  task_run,
  schedule_type,
  modifier = NULL,
  day = NULL,
  months = NULL,
  idle_time = NULL,
  start_time = NULL,
  end_time = NULL,
  duration = NULL,
  interval = NULL,
  start_date = NULL,
  end_date = NULL,
  run_level = NULL,
  run_as_user = NULL,
  kill_on_end = FALSE,
  delete_when_done = FALSE,
  force = FALSE,
  interactive_only = FALSE
) {
  # Validate required arguments
  .validate_task_name(task_name)
  checkmate::assert_string(task_run, min.chars = 1L)
  checkmate::assert_string(schedule_type)

  args <- c(
    "/create",
    "/sc",
    schedule_type,
    "/tn",
    .quote_task_name(task_name),
    "/tr",
    .quote_path(task_run)
  )

  # Add optional arguments
  if (!is.null(modifier)) {
    args <- c(args, "/mo", as.character(modifier))
  }

  if (!is.null(day)) {
    args <- c(args, "/d", day)
  }

  if (!is.null(months)) {
    args <- c(args, "/m", months)
  }

  if (!is.null(idle_time)) {
    checkmate::assert_integerish(idle_time, lower = 1L, upper = 999L, len = 1L)
    args <- c(args, "/i", as.character(idle_time))
  }

  if (!is.null(start_time)) {
    .validate_time(start_time, "start_time")
    args <- c(args, "/st", start_time)
  }

  if (!is.null(end_time)) {
    .validate_time(end_time, "end_time")
    args <- c(args, "/et", end_time)
  }

  if (!is.null(duration)) {
    args <- c(args, "/du", duration)
  }

  if (!is.null(interval)) {
    checkmate::assert_integerish(
      interval,
      lower = 1L,
      upper = 599940L,
      len = 1L
    )
    args <- c(args, "/ri", as.character(interval))
  }

  if (!is.null(start_date)) {
    start_date <- .validate_date(start_date, "start_date")
    args <- c(args, "/sd", start_date)
  }

  if (!is.null(end_date)) {
    end_date <- .validate_date(end_date, "end_date")
    args <- c(args, "/ed", end_date)
  }

  if (!is.null(run_level)) {
    run_level <- toupper(run_level)
    checkmate::assert_choice(run_level, c("LIMITED", "HIGHEST"))
    args <- c(args, "/rl", run_level)
  }

  if (!is.null(run_as_user)) {
    args <- c(args, "/ru", run_as_user)
  }

  if (kill_on_end) {
    args <- c(args, "/k")
  }

  if (delete_when_done) {
    args <- c(args, "/z")
  }

  if (force) {
    args <- c(args, "/f")
  }

  if (interactive_only) {
    args <- c(args, "/it")
  }

  # TODO: Add /xml parameter for XML file import
  # TODO: Add /s, /u, /p parameters for remote computer support
  # TODO: Add /np parameter for no password storage
  # TODO: Add /delay parameter for ONSTART/ONLOGON/ONEVENT

  args
}
