#' Modify a scheduled task
#'
#' Changes properties of an existing scheduled task. Only specified parameters
#' are modified; all other properties remain unchanged.
#'
#' @param task_name Character string. Name of the task to modify. For tasks in
#'   subfolders, include the full path (e.g., `"\\MyFolder\\MyTask"`).
#' @param task_run Character string. New program or command to run. Optional.
#' @param start_time Character string. New start time in HH:MM 24-hour format. Optional.
#' @param end_time Character string. New end time in HH:MM 24-hour format. Optional.
#' @param duration Character string. New maximum duration in HHHH:MM format. Optional.
#'   Cannot be used with `end_time`.
#' @param interval Integer. New repetition interval in minutes (1-599940). Optional.
#' @param start_date Date or character string. New start date. Optional.
#' @param end_date Date or character string. New end date. Optional.
#' @param run_level Character string. New run level: `"LIMITED"` or `"HIGHEST"`. Optional.
#' @param run_as_user Character string. New user account to run the task.
#'   If specified, you will be prompted for the password unless `run_as_user`
#'   is `"SYSTEM"`. Optional.
#' @param run_as_password Character string. Password for the `run_as_user` account.
#'   If `NULL` and `run_as_user` is specified (and not `"SYSTEM"`), you will be
#'   prompted for the password. Optional.
#' @param enable Logical. If `TRUE`, enables the task. If `FALSE`, disables the task.
#'   If `NULL` (default), the enabled state is not changed. Optional.
#' @param kill_on_end Logical. If `TRUE`, stops the program when end time or
#'   duration is reached. Default is `FALSE`.
#' @param delete_when_done Logical. If `TRUE`, marks the task to be deleted
#'   after completion. Default is `FALSE`.
#' @param interactive_only Logical. If `TRUE`, the task only runs when the
#'   user is logged on. Default is `FALSE`.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Change the program a task runs
#' schtask_change("MyTask", task_run = "C:\\NewPath\\newprogram.exe")
#'
#' # Change the start time
#' schtask_change("MyTask", start_time = "08:30")
#'
#' # Disable a task
#' schtask_change("MyTask", enable = FALSE)
#'
#' # Change the user account (will prompt for password)
#' schtask_change("MyTask", run_as_user = "DOMAIN\\NewUser")
#'
#' # Change to run as SYSTEM (no password required)
#' schtask_change("MyTask", run_as_user = "SYSTEM")
#' }
schtask_change <- function(
    task_name,
    task_run = NULL,
    start_time = NULL,
    end_time = NULL,
    duration = NULL,
    interval = NULL,
    start_date = NULL,
    end_date = NULL,
    run_level = NULL,
    run_as_user = NULL,
    run_as_password = NULL,
    enable = NULL,
    kill_on_end = FALSE,
    delete_when_done = FALSE,
    interactive_only = FALSE
) {
  .validate_task_name(task_name)

  if (!is.null(end_time) && !is.null(duration))
    cli::cli_abort("Cannot specify both {.arg end_time} and {.arg duration}.")

  args <- c("/change", "/tn", .quote_task_name(task_name))

  # Add optional modifications
  if (!is.null(task_run)) {
    checkmate::assert_string(task_run, min.chars = 1L)
    args <- c(args, "/tr", .quote_path(task_run))
  }

  if (!is.null(start_time)) {
    .validate_time(start_time, "start_time")
    args <- c(args, "/st", start_time)
  }

  if (!is.null(end_time)) {
    .validate_time(end_time, "end_time")
    args <- c(args, "/et", end_time)
  }

  if (!is.null(duration))
    args <- c(args, "/du", duration)

  if (!is.null(interval)) {
    checkmate::assert_integerish(interval, lower = 1L, upper = 599940L, len = 1L)
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
    checkmate::assert_string(run_as_user, min.chars = 1L)
    args <- c(args, "/ru", run_as_user)

    # Handle password for non-SYSTEM accounts
    if (!toupper(run_as_user) %in% c("SYSTEM", "NT AUTHORITY\\SYSTEM")) {
      if (is.null(run_as_password))
        run_as_password <- .prompt_password(
          sprintf("Enter password for %s: ", run_as_user)
        )

      args <- c(args, "/rp", run_as_password)
    }
  }

  if (!is.null(enable)) {
    checkmate::assert_flag(enable)
    if (enable) {
      args <- c(args, "/enable")
    } else {
      args <- c(args, "/disable")
    }
  }

  if (kill_on_end)
    args <- c(args, "/k")

  if (delete_when_done)
    args <- c(args, "/z")

  if (interactive_only)
    args <- c(args, "/it")

  # TODO: Add /s, /u, /p parameters for remote computer support

  .execute_schtasks(args, error_msg = "Failed to modify scheduled task")
  cli::cli_alert_success("Modified scheduled task: {.val {task_name}}")

  invisible(TRUE)
}
