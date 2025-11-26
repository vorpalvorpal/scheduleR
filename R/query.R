#' Query scheduled tasks
#'
#' Retrieves information about scheduled tasks from Windows Task Scheduler.
#'
#' @param task_name Optional character string specifying a single task to query.
#'   If `NULL` (default), all tasks are returned. Task names in subfolders should
#'
#'   include the full path (e.g., `"\\Microsoft\\Office\\Office Subscription Maintenance"`).
#' @param verbose Logical. If `TRUE` (default), returns detailed task information
#'   including run times, last result, user info, etc. If `FALSE`, returns only
#'   basic information (task name, next run time, status).
#'
#' @return A tibble containing task information. The columns depend on the `verbose`
#'   parameter:
#'
#'   **When `verbose = FALSE`:**
#'
#'   * `task_name`: Name of the scheduled task
#'   * `next_run_time`: Next scheduled run time
#'   * `status`: Current status of the task
#'
#'   **When `verbose = TRUE`:** Additional columns including `last_run_time`,
#'   `last_result`, `author`, `task_to_run`, `start_in`, `comment`,
#'   `scheduled_task_state`, `idle_time`, `power_management`, `run_as_user`,
#'
#'   `delete_task_if_not_rescheduled`, `stop_task_if_runs_x_hours_and_x_mins`,
#'   `schedule`, `schedule_type`, `start_time`, `start_date`, `end_date`,
#'   `days`, `months`, `repeat_every`, `repeat_until_time`, `repeat_until_duration`,
#'   `repeat_stop_if_still_running`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Query all tasks with verbose output
#' tasks <- schtask_query()
#'
#' # Query all tasks with basic output
#' tasks <- schtask_query(verbose = FALSE)
#'
#' # Query a specific task
#' task <- schtask_query(task_name = "MyBackupTask")
#'
#' # Query a task in a subfolder
#' task <- schtask_query(task_name = "\\Microsoft\\Windows\\Defrag\\ScheduledDefrag")
#' }
schtask_query <- function(task_name = NULL, verbose = TRUE) {
  checkmate::assert_string(task_name, null.ok = TRUE)
  checkmate::assert_flag(verbose)

  args <- c("/query", "/fo", "CSV")

  if (!verbose)
    args <- c(args, "/nh")
  else
    args <- c(args, "/v")

  if (!is.null(task_name)) {
    .validate_task_name(task_name)
    args <- c(args, "/tn", .quote_task_name(task_name))
  }

  # TODO: Add /s, /u, /p parameters for remote computer support
  # TODO: Add /xml parameter for XML export

  result <- .execute_schtasks(args, error_msg = "Failed to query scheduled tasks")

  if (!verbose) {
    # Non-verbose output doesn't have headers when using /nh,
    # so we need to parse differently
    output_lines <- result$output

    if (length(output_lines) == 0L)
      return(tibble::tibble(
        task_name = character(),
        next_run_time = character(),
        status = character()
      ))

    # Parse CSV without headers
    csv_text <- paste(output_lines, collapse = "\n")
    df <- readr::read_csv(
      csv_text,
      col_names = c("task_name", "next_run_time", "status"),
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )

    return(df)
  }

  # Verbose output has headers
  .parse_query_csv(result$output)
}
