#' Run a scheduled task immediately
#'
#' Starts a scheduled task immediately, ignoring its normal schedule. The task's
#' schedule is not affected; it will continue to run at its scheduled times.
#'
#' @param task_name Character string. Name of the task to run. For tasks in
#'   subfolders, include the full path (e.g., `"\\MyFolder\\MyTask"`).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @details
#' This function starts the task immediately using the program location, user
#' account, and password saved in the task definition. It does not change the
#' next scheduled run time.
#'
#' If the task fails to run, check the Task Scheduler log at
#' `%SystemRoot%\\SchedLgU.txt` for error details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run a task immediately
#' schtask_run("MyBackupTask")
#'
#' # Run a task in a subfolder
#' schtask_run("\\Microsoft\\Windows\\Defrag\\ScheduledDefrag")
#' }
schtask_run <- function(task_name) {
  .validate_task_name(task_name)

  args <- c(
    "/run",
    "/tn", .quote_task_name(task_name)
  )

  # TODO: Add /s, /u, /p parameters for remote computer support

  .execute_schtasks(args, error_msg = "Failed to run scheduled task")
  cli::cli_alert_success("Started scheduled task: {.val {task_name}}")

  invisible(TRUE)
}
