#' Stop a running scheduled task
#'
#' Stops a scheduled task that is currently running.
#'
#' @param task_name Character string. Name of the task to stop. For tasks in
#'   subfolders, include the full path (e.g., `"\\MyFolder\\MyTask"`).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @details
#' This function sends a termination signal to the program started by the
#' scheduled task. It only affects the current instance of the program; the
#' task will run again at its next scheduled time.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Stop a running task
#' schtask_end("MyLongRunningTask")
#' }
schtask_end <- function(task_name) {
  .validate_task_name(task_name)

  args <- c(
    "/end",
    "/tn", .quote_task_name(task_name)
  )

  # TODO: Add /s, /u, /p parameters for remote computer support

  .execute_schtasks(args, error_msg = "Failed to stop scheduled task")
  cli::cli_alert_success("Stopped scheduled task: {.val {task_name}}")

  invisible(TRUE)
}
