#' Delete a scheduled task
#'
#' Removes a scheduled task from Windows Task Scheduler.
#'
#' @param task_name Character string. Name of the task to delete. For tasks in
#'   subfolders, include the full path (e.g., `"\\MyFolder\\MyTask"`).
#' @param confirm Logical. If `FALSE` (default), the task is deleted without
#'   prompting for confirmation. If `TRUE`, you will be asked to confirm deletion
#'   (only works in interactive sessions).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Delete a task without confirmation
#' schtask_delete("MyOldTask")
#'
#' # Delete with confirmation
#' schtask_delete("ImportantTask", confirm = TRUE)
#'
#' # Delete a task in a subfolder
#' schtask_delete("\\MyFolder\\SubTask")
#' }
schtask_delete <- function(task_name, confirm = FALSE) {
  .validate_task_name(task_name)
  checkmate::assert_flag(confirm)

  if (confirm && interactive()) {
    response <- readline(
      sprintf("Delete scheduled task '%s'? (y/N): ", task_name)
    )
    if (!tolower(response) %in% c("y", "yes")) {
      cli::cli_alert_info("Deletion cancelled.")
      return(invisible(FALSE))
    }
  }

  args <- c(
    "/delete",
    "/tn", .quote_task_name(task_name),
    "/f"  # Force deletion without prompting
  )

  # TODO: Add /s, /u, /p parameters for remote computer support

  .execute_schtasks(args, error_msg = "Failed to delete scheduled task")
  cli::cli_alert_success("Deleted scheduled task: {.val {task_name}}")

  invisible(TRUE)
}
