# scheduleR

An R interface to Windows Task Scheduler via the `schtasks` command-line tool.

## Installation

```r
# Install from local source
install.packages("path/to/scheduleR", repos = NULL, type = "source")
```

## Overview
scheduleR provides R functions to create, query, modify, and manage scheduled tasks on Windows systems. It wraps the Windows `schtasks.exe` command-line utility.

## Functions

### Query Tasks

```r
# List all scheduled tasks (verbose by default)
tasks <- schtask_query()

# List with basic information only
tasks <- schtask_query(verbose = FALSE)

# Query a specific task
task <- schtask_query(task_name = "MyTask")
```

### Create Tasks

Different functions are available for different schedule types:

```r
# Run every 30 minutes
schtask_create_minute("MinuteTask", "C:\\Scripts\\script.bat", every = 30)

# Run every 2 hours
schtask_create_hourly("HourlyTask", "C:\\Scripts\\script.bat", every = 2)

# Run daily at 3am
schtask_create_daily("DailyTask", "C:\\Scripts\\backup.bat", start_time = "03:00")

# Run every weekday
schtask_create_weekly("WeekdayTask", "C:\\Scripts\\sync.exe",
                      days = c("MON", "TUE", "WED", "THU", "FRI"),
                      start_time = "08:00")

# Run on the 15th of every month
schtask_create_monthly("MonthlyTask", "C:\\Scripts\\report.exe",
                       day = 15, start_time = "09:00")

# Run on the second Tuesday of every month
schtask_create_monthly("SecondTuesday", "C:\\Scripts\\meeting.exe",
                       modifier = "SECOND", day = "TUE", start_time = "10:00")

# Run once at a specific time
schtask_create_once("OneTime", "C:\\Scripts\\setup.exe",
                    start_time = "14:30", start_date = "2024/06/15")

# Run at system startup
schtask_create_on_start("StartupTask", "C:\\Scripts\\startup.exe")

# Run when a user logs on
schtask_create_on_logon("LogonTask", "C:\\Scripts\\logon.bat")

# Run when system is idle
schtask_create_on_idle("IdleTask", "C:\\Scripts\\maintenance.exe", idle_time = 10)
```

### Modify Tasks

```r
# Change the program a task runs
schtask_change("MyTask", task_run = "C:\\NewPath\\newprogram.exe")

# Change the start time
schtask_change("MyTask", start_time = "08:30")

# Disable a task
schtask_change("MyTask", enable = FALSE)

# Enable a task
schtask_change("MyTask", enable = TRUE)
```

### Run and Stop Tasks

```r
# Run a task immediately (ignores schedule)
schtask_run("MyTask")

# Stop a running task
schtask_end("MyTask")
```

### Delete Tasks

```r
# Delete a task
schtask_delete("MyTask")

# Delete with confirmation prompt
schtask_delete("ImportantTask", confirm = TRUE)
```

## Requirements

- Windows operating system
- Administrator privileges for most operations
- R 4.0.0 or later

## Dependencies

- askpass
- checkmate
- cli
- dplyr
- purrr
- readr
- rlang
- snakecase
- stringr
- tibble

## Notes

- Task names must be 238 characters or fewer and cannot contain: `< > : " / \ | ? *`
- All file paths should use full paths and will be automatically quoted
- Passwords are prompted interactively when required (hidden input via `askpass`)

## Future Development

- Remote computer support (currently local only)
- XML import/export
- ONEVENT schedule type

## Licence

MIT
