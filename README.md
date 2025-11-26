# scheduleR

An R interface to Windows Task Scheduler for scheduling scripts and programs via the `schtasks` command-line tool.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("vorpalvorpal/scheduleR")
```

## Overview
scheduleR provides R functions to create, query, modify, and manage scheduled tasks on Windows systems. It wraps the Windows `schtasks.exe` command-line utility. The package is designed for scheduling R scripts but can also schedule other scripts (Python, batch files, etc.) by specifying a custom script interpreter.

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

Different functions are available for different schedule types. By default, tasks use `Rscript.exe` from the current R installation to run scripts from the project root (determined by `here::here()`):

```r
# Run an R script every 30 minutes (uses Rscript.exe by default)
schtask_create_minute("DataRefresh", "refresh_data.R", every = 30)

# Run every 2 hours from a specific directory
schtask_create_hourly("HourlyCheck", "check_status.R", every = 2,
                      exec_path = "C:/Projects/analysis")

# Run daily at 3am
schtask_create_daily("NightlyBackup", "backup.R", start_time = "03:00")

# Run every weekday
schtask_create_weekly("WeekdayReport", "daily_report.R",
                      days = c("MON", "TUE", "WED", "THU", "FRI"),
                      start_time = "08:00")

# Run on the 15th of every month
schtask_create_monthly("MonthlyReport", "monthly_report.R",
                       day = 15, start_time = "09:00")

# Run on the second Tuesday of every month
schtask_create_monthly("SecondTuesday", "meeting_prep.R",
                       modifier = "SECOND", day = "TUE", start_time = "10:00")

# Run once at a specific time
schtask_create_once("OneTimeSetup", "setup.R",
                    start_time = "14:30", start_date = "2024/06/15")

# Run at system startup
schtask_create_on_start("StartupTask", "startup.R")

# Run when a user logs on
schtask_create_on_logon("LogonTask", "logon.R")

# Run when system is idle for 10 minutes
schtask_create_on_idle("IdleMaintenance", "maintenance.R", idle_time = 10)

# Run a Python script every hour (specify full path to python.exe)
schtask_create_hourly("PythonTask", "script.py",
                      script = "C:/Python311/python.exe", every = 1)

# Run a Julia script daily
schtask_create_daily("JuliaTask", "analysis.jl",
                     script = "C:/Users/user/AppData/Local/Programs/Julia-1.9.0/bin/julia.exe",
                     start_time = "08:00")

# Run a batch file without a script interpreter
schtask_create_daily("BatchTask", "backup.bat",
                     script = NULL, start_time = "02:00")
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
- here
- purrr
- readr
- rlang
- snakecase
- stringr
- tibble

## Notes

- Task names must be 238 characters or fewer and cannot contain: `< > : " / \ | ? *`
- By default, scripts are executed using `Rscript.exe` from the current R installation (`file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")`) from the project root (`here::here()`)
- **Path handling**:
  - `task_run` and `script` can be absolute paths, relative paths (interpreted relative to `exec_path`), or executable names on PATH
  - `exec_path` is automatically expanded (e.g., `~` becomes the user's home directory) and normalized for Windows
  - Relative paths are preferred to avoid Windows command length limits
- Use the `script` parameter to specify a script interpreter (e.g., `"C:/Python311/python.exe"` or `"python"` if on PATH)
- Set `script = NULL` to run executables directly (e.g., batch files, programs on PATH)
- The package validates file extensions against common interpreters:
  - **Rscript.exe**: expects `.R` or `.r` files
  - **python.exe**: expects `.py` or `.pyw` files
  - **julia.exe**: expects `.jl` files
  - **perl.exe**: expects `.pl` or `.pm` files
  - **node.exe**: expects `.js`, `.mjs`, or `.cjs` files
  - **ts-node.exe**: expects `.ts` files
  - **raku.exe/rakudo.exe**: expects `.raku`, `.rakumod`, `.pl6`, or `.pm6` files
- Warnings are issued when file extensions don't match the expected interpreter
- The scheduled task uses `cmd /c "cd /d <exec_path> && <script> <task_file>"` to run tasks
- Passwords are prompted interactively when required (hidden input via `askpass`)

## Future Development

- Remote computer support (currently local only)
- XML import/export
- ONEVENT schedule type

## Licence

MIT
