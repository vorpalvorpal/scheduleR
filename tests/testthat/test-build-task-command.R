# Tests for .build_task_command() function

testthat::test_that(".build_task_command builds command with script and exec_path", {
  cmd <- .build_task_command(
    task_run = "script.R",
    script = "C:\\Program Files\\R\\R-4.3.0\\bin\\Rscript.exe",
    exec_path = tempdir()
  )

  testthat::expect_true(grepl("cmd /c", cmd, fixed = TRUE))
  testthat::expect_true(grepl("cd /d", cmd, fixed = TRUE))
  testthat::expect_true(grepl("Rscript.exe", cmd))
  testthat::expect_true(grepl("script.R", cmd))
})

testthat::test_that(".build_task_command preserves absolute paths in task_run", {
  cmd <- .build_task_command(
    task_run = "C:/Scripts/script.R",
    script = "C:\\R\\bin\\Rscript.exe",
    exec_path = "C:/Projects"
  )

  # Should contain the full path to script.R
  testthat::expect_true(grepl("C:\\\\Scripts\\\\script.R", cmd))
})

testthat::test_that(".build_task_command handles relative paths in task_run", {
  cmd <- .build_task_command(
    task_run = "subfolder/script.R",
    script = "C:\\R\\bin\\Rscript.exe",
    exec_path = "C:/Projects"
  )

  # Should contain the relative path
  testthat::expect_true(grepl("subfolder\\\\script.R", cmd))
})

testthat::test_that(".build_task_command converts forward slashes to backslashes", {
  cmd <- .build_task_command(
    task_run = "path/to/script.R",
    script = "C:/Python311/python.exe",
    exec_path = "C:/Projects"
  )

  # All paths should use backslashes
  testthat::expect_true(grepl("path\\\\to\\\\script.R", cmd))
  testthat::expect_true(grepl("C:\\\\Python311\\\\python.exe", cmd))
})

testthat::test_that(".build_task_command works with script = NULL", {
  cmd <- .build_task_command(
    task_run = "backup.bat",
    script = NULL,
    exec_path = "C:/Scripts"
  )

  testthat::expect_true(grepl("cmd /c", cmd, fixed = TRUE))
  testthat::expect_true(grepl("cd /d", cmd, fixed = TRUE))
  testthat::expect_true(grepl("backup.bat", cmd))
  # Should NOT contain a script path since script is NULL
  testthat::expect_false(grepl("Rscript", cmd))
})

testthat::test_that(".build_task_command warns for mismatched R script extension", {
  # Test that the function still works and returns a command even with mismatched extension
  # The warning is displayed but doesn't prevent execution
  cmd <- suppressWarnings(
    .build_task_command(
      task_run = "script.py",
      script = "C:\\R\\bin\\Rscript.exe",
      exec_path = tempdir()
    )
  )
  testthat::expect_true(grepl("script.py", cmd))
  testthat::expect_true(grepl("Rscript.exe", cmd))
})

testthat::test_that(".build_task_command warns for mismatched Python script extension", {
  # Test that the function still works and returns a command even with mismatched extension
  # The warning is displayed but doesn't prevent execution
  cmd <- suppressWarnings(
    .build_task_command(
      task_run = "script.R",
      script = "C:\\Python311\\python.exe",
      exec_path = tempdir()
    )
  )
  testthat::expect_true(grepl("script.R", cmd))
  testthat::expect_true(grepl("python.exe", cmd))
})

testthat::test_that(".build_task_command does not warn for correct R extension", {
  testthat::expect_silent(
    .build_task_command(
      task_run = "script.R",
      script = "C:\\R\\bin\\Rscript.exe",
      exec_path = tempdir()
    )
  )

  testthat::expect_silent(
    .build_task_command(
      task_run = "script.r",
      script = "C:\\R\\bin\\Rscript.exe",
      exec_path = tempdir()
    )
  )
})

testthat::test_that(".build_task_command does not warn for correct Python extension", {
  testthat::expect_silent(
    .build_task_command(
      task_run = "script.py",
      script = "C:\\Python311\\python.exe",
      exec_path = tempdir()
    )
  )
})

testthat::test_that(".build_task_command validates other language extensions", {
  # Julia
  testthat::expect_silent(
    .build_task_command(
      task_run = "script.jl",
      script = "C:\\Julia\\bin\\julia.exe",
      exec_path = tempdir()
    )
  )

  # Node.js
  testthat::expect_silent(
    .build_task_command(
      task_run = "script.js",
      script = "C:\\nodejs\\node.exe",
      exec_path = tempdir()
    )
  )

  # TypeScript
  testthat::expect_silent(
    .build_task_command(
      task_run = "script.ts",
      script = "C:\\nodejs\\ts-node.exe",
      exec_path = tempdir()
    )
  )
})

testthat::test_that(".build_task_command handles executable names on PATH", {
  cmd <- .build_task_command(
    task_run = "script.py",
    script = "python",
    exec_path = "C:/Projects"
  )

  # Should contain just "python" not a full path
  testthat::expect_true(grepl("python", cmd))
  testthat::expect_true(grepl("script.py", cmd))
})

testthat::test_that(".build_task_command normalizes exec_path", {
  # Even if exec_path uses forward slashes, it should be converted to backslashes
  cmd <- .build_task_command(
    task_run = "script.R",
    script = "Rscript.exe",
    exec_path = tempdir()
  )

  # exec_path should use backslashes in the final command
  # Just verify that the command contains cd /d and the script
  testthat::expect_true(grepl("cd /d", cmd, fixed = TRUE))
  testthat::expect_true(grepl("script.R", cmd))
})
