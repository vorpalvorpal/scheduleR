testthat::test_that(".normalise_day_of_week accepts abbreviated days", {
  testthat::expect_equal(.normalise_day_of_week("MON"), "MON")
  testthat::expect_equal(.normalise_day_of_week("TUE"), "TUE")
  testthat::expect_equal(.normalise_day_of_week("WED"), "WED")
  testthat::expect_equal(.normalise_day_of_week("THU"), "THU")
  testthat::expect_equal(.normalise_day_of_week("FRI"), "FRI")
  testthat::expect_equal(.normalise_day_of_week("SAT"), "SAT")
  testthat::expect_equal(.normalise_day_of_week("SUN"), "SUN")
})

testthat::test_that(".normalise_day_of_week normalises case", {
  testthat::expect_equal(.normalise_day_of_week("mon"), "MON")
  testthat::expect_equal(.normalise_day_of_week("Mon"), "MON")
  testthat::expect_equal(.normalise_day_of_week("friday"), "FRI")
  testthat::expect_equal(.normalise_day_of_week("Friday"), "FRI")
})

testthat::test_that(".normalise_day_of_week accepts full day names", {
  testthat::expect_equal(.normalise_day_of_week("MONDAY"), "MON")
  testthat::expect_equal(.normalise_day_of_week("monday"), "MON")
  testthat::expect_equal(.normalise_day_of_week("Tuesday"), "TUE")
  testthat::expect_equal(.normalise_day_of_week("WEDNESDAY"), "WED")
  testthat::expect_equal(.normalise_day_of_week("thursday"), "THU")
  testthat::expect_equal(.normalise_day_of_week("FRIDAY"), "FRI")
  testthat::expect_equal(.normalise_day_of_week("Saturday"), "SAT")
  testthat::expect_equal(.normalise_day_of_week("sunday"), "SUN")
})

testthat::test_that(".normalise_day_of_week rejects invalid days", {
  testthat::expect_error(.normalise_day_of_week("MOND"), "Invalid day of week")
  testthat::expect_error(.normalise_day_of_week("Tues"), "Invalid day of week")
  testthat::expect_error(.normalise_day_of_week("1"), "Invalid day of week")
  testthat::expect_error(.normalise_day_of_week(""), "Invalid day of week")
})

testthat::test_that(".normalise_days_of_week handles multiple days", {
  testthat::expect_equal(
    .normalise_days_of_week(c("MON", "WED", "FRI")),
    "MON,WED,FRI"
  )
  testthat::expect_equal(
    .normalise_days_of_week(c("monday", "wednesday", "friday")),
    "MON,WED,FRI"
  )
  testthat::expect_equal(
    .normalise_days_of_week("TUE"),
    "TUE"
  )
})

testthat::test_that(".normalise_months accepts abbreviated months", {
  testthat::expect_equal(.normalise_months("JAN"), "JAN")
  testthat::expect_equal(.normalise_months("FEB"), "FEB")
  testthat::expect_equal(.normalise_months("DEC"), "DEC")
})

testthat::test_that(".normalise_months normalises case", {
  testthat::expect_equal(.normalise_months("jan"), "JAN")
  testthat::expect_equal(.normalise_months("Jan"), "JAN")
  testthat::expect_equal(.normalise_months("december"), "DEC")
})

testthat::test_that(".normalise_months accepts full month names", {
  testthat::expect_equal(.normalise_months("JANUARY"), "JAN")
  testthat::expect_equal(.normalise_months("february"), "FEB")
  testthat::expect_equal(.normalise_months("March"), "MAR")
  testthat::expect_equal(.normalise_months("DECEMBER"), "DEC")
})

testthat::test_that(".normalise_months accepts wildcard", {
  testthat::expect_equal(.normalise_months("*"), "*")
})

testthat::test_that(".normalise_months handles multiple months", {
  testthat::expect_equal(
    .normalise_months(c("JAN", "APR", "JUL", "OCT")),
    "JAN,APR,JUL,OCT"
  )
  testthat::expect_equal(
    .normalise_months(c("january", "july")),
    "JAN,JUL"
  )
})

testthat::test_that(".normalise_months rejects invalid months", {
  testthat::expect_error(.normalise_months("JANU"), "Invalid month")
  testthat::expect_error(.normalise_months("13"), "Invalid month")
  testthat::expect_error(.normalise_months(""), "Invalid month")
})
