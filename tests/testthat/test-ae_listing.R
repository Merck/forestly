test_that("propercase() converts character vectors correctly", {
  expect_equal(propercase("MILD"), "Mild")
  expect_equal(propercase(c("MILD", "MODERATE", "SEVERE")), 
               c("Mild", "Moderate", "Severe"))
})

test_that("propercase() handles factors correctly", {
  test_factor <- factor(c("MILD", "MODERATE", "SEVERE"))
  result <- propercase(test_factor)
  expect_type(result, "character")
  expect_equal(result, c("Mild", "Moderate", "Severe"))
})

test_that("titlecase() converts character vectors correctly", {
  expect_equal(titlecase("AMERICAN INDIAN OR ALASKA NATIVE"), 
               "American Indian or Alaska Native")
  expect_equal(titlecase(c("tHEre is oNe", "tHAt is tWo")),
               c("There is One", "That is Two"))
})

test_that("titlecase() handles factors correctly", {
  test_factor <- factor(c("MILD", "MODERATE"))
  result <- titlecase(test_factor)
  expect_type(result, "character")
  expect_equal(result, c("Mild", "Moderate"))
})

test_that("titlecase() handles lower parameter correctly", {
  test_string <- "The Quick BROWN Fox"
  expect_equal(titlecase(test_string, lower = TRUE), "The Quick Brown Fox")
  expect_equal(titlecase(test_string, lower = FALSE), "The Quick BROWN Fox")
})