# Create input object
adsl <- r2rtf::r2rtf_adsl |> dplyr::mutate(TRTA = TRT01A)
adae <- r2rtf::r2rtf_adae

# Test 1
test1 <- meta_forestly(
  adsl,
  adae,
  population_term = "apat",
  observation_term = "wk12",
  parameter_term = "any;rel;ser"
)

test_that("There is 1 analysis in the output: ae_forestly", {
  expect_equal(names(test1$analysis), c("ae_forestly"))
})

test_that("There is 1 population: apat", {
  expect_equal(names(test1$population), "apat")
})

test_that("There is 1 observation: wk12", {
  expect_equal(names(test1$observation), c("wk12"))
})

test_that("There are 3 observations: any, rel, ser", {
  expect_equal(names(test1$parameter), c("any", "rel", "ser"))
})

test_that("The output is a list", {
  expect_type(test1, "list")
})

# Test 2
test2 <- meta_forestly(adsl, adae,
  population_term = "trt",
  population_subset = TRTFL == "Y",
  observation_term = "trtem",
  observation_subset = TRTEMFL == "Y" & TRTFL == "Y",
  parameter_term = "any;g35"
)

test_that("There is 1 population: trt", {
  expect_equal(names(test2$population), "trt")
})

test_that("There is 1 population subset criteria: TRTFL == Y", {
  expect_equal(test2$population$trt$subset, quote(TRTFL == "Y"))
})

test_that("There is 1 observation: trtem", {
  expect_equal(names(test2$observation), "trtem")
})

test_that("There is 1 observation subset criteria: TREMTFL == Y & TRTFL == Y", {
  expect_equal(test2$observation$trtem$subset, quote(TRTEMFL == "Y" & TRTFL == "Y"))
})

test_that("There are 2 observations: any, g35", {
  expect_equal(names(test2$parameter), c("any", "g35"))
})
