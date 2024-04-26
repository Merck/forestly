test_that("There is 1 analysis in the output: ae_forestly", {
  test1 <- test_meta_forestly_1()
  expect_equal(names(test1$analysis), c("ae_forestly"))
})

test_that("There is 1 population: apat", {
  test1 <- test_meta_forestly_1()
  expect_equal(names(test1$population), "apat")
})

test_that("There is 1 observation: wk12", {
  test1 <- test_meta_forestly_1()
  expect_equal(names(test1$observation), c("wk12"))
})

test_that("There are 3 observations: any, rel, ser", {
  test1 <- test_meta_forestly_1()
  expect_equal(names(test1$parameter), c("any", "rel", "ser"))
})

test_that("The output is a list", {
  test1 <- test_meta_forestly_1()
  expect_type(test1, "list")
})

test_that("There is 1 population: trt", {
  test2 <- test_meta_forestly_2()
  expect_equal(names(test2$population), "trt")
})

test_that("There is 1 population subset criteria: TRTFL == Y", {
  test2 <- test_meta_forestly_2()
  expect_equal(test2$population$trt$subset, quote(TRTFL == "Y"))
})

test_that("There is 1 observation: trtem", {
  test2 <- test_meta_forestly_2()
  expect_equal(names(test2$observation), "trtem")
})

test_that("There is 1 observation subset criteria: TREMTFL == Y & TRTFL == Y", {
  test2 <- test_meta_forestly_2()
  expect_equal(test2$observation$trtem$subset, quote(TRTEMFL == "Y" & TRTFL == "Y"))
})

test_that("There are 2 observations: any, g35", {
  test2 <- test_meta_forestly_2()
  expect_equal(names(test2$parameter), c("any", "g35"))
})
