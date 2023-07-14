test_that("use snapshot for testing", {
  js_notxt <- sparkline_point_js(iris, "Sepal.Length")
  expect_snapshot_output(js_notxt)
})
