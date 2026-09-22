test_that("use snapshot for testing", {
  js_notxt <- sparkline_point_js(iris, "Sepal.Length")
  expect_snapshot_output(js_notxt)
})

test_that("cell sparklines do not draw the x-axis line or ticks", {
  # Cell plots have a fixed height, so an axis line would appear as a stray
  # horizontal rule inside the table row. Only the footer/header axis should
  # render the line and ticks.
  js_cell <- sparkline_point_js(iris, "Sepal.Length", type = "cell")
  expect_match(js_cell, '"showline": false', fixed = TRUE)
  expect_match(js_cell, '"ticks": ""', fixed = TRUE)

  js_footer <- sparkline_point_js(iris, "Sepal.Length", type = "footer")
  expect_match(js_footer, '"showline": true', fixed = TRUE)
  expect_match(js_footer, '"ticks": "outside"', fixed = TRUE)
})
