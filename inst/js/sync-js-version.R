url <- "https://cdn.plot.ly/plotly-2.11.1.min.js"
download.file(url, destfile = "inst/js/plotly-min.js")

url <- "https://unpkg.com/react-plotly.js@2.5.1/dist/create-plotly-component.js"
download.file(url, destfile = "inst/js/create-plotly-component.js")
