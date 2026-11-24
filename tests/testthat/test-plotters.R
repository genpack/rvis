# Tests of the apexcharter and echarts4r wrappers (apexcharter.R, echarts.R)

deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE, stringsAsFactors = FALSE)
long   <- data.frame(age = rep(deaths$age, 2), rate = c(deaths$`Rural Male`, deaths$`Urban Male`),
                     area = rep(c("Rural", "Urban"), each = 5), stringsAsFactors = FALSE)
ts     <- data.frame(date = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
                     sales = c(5, 7, 6, 9, 11, 10, 12, 14, 13, 15, 16, 18), cost = 1:12)
graph  <- list(nodes = data.frame(id = c("a", "b", "c"), weight = c(1, 2, 3), team = c("x", "x", "y")),
               links = data.frame(from = c("a", "b"), to = c("b", "c"), w = c(1, 4)))

expect_widget <- function(w, cls) {
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, cls)
  # the widget must serialise to JSON for the page
  expect_type(htmlwidgets:::toJSON(w$x), "character")
}

test_that("apexcharter wrappers return charts", {
  skip_if_not_installed("apexcharter")
  cfg <- list(title = "Death rates", xAxis.label = "Age", legend.position = "top")
  bar <- rvisPlot("bar", "apexcharter", obj = deaths, x = "age", y = list("Rural Male", "Urban Male"), config = cfg)
  expect_widget(bar, "apexcharter")
  expect_equal(length(bar$x$ax_opts$series), 2)
  expect_equal(bar$x$ax_opts$title$text, "Death rates")
  expect_equal(bar$x$ax_opts$legend$position, "top")

  hbar <- rvisPlot("bar", "apexcharter", obj = deaths, x = "Rural Male", y = "age")
  expect_true(hbar$x$ax_opts$plotOptions$bar$horizontal)

  grouped <- rvisPlot("line", "apexcharter", obj = long, x = "age", y = "rate", group = "area", config = list(stack.enabled = TRUE))
  expect_equal(vapply(grouped$x$ax_opts$series, `[[`, "", "name"), c("Rural", "Urban"))

  expect_widget(rvisPlot("area", "apexcharter", obj = deaths, x = "age", y = "Rural Female"), "apexcharter")
  expect_widget(rvisPlot("tsline", "apexcharter", obj = ts, x = "date", y = list("sales", "cost")), "apexcharter")
  expect_widget(rvisPlot("tsarea", "apexcharter", obj = ts, x = "date", y = "sales"), "apexcharter")
  expect_widget(rvisPlot("tsbar", "apexcharter", obj = ts, x = "date", y = "sales"), "apexcharter")
  expect_widget(rvisPlot("scatter", "apexcharter", obj = iris, x = "Sepal.Length", y = "Petal.Length", group = "Species"), "apexcharter")
  expect_widget(rvisPlot("bubble", "apexcharter", obj = mtcars, x = "wt", y = "mpg", size = "hp"), "apexcharter")
  pie <- rvisPlot("pie", "apexcharter", obj = deaths, label = "age", theta = "Rural Male", config = list(pie.donut = TRUE))
  expect_equal(pie$x$ax_opts$chart$type, "donut")
  expect_widget(rvisPlot("heatmap", "apexcharter", obj = long, x = "age", y = "area", z = "rate"), "apexcharter")
  expect_widget(rvisPlot("radar", "apexcharter", obj = deaths, x = "age", y = list("Rural Male", "Urban Male")), "apexcharter")
  colored <- rvisPlot("bar", "apexcharter", obj = deaths, x = "age", y = list("Rural Male", "Urban Male"), color = list("red", "blue"))
  expect_equal(unlist(colored$x$ax_opts$colors), c("red", "blue"))
  expect_null(apexcharter.bar(deaths[0, ], x = "age", y = "Rural Male"))
  expect_error(rvisPlot("bar", "apexcharter", obj = deaths, x = "age", y = "Rural Male", config = list(legend.position = "middle")))
})

test_that("echarts wrappers return charts", {
  skip_if_not_installed("echarts4r")
  cfg <- list(title = "Death rates", stack.enabled = TRUE, zoom.enabled = TRUE)
  bar <- rvisPlot("bar", "echarts", obj = deaths, x = "age", y = list("Rural Male", "Urban Male"), config = cfg)
  expect_widget(bar, "echarts4r")
  expect_equal(length(bar$x$opts$series), 2)
  expect_equal(bar$x$opts$series[[1]]$stack, "stack")
  expect_equal(bar$x$opts$title[[1]]$text, "Death rates")

  grouped <- rvisPlot("line", "echarts", obj = long, x = "age", y = "rate", group = "area")
  expect_equal(length(grouped$x$opts$series), 2)

  expect_widget(rvisPlot("area", "echarts", obj = deaths, x = "age", y = "Rural Female"), "echarts4r")
  expect_widget(rvisPlot("tsline", "echarts", obj = ts, x = "date", y = list("sales", "cost")), "echarts4r")
  expect_widget(rvisPlot("tsarea", "echarts", obj = ts, x = "date", y = "sales"), "echarts4r")
  expect_widget(rvisPlot("tsbar", "echarts", obj = ts, x = "date", y = "sales"), "echarts4r")
  expect_widget(rvisPlot("scatter", "echarts", obj = iris, x = "Sepal.Length", y = "Petal.Length", group = "Species"), "echarts4r")
  expect_widget(rvisPlot("bubble", "echarts", obj = mtcars, x = "wt", y = "mpg", size = "hp"), "echarts4r")
  expect_widget(rvisPlot("pie", "echarts", obj = deaths, label = "age", theta = "Rural Male", config = list(pie.donut = TRUE)), "echarts4r")
  expect_widget(rvisPlot("box", "echarts", obj = iris, y = list("Sepal.Length", "Petal.Length")), "echarts4r")
  expect_widget(rvisPlot("heatmap", "echarts", obj = long, x = "age", y = "area", z = "rate"), "echarts4r")
  expect_widget(rvisPlot(type = "calendar", plotter = "echarts", obj = data.frame(day = seq(as.Date("2024-01-01"), by = "day", length.out = 60), n = 1:60),
                         t = "day", color = "n"), "echarts4r")
  gauge <- rvisPlot("gauge", "echarts", obj = mtcars, theta = "mpg")
  expect_equal(gauge$x$opts$series[[1]]$data[[1]]$value, mean(mtcars$mpg))
  expect_widget(rvisPlot("sankey", "echarts", obj = graph$links, source = "from", target = "to", linkWidth = "w"), "echarts4r")
  tm <- rvisPlot("treemap", "echarts", obj = long, label = "age", size = "rate", group = "area")
  expect_equal(length(tm$x$opts$series[[1]]$data), 2)
  expect_widget(rvisPlot("sunburst", "echarts", obj = long, label = "age", size = "rate", group = "area"), "echarts4r")
  g <- rvisPlot("graph", "echarts", obj = graph, key = "id", size = "weight", group = "team", source = "from", target = "to")
  expect_equal(length(g$x$opts$series[[1]]$data), 3)
  expect_equal(length(g$x$opts$series[[1]]$links), 2)
  # the older echarts types still work
  expect_widget(rvisPlot("funnel", "echarts", obj = deaths, label = "age", size = "Rural Male"), "echarts4r")
})

test_that("new plotters work as generic dashboard outputs", {
  skip_if_not_installed("apexcharter")
  skip_if_not_installed("echarts4r")
  skip_if_not_installed("shiny")
  dash <- rvisDashboard(list(layout = "page", items = list(
    page = list(type = "shiny::fluidPage", layout = c("ax", "ec")),
    ax   = list(type = "apexcharter::apexchartOutput",
                service = "rvisPlot('bar', 'apexcharter', obj = data.frame(a = c('x', 'y'), b = 1:2), x = 'a', y = 'b')"),
    ec   = list(type = "echarts4r::echarts4rOutput",
                service = "rvisPlot('pie', 'echarts', obj = data.frame(a = c('x', 'y'), b = 1:2), label = 'a', theta = 'b')"))))
  ui <- as.character(dash$dashboard.ui())
  expect_match(ui, 'id="ax"', fixed = TRUE)
  expect_match(ui, 'id="ec"', fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    expect_match(output$ax, "\"series\"", fixed = TRUE)
    expect_match(output$ec, "\"type\":\"pie\"", fixed = TRUE)
  })
})
