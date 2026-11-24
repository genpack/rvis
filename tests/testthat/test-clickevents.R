# Tests of rvisClickEvents() (clickevents.R), the grviz widget and dashboard outputs with click.events

click_hook <- function(w) {
  hooks <- w$jsHooks$render
  expect_length(hooks, 1)
  hooks[[1]]
}

test_that("click javascript is added to supported widgets", {
  skip_if_not_installed("DiagrammeR")
  w <- rvisClickEvents(DiagrammeR::grViz("digraph {a -> b}"))
  hook <- click_hook(w)
  expect_equal(hook$data$handler, "graphviz")
  expect_null(hook$data$input)
  expect_match(as.character(hook$code), "_item_click", fixed = TRUE)
  expect_match(as.character(hook$code), "Shiny.setInputValue", fixed = TRUE)

  w <- rvisClickEvents(DiagrammeR::grViz("digraph {a -> b}"), inputId = "picked", ids = c(a = "A"))
  expect_equal(click_hook(w)$data$input, "picked")
  expect_equal(click_hook(w)$data$ids, list(a = "A"))
})

test_that("each widget class gets its handler", {
  skip_if_not_installed("echarts4r")
  skip_if_not_installed("apexcharter")
  deaths <- data.frame(age = rownames(VADeaths), VADeaths, check.names = FALSE)
  expect_equal(click_hook(rvisClickEvents(rvisPlot("bar", "echarts", obj = deaths, x = "age", y = "Rural Male")))$data$handler, "echarts")
  expect_equal(click_hook(rvisClickEvents(rvisPlot("bar", "apexcharter", obj = deaths, x = "age", y = "Rural Male")))$data$handler, "apex")
  skip_if_not_installed("networkD3")
  sk <- networkD3::sankeyNetwork(Links = data.frame(source = 0, target = 1, value = 1), Nodes = data.frame(name = c("A", "B")),
                                 Source = "source", Target = "target", Value = "value", NodeID = "name")
  expect_equal(click_hook(rvisClickEvents(sk))$data$handler, "d3data")
})

test_that("other widgets need a selector", {
  skip_if_not_installed("sunburstR")
  sb <- sunburstR::sunburst(data.frame(seq = c("a-b", "c"), n = 1:2))
  expect_error(rvisClickEvents(sb), "selector")
  hook <- click_hook(rvisClickEvents(sb, selector = "path"))
  expect_equal(hook$data$handler, "selector")
  expect_equal(hook$data$selector, "path")
  expect_error(rvisClickEvents(list(a = 1)))
})

test_that("grviz graphs use DiagrammeR and report node keys", {
  skip_if_not_installed("DiagrammeR")
  graph <- list(nodes = data.frame(id = c("a", "b", "c"), label = c("Alpha", "Beta", "Gamma")),
                links = data.frame(source = c("a", "b"), target = c("b", "c")))
  w <- rvisPlot("graph", "grviz", obj = graph, key = "id", label = "label", source = "source", target = "target")
  expect_s3_class(w, "grViz")
  expect_length(w$jsHooks$render, 0)              # no click javascript unless asked for
  expect_equal(unlist(click_hook(rvisClickEvents(w))$data$ids), c(`1` = "a", `2` = "b", `3` = "c"))

  w <- rvisPlot("graph", "grviz", obj = graph, key = "id", label = "label", source = "source", target = "target",
                config = list(shinyInput.click = "node_clicked"))
  expect_equal(click_hook(w)$data$input, "node_clicked")
  expect_match(as.character(grvizOutput("g")), "grViz", fixed = TRUE)
})

test_that("dashboard outputs with click.events send clicks", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("yaml")
  skip_if_not_installed("echarts4r")
  skip_if_not_installed("DiagrammeR")
  dash <- rvisDashboard(rvisDashboardExample("click-events"))
  ui <- as.character(dash$dashboard.ui())
  expect_match(ui, 'id="graph"', fixed = TRUE)
  expect_match(ui, 'id="bars"', fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    expect_match(output$graph, "_item_click", fixed = TRUE)
    expect_match(output$graph, '"ids":{"1":"raw"', fixed = TRUE)
    expect_match(output$bars, '"handler":"echarts"', fixed = TRUE)
    session$setInputs(graph_item_click = list(type = "node", id = "model", label = "Model"))
    expect_match(output$clicked, "model", fixed = TRUE)
  })
})
