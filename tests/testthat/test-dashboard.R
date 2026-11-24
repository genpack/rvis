# Tests of the DASHBOARD class (dashboard.R) and of dashboards built from configurations (dashconfig.R)

skip_if_not_installed("shiny")
skip_if_not_installed("shinydashboard")
skip_if_not_installed("yaml")

html <- function(ui) as.character(ui)

test_that("a dashboard is built from an R list", {
  dash <- rvisDashboard(list(
    layout = "page",
    items = list(
      page = list(type = "fluidPage", title = "It's a test", layout = list(c("n", "txt"))),
      n    = list(type = "sliderInput", title = "N", min = 1, max = 10, value = 5),
      txt  = list(type = "textOutput", service = "paste('n =', input$n)"))))
  expect_s4_class(dash, "DASHBOARD")
  ui <- dash$dashboard.ui()
  expect_match(html(ui), "It's a test", fixed = TRUE)
  expect_match(html(ui), 'id="txt"', fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    session$setInputs(n = 7)
    expect_equal(output$txt, "n = 7")
  })
})

test_that("the original R interface works", {
  items <- list(page = list(type = "sidebarLayout", layout.side = "k", layout.main = "out"),
                k    = list(type = "numericInput", title = "k", value = 2),
                out  = list(type = "verbatimTextOutput", service = "input$k * 2"))
  dash <- new("DASHBOARD", items = items, king.layout = list("page"))
  expect_match(html(dash$dashboard.ui()), "shiny-text-output", fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    session$setInputs(k = 3)
    expect_match(output$out, "6")
  })
})

test_that("reactive values, prescript, observers and input services work", {
  dash <- rvisDashboard(list(
    layout = "page", values = list(count = 0, seen = ""),
    prescript = "doubled <- reactive(sync$count * 2)",
    observers = list("sync$seen <- paste('count', sync$count)"),
    items = list(page = list(type = "fluidPage", layout = list(c("go", "res", "obs"))),
                 go   = list(type = "actionButton", title = "Go", service = "sync$count <- sync$count + 1"),
                 res  = list(type = "textOutput", service = "doubled()"),
                 obs  = list(type = "textOutput", service = "sync$seen"))))
  dash$dashboard.ui()
  shiny::testServer(dash$dashboard.server(), {
    session$setInputs(go = 1)
    expect_equal(output$res, "2")
    expect_equal(output$obs, "count 1")
  })
})

test_that("YAML gotchas are handled", {
  f <- tempfile(fileext = ".yml")
  writeLines(c("layout: page", "items:",
               "  page: {type: fluidPage, layout: [[n, y]]}",
               "  n: {type: sliderInput, title: N, min: 4, max: 6.4, value: [4, 6.4]}",
               "  y: {type: radioButtons, title: Y, choices: {Normal: norm, Uniform: unif}}"), f)
  dash <- rvisDashboard(f)
  expect_true(all(c("n", "y") %in% names(dash$items)))
  expect_equal(dash$items$n$value, c(4, 6.4))
  ui <- html(dash$dashboard.ui())
  expect_match(ui, 'value="unif"', fixed = TRUE)
})

test_that("a dashboard is built from a JSON file", {
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".json")
  writeLines('{"layout": "page", "items": {
    "page": {"type": "fluidPage", "layout": [[{"items": "a"}, {"items": "b"}]]},
    "a": {"type": "selectInput", "title": "A", "choices": ["x", "y"]},
    "b": {"type": "textOutput", "service": "input$a"}}}', f)
  dash <- rvisDashboard(f)
  ui <- html(dash$dashboard.ui())
  expect_match(ui, 'class="col-sm-6"', fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    session$setInputs(a = "y")
    expect_equal(output$b, "y")
  })
})

test_that("YAML one-item lists make columns", {
  f <- tempfile(fileext = ".yml")
  writeLines(c("layout: page", "items:",
               "  page: {type: fluidPage, layout: [[[a], [b], [c]]]}",
               "  a: {type: static, html: A}", "  b: {type: static, html: B}", "  c: {type: static, html: C}"), f)
  ui <- html(rvisDashboard(f)$dashboard.ui())
  expect_equal(lengths(regmatches(ui, gregexpr('class="col-sm-4"', ui))), 3)
})

test_that("dashboard methods can be called in services", {
  dash <- rvisDashboard(list(layout = "page", items = list(
    page = list(type = "fluidPage", layout = list(c("x", "out"))),
    x    = list(type = "selectInput", title = "X", choices = c("a", "b")),
    out  = list(type = "textOutput", service = "paste(is.function(hideItems), is.function(disableItems), is.function(updateItem4Session))"))))
  dash$dashboard.ui()
  shiny::testServer(dash$dashboard.server(), {
    expect_equal(output$out, "TRUE TRUE TRUE")
  })
})

test_that("fixed bugs stay fixed", {
  # tabPanel cloth and several king items
  dash <- rvisDashboard(list(layout = list("head", "page"), items = list(
    head = list(type = "static", html = "<h1>Head</h1>"),
    page = list(type = "fluidPage", layout = list("tabs")),
    tabs = list(type = "tabsetPanel", layout = c("a")),
    a    = list(type = "textOutput", service = "'a'", tooltip = "tip", cloth = list(type = "tabPanel", title = "A")))))
  ui <- html(dash$dashboard.ui())
  expect_match(ui, "<h1>Head</h1>", fixed = TRUE)
  expect_match(ui, 'data-value="A"', fixed = TRUE)
  expect_match(ui, "shinyBS.addTooltip('a'", fixed = TRUE)
})

test_that("namespaced types call the package function directly", {
  dash <- rvisDashboard(list(layout = "page", items = list(
    page  = list(type = "shiny::fluidPage", title = "Generic", layout = list("head", "row")),
    head  = list(type = "shiny::h3", args = list("Generic heading")),
    row   = list(type = "shiny::fluidRow", layout = c("k", "go", "out", "tabs")),
    k     = list(type = "shiny::numericInput", label = "K", value = 2, min = 0),
    go    = list(type = "shiny::actionButton", label = "Go", service = "sync$clicked <- TRUE"),
    out   = list(type = "shiny::textOutput", service = "input$k * 3",
                 cloth = list(type = "shiny::column", width = 6)),
    tabs  = list(type = "shiny::tabsetPanel", layout = c("t1", "t2")),
    t1    = list(type = "shiny::tabPanel", title = "One", value = "one", layout = "p1"),
    t2    = list(type = "shiny::tabPanel", title = "Two", value = "two"),
    p1    = list(type = "shiny::p", args = "first tab"))))
  expect_equal(dash$itemRole("k"), "input")
  expect_equal(dash$itemRole("out"), "output")
  expect_equal(dash$itemRole("row"), "container")
  ui <- html(dash$dashboard.ui())
  expect_match(ui, "<h3>Generic heading</h3>", fixed = TRUE)
  expect_match(ui, 'id="k"', fixed = TRUE)
  expect_match(ui, 'min="0"', fixed = TRUE)
  expect_match(ui, 'class="col-sm-6"', fixed = TRUE)
  expect_match(ui, 'id="tabs"', fixed = TRUE)     # the item name is passed as id
  expect_match(ui, "first tab", fixed = TRUE)
  shiny::testServer(dash$dashboard.server(), {
    session$setInputs(k = 5)
    expect_equal(output$out, "15")
    session$setInputs(go = 1)
    expect_true(sync$clicked)
  })
})

test_that("generic render, update and role overrides work", {
  item <- list(type = "shiny::uiOutput")
  expect_equal(genericRenderFunction(item, "x"), "shiny::renderUI")
  expect_equal(genericRenderFunction(list(type = "shiny::plotOutput"), "x"), "shiny::renderPlot")
  expect_equal(genericRenderFunction(list(type = "shiny::plotOutput", render = "my::fun"), "x"), "my::fun")
  expect_identical(genericUpdateFunction(list(type = "shiny::selectInput"), "x"), shiny::updateSelectInput)
  expect_error(genericRenderFunction(list(type = "shiny::fluidPage"), "x"), "render")
  expect_error(genericFunction("shiny::notAFunction"), "not a function")
  expect_false(isGenericType("selectInput"))
  expect_true(isGenericType("shinyWidgets::pickerInput"))
  expect_equal(genericRole(list(type = "shiny::fluidRow", role = "output")), "output")

  dash <- rvisDashboard(list(layout = "page", items = list(
    page = list(type = "shiny::fluidPage", layout = "out"),
    out  = list(type = "shiny::tableOutput", service = "head(cars, 2)", render.args = list(digits = 0)))))
  dash$dashboard.ui()
  shiny::testServer(dash$dashboard.server(), {
    expect_match(output$out, "<table", fixed = TRUE)
  })
})

test_that("generic items from other packages work", {
  skip_if_not_installed("shinyWidgets")
  dash <- rvisDashboard(list(layout = "page", items = list(
    page = list(type = "shiny::fluidPage", layout = c("pk", "sw")),
    pk   = list(type = "shinyWidgets::pickerInput", label = "Pick", choices = c("a", "b"), multiple = TRUE),
    sw   = list(type = "shinyWidgets::materialSwitch", label = "Switch", value = TRUE,
                cloth = list(type = "shinydashboard::box", title = "In a box", width = 4)))))
  ui <- html(dash$dashboard.ui())
  expect_match(ui, 'id="pk"', fixed = TRUE)
  expect_match(ui, "In a box", fixed = TRUE)
  expect_equal(dash$itemRole("sw"), "input")
  expect_identical(genericUpdateFunction(dash$items$pk, "pk"), shinyWidgets::updatePickerInput)
})

test_that("all example dashboards are built", {
  ex <- rvisDashboardExample()
  expect_gt(nrow(ex), 30)
  for (k in seq_len(nrow(ex))) {
    deps <- c(shinythemes = "shinythemes", diamonds = "ggplot2", `basic-datatable` = "ggplot2", `plot-interaction` = "ggplot2")
    skip_pkg <- c(deps[ex$name[k]], if (ex$name[k] %in% c("charts", "us-states")) c("highcharter", "plotly", "leaflet", "dygraphs"))
    if (any(!vapply(stats::na.omit(skip_pkg), requireNamespace, logical(1), quietly = TRUE))) next
    dash <- rvisDashboard(ex$file[k])
    ui <- dash$dashboard.ui()
    expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")), info = ex$name[k])
    expect_type(dash$dashboard.server(), "closure")
  }
})
