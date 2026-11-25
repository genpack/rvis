# Comparing categories: bar, line, area and combo charts

These charts compare values across categories: one bar, point or area
per category, and optionally several series side by side. Every example
calls `rvisPlot(type, plotter, ...)` with the same data and dimension
names, so switching plotter is mostly a matter of changing the `plotter`
argument. Examples are evaluated when the plotting package of the
wrapper is installed; otherwise only their code is shown.

## Data: death rates in Virginia (`VADeaths`)

`VADeaths` gives death rates per 1000 people in Virginia (1940) for five
age groups, in four population groups. The *wide* table has one column
per population group; the *long* (molten) table has one row per age
group and population group, which is the input of the `*.molten` chart
types.

``` r

deaths <- data.frame(age = rownames(VADeaths), VADeaths, stringsAsFactors = FALSE)
names(deaths) <- c("age", "rural_male", "rural_female", "urban_male", "urban_female")
deaths$lowest  <- apply(VADeaths, 1, min)
deaths$highest <- apply(VADeaths, 1, max)
deaths
#>         age rural_male rural_female urban_male urban_female lowest highest
#> 50-54 50-54       11.7          8.7       15.4          8.4    8.4    15.4
#> 55-59 55-59       18.1         11.7       24.3         13.6   11.7    24.3
#> 60-64 60-64       26.9         20.3       37.0         19.3   19.3    37.0
#> 65-69 65-69       41.0         30.9       54.6         35.1   30.9    54.6
#> 70-74 70-74       66.0         54.3       71.1         50.0   50.0    71.1

groups <- c("rural_male", "rural_female", "urban_male", "urban_female")
deaths_long <- data.frame(
  age   = rep(deaths$age, times = length(groups)),
  group = factor(rep(groups, each = nrow(deaths))),
  rate  = unlist(deaths[, groups], use.names = FALSE),
  stringsAsFactors = FALSE)
head(deaths_long)
#>     age        group rate
#> 1 50-54   rural_male 11.7
#> 2 55-59   rural_male 18.1
#> 3 60-64   rural_male 26.9
#> 4 65-69   rural_male 41.0
#> 5 70-74   rural_male 66.0
#> 6 50-54 rural_female  8.7
```

Several series are given to a dimension as a list of column names, for
example `y = list("rural_male", "urban_male")`. Names of the list (if
any) become series labels.

## Bar charts

A bar chart compares one or more numeric columns (`y`) across the
categories of `x`. Most wrappers draw horizontal bars when `x` is
numeric and `y` is categorical.

### amCharts

``` r

rvisPlot(type = "bar", plotter = "amCharts", obj = deaths, x = "age",
         y = list("rural_male", "rural_female", "urban_male", "urban_female"),
         config = list(title = "Death rates by age group"))
```

### billboarder

``` r

rvisPlot(type = "bar", plotter = "billboarder", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"),
         config = list(title = "Male death rates", yAxis.label = "per 1000"))
```

### c3

``` r

rvisPlot(type = "bar", plotter = "c3", obj = deaths, x = "age",
         y = list("rural_female", "urban_female"))
```

### d3plus

``` r

rvisPlot(type = "bar", plotter = "d3plus", obj = deaths, x = "age",
         y = list("rural_male", "rural_female"), config = list(title = "Rural death rates"))
```

### echarts

``` r

rvisPlot(type = "bar", plotter = "echarts", obj = deaths, x = "age",
         y = list("urban_male", "urban_female"))
```

### googleVis

``` r

rvisPlot(type = "bar", plotter = "googleVis", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"))
```

### highcharter

``` r

rvisPlot(type = "bar", plotter = "highcharter", obj = deaths, x = "age",
         y = list("rural_male", "rural_female", "urban_male", "urban_female"),
         config = list(title = "Death rates by age group", yAxis.label = "per 1000"))
```

### nvd3

``` r

rvisPlot(type = "bar", plotter = "nvd3", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"))
```

### plotly

``` r

rvisPlot(type = "bar", plotter = "plotly", obj = deaths, x = "age",
         y = list("rural_male", "rural_female", "urban_male", "urban_female"),
         config = list(barMode = "group", yAxis.label = "per 1000"))
```

### candela

`candela` draws a single measure coloured by a grouping column, so it
uses the long table.

``` r

rvisPlot(type = "bar", plotter = "candela", obj = deaths_long, x = "age", y = "rate", color = "group")
```

### dc

``` r

rvisPlot(type = "bar", plotter = "dc", obj = deaths, x = "age", y = "urban_male")
```

### apexcharter

``` r

rvisPlot(type = "bar", plotter = "apexcharter", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"),
         config = list(title = "Male death rates", yAxis.label = "per 1000", stack.enabled = TRUE))
```

## Bar charts from a long table

The series come from the values of a grouping column instead of separate
columns.

### d3plus

``` r

rvisPlot(type = "bar.molten", plotter = "d3plus", obj = deaths_long,
         x = "age", y = "rate", group = "group")
```

## Line charts

### c3

``` r

rvisPlot(type = "line", plotter = "c3", obj = deaths, x = "age", y = list("rural_male", "urban_male"))
```

### googleVis

`googleVis` line charts can put series on a second axis with `y2`.

``` r

rvisPlot(type = "line", plotter = "googleVis", obj = deaths, x = "age",
         y = "rural_male", y2 = "urban_female",
         config = list(yAxis.label = "rural male", y2Axis.label = "urban female"))
```

### highcharter

``` r

rvisPlot(type = "line", plotter = "highcharter", obj = deaths, x = "age",
         y = list("rural_male", "rural_female", "urban_male", "urban_female"))
```

### apexcharter

``` r

rvisPlot(type = "line", plotter = "apexcharter", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"), config = list(line.shape = "smooth"))
```

### echarts

``` r

# series from the values of a column with `group`:
rvisPlot(type = "line", plotter = "echarts", obj = deaths_long, x = "age", y = "rate", group = "group",
         config = list(title = "Death rates", yAxis.label = "per 1000"))
```

## Line charts from a long table

### d3plus

``` r

rvisPlot(type = "line.molten", plotter = "d3plus", obj = deaths_long, x = "age", y = "rate", group = "group")
```

## Area charts

### c3

``` r

rvisPlot(type = "area", plotter = "c3", obj = deaths, x = "age", y = list("rural_male", "rural_female"))
```

### googleVis

``` r

rvisPlot(type = "area", plotter = "googleVis", obj = deaths, x = "age", y = list("urban_male", "urban_female"))
```

### highcharter

``` r

rvisPlot(type = "area", plotter = "highcharter", obj = deaths, x = "age", y = list("rural_male", "urban_male"))
```

### apexcharter

``` r

rvisPlot(type = "area", plotter = "apexcharter", obj = deaths, x = "age",
         y = list("rural_female", "urban_female"))
```

### echarts

``` r

rvisPlot(type = "area", plotter = "echarts", obj = deaths, x = "age",
         y = list("rural_female", "urban_female"), config = list(stack.enabled = TRUE))
```

## Area charts from a long table

### d3plus

``` r

rvisPlot(type = "area.molten", plotter = "d3plus", obj = deaths_long, x = "age", y = "rate", group = "group")
```

### xCharts

``` r

rvisPlot(type = "area.molten", plotter = "xCharts", obj = deaths_long, x = "age", y = "rate", group = "group")
```

## Area range charts

An area range chart shows an interval per category: here the lowest and
highest death rate of the four population groups.

### highcharter

``` r

rvisPlot(type = "area.range", plotter = "highcharter", obj = deaths, x = "age",
         low = "lowest", high = "highest", config = list(title = "Range of death rates"))
```

## Combo charts

A combo chart mixes shapes (bars, lines, points, …) in one chart;
`shape` gives one shape per series of `y`.

### c3

``` r

rvisPlot(type = "combo", plotter = "c3", obj = deaths, x = "age",
         y = list("rural_male", "urban_male", "rural_female"), shape = list("bar", "bar", "line"))
```

### highcharter

``` r

rvisPlot(type = "combo", plotter = "highcharter", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"), shape = list("column", "line"))
```

### plotly

``` r

rvisPlot(type = "combo", plotter = "plotly", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"), shape = list("bar", "line.point"))
```

### dimple

``` r

rvisPlot(type = "combo", plotter = "dimple", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"), shape = list("bar", "line"))
```

### dygraphs

`dygraphs` puts series on the left (`y`) or right (`y2`) axis.

``` r

rvisPlot(type = "combo", plotter = "dygraphs", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"), y2 = "urban_female")
```

## Radar charts

A radar chart shows one or more numeric columns (`y`) on axes arranged
in a circle, one axis per category of `x`.

### apexcharter

``` r

rvisPlot(type = "radar", plotter = "apexcharter", obj = deaths, x = "age",
         y = list("rural_male", "urban_male"))
```
