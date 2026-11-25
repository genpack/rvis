# Time series: line, area, bar and calendar charts over time

Time series charts put a date (`Date`) or date-time (`POSIXct`) column
on the horizontal axis. Examples are evaluated when the plotting package
of the wrapper is installed; otherwise only their code is shown.

## Data: New York air quality in 1973 (`airquality`)

`airquality` has daily measurements in New York from May to September
1973.

``` r

air <- airquality
air$date <- as.Date(sprintf("1973-%02d-%02d", air$Month, air$Day))
air$time <- as.POSIXct(air$date, tz = "UTC")           # date-time version, for plotters that need it
air <- air[, c("date", "time", "Temp", "Wind", "Ozone", "Solar.R")]
head(air)
#>         date       time Temp Wind Ozone Solar.R
#> 1 1973-05-01 1973-05-01   67  7.4    41     190
#> 2 1973-05-02 1973-05-02   72  8.0    36     118
#> 3 1973-05-03 1973-05-03   74 12.6    12     149
#> 4 1973-05-04 1973-05-04   62 11.5    18     313
#> 5 1973-05-05 1973-05-05   56 14.3    NA      NA
#> 6 1973-05-06 1973-05-06   66 14.9    28      NA

# Long (molten) table: one row per day and measure
air_long <- data.frame(
  date    = rep(air$date, times = 2),
  measure = factor(rep(c("Temp", "Wind"), each = nrow(air))),
  value   = c(air$Temp, air$Wind))
head(air_long)
#>         date measure value
#> 1 1973-05-01    Temp    67
#> 2 1973-05-02    Temp    72
#> 3 1973-05-03    Temp    74
#> 4 1973-05-04    Temp    62
#> 5 1973-05-05    Temp    56
#> 6 1973-05-06    Temp    66

# Weekly means, for bar charts
air$week <- as.Date(cut(air$date, "week"))
weekly <- aggregate(cbind(Temp, Wind) ~ week, data = air, FUN = mean)
names(weekly)[1] <- "date"
head(weekly)
#>         date     Temp     Wind
#> 1 1973-04-30 66.16667 11.45000
#> 2 1973-05-07 66.14286 10.98571
#> 3 1973-05-14 63.28571 12.45714
#> 4 1973-05-21 60.85714 12.50000
#> 5 1973-05-28 74.57143 10.62857
#> 6 1973-06-04 84.85714 10.57143
```

## Time series line charts

### amCharts

`amCharts` time series need a date-time (`POSIXct`) column. `low` and
`high` add a range around a series.

``` r

rvisPlot(type = "tsline", plotter = "amCharts", obj = air, x = "time", y = list("Temp", "Wind"),
         config = list(title = "Temperature and wind"))
```

### dygraphs

``` r

rvisPlot(type = "tsline", plotter = "dygraphs", obj = air, x = "date", y = "Temp", y2 = "Wind",
         config = list(title = "Temperature (left) and wind (right)"))
```

### plotly

``` r

rvisPlot(type = "tsline", plotter = "plotly", obj = air, x = "date", y = "Temp")
```

### billboarder

``` r

rvisPlot(type = "tsline", plotter = "billboarder", obj = air, x = "date", y = list("Temp", "Wind"))
```

### c3

``` r

rvisPlot(type = "tsline", plotter = "c3", obj = air, x = "date", y = list("Temp", "Wind"))
```

### d3plus

``` r

rvisPlot(type = "tsline", plotter = "d3plus", obj = air, x = "date", y = list("Temp", "Wind"))
```

### morrisjs

``` r

rvisPlot(type = "tsline", plotter = "morrisjs", obj = air, x = "date", y = list("Temp", "Wind"))
```

### apexcharter

``` r

rvisPlot(type = "tsline", plotter = "apexcharter", obj = air, x = "date", y = list("Temp", "Wind"),
         config = list(title = "Temperature and wind"))
```

### echarts

``` r

rvisPlot(type = "tsline", plotter = "echarts", obj = air, x = "date", y = list("Temp", "Wind"),
         config = list(zoom.enabled = TRUE))
```

## Time series line charts from a long table

### d3plus

``` r

rvisPlot(type = "tsline.molten", plotter = "d3plus", obj = air_long, x = "date", y = "value", group = "measure")
```

## Line chart with a choice of line type

`billboarder.tsLineChart()` takes the line type as `shape` (`"line"`,
`"spline"`, `"step"`, `"area"`, …) and one colour per series.

### billboarder

``` r

rvisPlot(type = "tsLineChart", plotter = "billboarder", obj = air, x = "date", y = "Temp",
         shape = "step", color = "firebrick")
```

## Stock chart

A Highcharts stock chart adds a navigator and a range selector.

### highcharter

``` r

rvisPlot(type = "tsline.stock", plotter = "highcharter", obj = air, x = "date", y = list("Temp", "Wind"))
```

## Time series area charts

### billboarder

``` r

rvisPlot(type = "tsarea", plotter = "billboarder", obj = air, x = "date", y = list("Temp", "Wind"))
```

### c3

``` r

rvisPlot(type = "tsarea", plotter = "c3", obj = air, x = "date", y = list("Temp", "Wind"))
```

### streamgraph

``` r

rvisPlot(type = "tsarea", plotter = "streamgraph", obj = air_long, x = "date", y = "value", group = "measure")
```

### apexcharter

``` r

rvisPlot(type = "tsarea", plotter = "apexcharter", obj = weekly, x = "date", y = "Temp")
```

### echarts

``` r

rvisPlot(type = "tsarea", plotter = "echarts", obj = weekly, x = "date", y = list("Temp", "Wind"))
```

## Time series area charts from a long table

### d3plus

``` r

rvisPlot(type = "tsarea.molten", plotter = "d3plus", obj = air_long, x = "date", y = "value", group = "measure")
```

## Time series bar charts

### plotly

``` r

rvisPlot(type = "tsbar", plotter = "plotly", obj = weekly, x = "date", y = "Temp")
```

### morrisjs

``` r

rvisPlot(type = "tsbar", plotter = "morrisjs", obj = weekly, x = "date", y = list("Temp", "Wind"))
```

### apexcharter

``` r

rvisPlot(type = "tsbar", plotter = "apexcharter", obj = weekly, x = "date", y = "Wind")
```

### echarts

``` r

rvisPlot(type = "tsbar", plotter = "echarts", obj = weekly, x = "date", y = "Wind")
```

## Time series bar charts from a long table

### d3plus

``` r

rvisPlot(type = "tsbar.molten", plotter = "d3plus", obj = air_long[air_long$date >= as.Date("1973-09-01"), ],
         x = "date", y = "value", group = "measure")
```

## Calendar heat maps

A calendar heat map colours each day by a numeric column.

### calheatmap

``` r

rvisPlot(type = "calendar", plotter = "calheatmap", obj = air, t = "date", color = "Temp")
```

### echarts

``` r

# `type` and `plotter` are named here: an argument `t` given after them by position would be taken as `type`
rvisPlot(type = "calendar", plotter = "echarts", obj = air, t = "date", color = "Temp")
```
