# Parts of a whole: pie, funnel, pyramid, gauge, bullet and word cloud charts

These charts show how a total is shared between categories (pie, funnel,
pyramid, word cloud) or how a single value compares with a scale (gauge,
bullet). Examples are evaluated when the plotting package of the wrapper
is installed; otherwise only their code is shown.

## Data: hair and eye colour of statistics students (`HairEyeColor`)

`HairEyeColor` counts 592 students by hair colour, eye colour and sex.

``` r

hec <- as.data.frame(HairEyeColor, stringsAsFactors = FALSE)

hair <- aggregate(Freq ~ Hair, data = hec, FUN = sum)          # students per hair colour
eyes <- aggregate(Freq ~ Eye, data = hec, FUN = sum)           # students per eye colour
eyes <- eyes[order(eyes$Freq, decreasing = TRUE), ]
hair
#>    Hair Freq
#> 1 Black  108
#> 2 Blond  127
#> 3 Brown  286
#> 4   Red   71
eyes
#>     Eye Freq
#> 2 Brown  220
#> 1  Blue  215
#> 4 Hazel   93
#> 3 Green   64

# One row per student, for charts that count rows themselves
students <- hec[rep(seq_len(nrow(hec)), hec$Freq), c("Hair", "Eye", "Sex")]

# Hair and eye colour combinations, for the word cloud
combos <- aggregate(Freq ~ Hair + Eye, data = hec, FUN = sum)
combos$label <- paste(combos$Hair, "hair", combos$Eye, "eyes")

# Share of students with brown eyes, in percent, for gauges and bullets
brown <- data.frame(label = "Brown eyes", percent = round(100 * eyes$Freq[eyes$Eye == "Brown"] / sum(eyes$Freq), 1))
brown
#>        label percent
#> 1 Brown eyes    37.2
```

## Pie charts

A pie chart shows the share of each category (`label`) in the total of a
numeric column (`theta`).

### amCharts

``` r

rvisPlot(type = "pie", plotter = "amCharts", obj = hair, label = "Hair", theta = "Freq")
```

### billboarder

``` r

rvisPlot(type = "pie", plotter = "billboarder", obj = hair, label = "Hair", theta = "Freq",
         config = list(title = "Hair colour"))
```

### c3

``` r

rvisPlot(type = "pie", plotter = "c3", obj = eyes, label = "Eye", theta = "Freq",
         config = list(title = "Eye colour"))
```

### highcharter

``` r

rvisPlot(type = "pie", plotter = "highcharter", obj = hair, label = "Hair", theta = "Freq",
         config = list(title = "Hair colour"))
```

### nvd3

``` r

rvisPlot(type = "pie", plotter = "nvd3", obj = eyes, label = "Eye", theta = "Freq")
```

### plotly

``` r

rvisPlot(type = "pie", plotter = "plotly", obj = eyes, label = "Eye", theta = "Freq")
```

### morrisjs

``` r

rvisPlot(type = "pie", plotter = "morrisjs", obj = hair, label = "Hair", theta = "Freq")
```

### apexcharter

``` r

rvisPlot(type = "pie", plotter = "apexcharter", obj = hair, label = "Hair", theta = "Freq",
         config = list(pie.donut = TRUE, title = "Students by hair colour"))
```

### echarts

``` r

rvisPlot(type = "pie", plotter = "echarts", obj = eyes, label = "Eye", theta = "Freq",
         config = list(title = "Students by eye colour"))
```

## Pie charts from a long table

The chart counts the rows of each value of `group`: here one row per
student.

### nvd3

``` r

rvisPlot(type = "pie.molten", plotter = "nvd3", obj = students, group = "Eye",
         config = list(donut = TRUE))
```

## Funnel charts

A funnel chart shows categories as stages of decreasing size.

### amCharts

`amCharts` funnels use `y` for the values and `label` for the stages.

``` r

rvisPlot(type = "funnel", plotter = "amCharts", obj = eyes, y = "Freq", label = "Eye")
```

### echarts

``` r

rvisPlot(type = "funnel", plotter = "echarts", obj = eyes, label = "Eye", size = "Freq")
```

### highcharter

``` r

rvisPlot(type = "funnel", plotter = "highcharter", obj = eyes, label = "Eye", size = "Freq")
```

## Pyramid charts

### highcharter

``` r

rvisPlot(type = "pyramid", plotter = "highcharter", obj = hair, label = "Hair", size = "Freq")
```

## Gauge charts

A gauge shows one value (`theta`) on a dial. When the table has several
rows, `amCharts` and `c3` show their mean.

### amCharts

``` r

rvisPlot(type = "gauge", plotter = "amCharts", obj = brown, theta = "percent")
```

### c3

``` r

rvisPlot(type = "gauge", plotter = "c3", obj = brown, theta = "percent")
```

### googleVis

`googleVis` draws one gauge per row, labelled by `label`.

``` r

rvisPlot(type = "gauge", plotter = "googleVis", obj = brown, label = "label", theta = "percent",
         config = list(theta.min = 0, theta.max = 100))
```

### echarts

``` r

rvisPlot(type = "gauge", plotter = "echarts", obj = brown$percent,
         config = list(title = "Brown eyes (%)"))
```

## Bullet charts

A bullet chart shows one value on a horizontal (`x`) or vertical (`y`)
scale.

### amCharts

``` r

rvisPlot(type = "bullet", plotter = "amCharts", obj = brown, x = "percent")
```

## Word clouds

A word cloud sizes each `label` by a numeric column (`size`).

### echarts

``` r

rvisPlot(type = "wordCloud", plotter = "echarts", obj = combos, label = "label", size = "Freq",
         config = list(size.min = 10, size.max = 40))
```
