# Relationships and distributions: scatter, bubble, histogram and box plots

Scatter and bubble charts show how numeric variables relate to each
other; histograms and box plots show how a variable is distributed.
Examples are evaluated when the plotting package of the wrapper is
installed; otherwise only their code is shown.

## Data: Edgar Anderson’s iris flowers (`iris`)

`iris` gives four measurements (cm) of 150 flowers of three species.

``` r

flowers <- iris
flowers$Species <- as.character(flowers$Species)
flowers$id <- paste0(substr(flowers$Species, 1, 3), seq_len(nrow(flowers)))
head(flowers)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species   id
#> 1          5.1         3.5          1.4         0.2  setosa set1
#> 2          4.9         3.0          1.4         0.2  setosa set2
#> 3          4.7         3.2          1.3         0.2  setosa set3
#> 4          4.6         3.1          1.5         0.2  setosa set4
#> 5          5.0         3.6          1.4         0.2  setosa set5
#> 6          5.4         3.9          1.7         0.4  setosa set6

# Mean measurements per species
species <- aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris, FUN = mean)
species$Species <- as.character(species$Species)
species
#>      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1     setosa        5.006       3.428        1.462       0.246
#> 2 versicolor        5.936       2.770        4.260       1.326
#> 3  virginica        6.588       2.974        5.552       2.026
```

## Scatter charts

A scatter chart places one point per row at (`x`, `y`). Depending on the
plotter, `color`, `size`, `shape` or `group` map further columns.

### plotly

``` r

rvisPlot(type = "scatter", plotter = "plotly", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         color = "Species", config = list(xAxis.label = "Sepal length", yAxis.label = "Petal length"))
```

### highcharter

``` r

rvisPlot(type = "scatter", plotter = "highcharter", obj = flowers, x = "Sepal.Length", y = "Petal.Length")
```

### billboarder

``` r

rvisPlot(type = "scatter", plotter = "billboarder", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         group = "Species")
```

### d3plus

`d3plus` needs a unique `label` per point.

``` r

rvisPlot(type = "scatter", plotter = "d3plus", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         size = "Petal.Width", label = "id")
```

### ggvis

`ggvis` maps a *factor* colour column to a colour scale (a character
column is read as colour names), so this example uses `iris`, where
`Species` is a factor.

``` r

rvisPlot(type = "scatter", plotter = "ggvis", obj = iris, x = "Sepal.Length", y = "Petal.Length",
         color = "Species")
```

### nvd3

``` r

rvisPlot(type = "scatter", plotter = "nvd3", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         group = "Species")
```

### dimple

``` r

rvisPlot(type = "scatter", plotter = "dimple", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         color = "Species")
```

### rCharts (Highcharts)

``` r

rvisPlot(type = "scatter", plotter = "rCharts", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         color = "Species", shape = "point")
```

### candela

``` r

rvisPlot(type = "scatter", plotter = "candela", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         color = "Species", size = "Petal.Width")
```

### dc

``` r

rvisPlot(type = "scatter", plotter = "dc", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         group = "Species")
```

### rbokeh

``` r

rvisPlot(type = "scatter", plotter = "rbokeh", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         color = "Species")
```

### apexcharter

``` r

rvisPlot(type = "scatter", plotter = "apexcharter", obj = flowers,
         x = "Petal.Length", y = "Petal.Width", group = "Species")
```

### echarts

``` r

rvisPlot(type = "scatter", plotter = "echarts", obj = flowers,
         x = "Sepal.Length", y = "Sepal.Width", group = "Species", config = list(zoom.enabled = TRUE))
```

## Scatter charts from a long table

The series come from the values of `group` (here the species).

### c3

``` r

rvisPlot(type = "scatter.molten", plotter = "c3", obj = flowers, x = "Sepal.Length", y = "Petal.Length",
         group = "Species")
```

### highcharter

``` r

rvisPlot(type = "scatter.molten", plotter = "highcharter", obj = flowers, x = "Sepal.Length",
         y = "Petal.Length", group = "Species")
```

### highcharts (rCharts)

``` r

rvisPlot(type = "scatter.molten", plotter = "highcharts", obj = flowers, x = "Sepal.Length",
         y = "Petal.Length", size = "Petal.Width", group = "Species")
```

## Bubble charts

A bubble chart sizes each item by `size`.

### highcharter

``` r

rvisPlot(type = "bubble", plotter = "highcharter", obj = species, x = "Sepal.Length", y = "Petal.Length",
         size = "Petal.Width", config = list(title = "Species means"))
```

### d3plus

`d3plus` bubbles are grouped by series: each `size` column with its
`label` column.

``` r

rvisPlot(type = "bubble", plotter = "d3plus", obj = species,
         size = list("Petal.Length", "Sepal.Length"), label = list("Species", "Species"))
```

### bubbles

``` r

rvisPlot(type = "bubble", plotter = "bubbles", obj = species, size = "Petal.Length", label = "Species")
```

### apexcharter

``` r

rvisPlot(type = "bubble", plotter = "apexcharter", obj = species,
         x = "Sepal.Length", y = "Petal.Length", size = "Petal.Width", group = "Species")
```

### echarts

``` r

rvisPlot(type = "bubble", plotter = "echarts", obj = flowers,
         x = "Sepal.Length", y = "Petal.Length", size = "Petal.Width", group = "Species")
```

## Bubble charts from a long table

### d3plus

``` r

rvisPlot(type = "bubble.molten", plotter = "d3plus", obj = flowers[seq(1, 150, by = 5), ],
         size = "Petal.Length", label = "id", group = "Species")
```

### bubbleCloud

``` r

rvisPlot(type = "bubble.molten", plotter = "bubbleCloud", obj = flowers, size = "Petal.Length", group = "Species")
```

## Histograms

### ggvis

``` r

rvisPlot(type = "histogram", plotter = "ggvis", obj = flowers, x = "Petal.Length",
         config = list(hist.bar.width = 0.25))
```

## Box plots

A box plot summarises the distribution of a numeric column for each
category.

### amCharts

``` r

rvisPlot(type = "box", plotter = "amCharts", obj = flowers, x = "Species", y = "Petal.Length")
```

### plotly

``` r

rvisPlot(type = "box", plotter = "plotly", obj = flowers, x = "Species", y = "Sepal.Width")
```

### bpexploder

``` r

rvisPlot(type = "box", plotter = "bpexploder", obj = flowers, x = "Species", y = "Petal.Width")
```

### echarts

``` r

rvisPlot(type = "box", plotter = "echarts", obj = flowers,
         y = list("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
```

## Heat maps

A heat map colours the cells of a grid of two categorical columns (`x`
and `y`) by a numeric column (`z`).

``` r

# mean measurements per species in a long table
species_long <- data.frame(Species = rep(species$Species, 4),
                           measure = rep(names(species)[-1], each = nrow(species)),
                           value   = unlist(species[, -1], use.names = FALSE))
```

### apexcharter

``` r

rvisPlot(type = "heatmap", plotter = "apexcharter", obj = species_long,
         x = "measure", y = "Species", z = "value")
```

### echarts

``` r

rvisPlot(type = "heatmap", plotter = "echarts", obj = species_long,
         x = "measure", y = "Species", z = "value")
```
