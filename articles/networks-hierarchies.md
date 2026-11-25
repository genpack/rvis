# Networks, flows and hierarchies: graph, sankey, sunburst, tree and treemap charts

Graphs and sankey diagrams show links between nodes; sunbursts, trees
and treemaps show how a total splits along nested categories. Examples
are evaluated when the plotting package of the wrapper is installed;
otherwise only their code is shown.

## Data: passengers of the Titanic (`Titanic`)

`Titanic` counts the 2201 people on board by class, sex, age and
survival.

``` r

titanic <- as.data.frame(Titanic, stringsAsFactors = FALSE)
head(titanic)
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0

# People per class, for single level charts
classes <- aggregate(Freq ~ Class, data = titanic, FUN = sum)
classes$survived <- aggregate(Freq ~ Class, data = titanic[titanic$Survived == "Yes", ], FUN = sum)$Freq
classes$survival_rate <- round(classes$survived / classes$Freq, 2)
classes
#>   Class Freq survived survival_rate
#> 1   1st  325      203          0.62
#> 2   2nd  285      118          0.41
#> 3   3rd  706      178          0.25
#> 4  Crew  885      212          0.24
```

Graph and sankey wrappers take a list with two tables: `nodes` (one row
per node) and `links` (one row per link between two nodes). rvis
function
[`df2Network()`](https://genpack.github.io/rvis/reference/df2Network.md)
builds them from the consecutive levels of a table: here class, then
sex, then survival.

``` r

flows <- df2Network(titanic, id_cols = c("Class", "Sex", "Survived"), value_col = "Freq")
flows$nodes$label <- sub(".*=", "", flows$nodes$label)    # "Class=1st" is shown as "1st"
flows$links <- as.data.frame(flows$links)
head(flows$nodes)
#>           id  label
#> 1 Sex=Female Female
#> 2   Sex=Male   Male
#> 3  Class=1st    1st
#> 4  Class=2nd    2nd
#> 5  Class=3rd    3rd
#> 6 Class=Crew   Crew
head(flows$links[, c("source", "target", "value", "ratio")])
#>       source       target value ratio
#> 1 Sex=Female  Survived=No   126 26.81
#> 2 Sex=Female Survived=Yes   344 73.19
#> 3   Sex=Male  Survived=No  1364 78.80
#> 4   Sex=Male Survived=Yes   367 21.20
#> 5  Class=1st   Sex=Female   145 44.62
#> 6  Class=1st     Sex=Male   180 55.38
```

## Graphs

A graph draws nodes (`key`, `label`, …) connected by links (`source`,
`target`, `linkWidth`, …).

### visNetwork

``` r

rvisPlot(type = "graph", plotter = "visNetwork", obj = flows, key = "id", label = "label",
         source = "source", target = "target", linkWidth = "value",
         config = list(layout = "hierarchical", direction = "left.right"))
```

### grviz (Graphviz)

`grviz` draws the graph with Graphviz (the `grViz` widget of package
DiagrammeR). In shiny, clicks on its nodes and links can be sent to an
input with
[`rvisClickEvents()`](https://genpack.github.io/rvis/reference/rvisClickEvents.md)
(nodes are reported by their key), see [clicks on chart
items](https://genpack.github.io/rvis/articles/dashboard-reference.html#clicks).

``` r

rvisPlot(type = "graph", plotter = "grviz", obj = flows, key = "id", label = "label",
         source = "source", target = "target", linkWidth = "value",
         config = list(direction = "left.right", link.width.min = 1, link.width.max = 8,
                       title = "Titanic: class, sex and survival"))
```

### echarts

``` r

# oval nodes sized by their number of passengers (the larger of the flows into and out of the node)
net <- flows
flow_in  <- tapply(net$links$value, net$links$target, sum)[net$nodes$id]
flow_out <- tapply(net$links$value, net$links$source, sum)[net$nodes$id]
net$nodes$passengers <- pmax(flow_in, flow_out, na.rm = TRUE)
rvisPlot(type = "graph", plotter = "echarts", obj = net, key = "id", label = "label", size = "passengers", shape = "oval",
         source = "source", target = "target", linkWidth = "value", config = list(node.size.min = 30, node.size.max = 70))
```

## Sankey diagrams

A sankey diagram draws links as bands whose width is proportional to
`linkWidth`.

### networkD3

``` r

rvisPlot(type = "sankey", plotter = "networkD3", obj = flows, key = "id", label = "label",
         source = "source", target = "target", linkWidth = "value",
         config = list(node.label.size = 24))
```

### googleVis

``` r

rvisPlot(type = "sankey", plotter = "googleVis", obj = flows, key = "id",
         source = "source", target = "target", linkWidth = "value")
```

### echarts

``` r

rvisPlot(type = "sankey", plotter = "echarts", obj = flows$links,
         source = "source", target = "target", linkWidth = "value")
```

## Sunburst charts

A sunburst shows nested categories (`label`, from the inner to the outer
ring) sized by `theta`. Click a slice to zoom in.

### coffeewheel

`coffeewheel` is embedded in rvis.

``` r

rvisPlot(type = "sunburst", plotter = "coffeewheel", obj = titanic, theta = "Freq",
         label = list("Class", "Sex", "Survived"), config = list(width = 600, height = 600))
```

### echarts

``` r

rvisPlot(type = "sunburst", plotter = "echarts", obj = aggregate(Freq ~ Class + Sex, data = titanic, FUN = sum),
         label = "Sex", size = "Freq", group = "Class")
```

## Trees

### sankeytree

``` r

rvisPlot(type = "tree", plotter = "sankeytree", obj = titanic, size = "Freq",
         label = list("Class", "Sex", "Survived"))
```

### d3plus

``` r

rvisPlot(type = "tree", plotter = "d3plus", obj = classes, label = "Class", size = "Freq", color = "survival_rate")
```

## Treemaps

A treemap shows categories as rectangles with areas proportional to
`size`.

### highcharter

``` r

rvisPlot(type = "treemap", plotter = "highcharter", obj = classes, label = "Class", size = "Freq")
```

### d3plus

`d3plus` treemaps can nest several `label` levels.

``` r

rvisPlot(type = "treemap", plotter = "d3plus", obj = titanic, label = list("Class", "Survived"), size = "Freq")
```

### echarts

``` r

rvisPlot(type = "treemap", plotter = "echarts", obj = aggregate(Freq ~ Class + Age, data = titanic, FUN = sum),
         label = "Age", size = "Freq", group = "Class")
```
