# Configuration reference

Pass configuration as a named list in `config`:

``` r

rvisPlot(
  type = "bar", plotter = "amCharts", obj = summary_data,
  x = "category", y = "value",
  config = list(
    title = "Quarterly results",
    legend.enabled = TRUE,
    legend.position = "bottom",
    barMode = "stack"
  )
)
```

Configuration is interpreted by the selected wrapper. The same key may
be unsupported or have a different default in another plotter. The table
below lists every property currently recorded in `data/properties.csv`,
including its wrapper, declared type, default, and allowed values. Use
only keys listed for the selected wrapper. The property names are
case-sensitive.

| Plotter | Key | Meaning | Type | Default | Allowed values |
|:---|:---|:---|:---|:---|:---|
| billboarder | yAxis.label.position | Controls the vertical axis label position. | character |  | top middle bottom inner-top inner-middle inner-bottom outer-top outer-middle outer-bottom inner outer |
| billboarder | xAxis.label.position | Controls the horizontal axis label position. | character |  | left right inner-left inner-right outer-left outer-right inner outer |
| billboarder | yAxis.grid.enabled | Controls the vertical axis grid enabled. | logical | F | T F |
| billboarder | xAxis.grid.enabled | Controls the horizontal axis grid enabled. | logical | F | T F |
| billboarder | yAxis.tick.label.suffix | Controls the vertical axis tick label suffix. | character |  |  |
| billboarder | xAxis.tick.label.suffix | Controls the horizontal axis tick label suffix. | character |  |  |
| billboarder | xAxis.tick.label.format | Controls the horizontal axis tick label format. | character | %Y-%m-%d |  |
| billboarder | yAxis.label | Controls the vertical axis label. | character |  |  |
| billboarder | xAxis.label | Controls the horizontal axis label. | character |  |  |
| billboarder | title | Chart title text. | character |  |  |
| billboarder | subtitle | Chart subtitle text. | character |  |  |
| billboarder | legend.enabled | Whether to display the legend. | logical | T | T F |
| billboarder | legend.position | Where to place the legend. | character |  | right bottom top-right top-left bottom-right bottom-left |
| billboarder | label.enabled | Whether to show data labels on the chart. | logical |  | T F |
| billboarder | size | Controls the size setting. | numeric integer |  | 0 Inf |
| billboarder | stack.enabled | Whether compatible series are stacked. | logical | F | T F |
| billboarder | additionalColumns | Extra data columns retained for use by the chart. | character |  |  |
| billboarder | subchart.enabled | Whether to show a secondary overview or range selector. | logical | F | T F |
| billboarder | subchart.height | Height of the secondary overview or range selector. | numeric integer | 30 | 10 100 |
| c3 | additionalColumns | Extra data columns retained for use by the chart. | character |  |  |
| bpexploder | size | Controls the size setting. | numeric integer | 0.75 | 0 1 |
| bubbles | label.threshold | Controls the label threshold. | numeric integer | 0 | 0 Inf |
| c3 | xAxis.label | Controls the horizontal axis label. | character |  |  |
| c3 | yAxis.label | Controls the vertical axis label. | character |  |  |
| c3 | stack | Whether compatible series are stacked. | logical | F | T F |
| candela | aggregator.function.string | Controls the aggregator function string. | character | value | value mean sum |
| coffeewheel | title | Chart title text. | character |  |  |
| coffeewheel | aggregator.function.string | Controls the aggregator function string. | character | sum | mean sum |
| coffeewheel | tree.function | Controls the tree function. | function |  |  |
| coffeewheel | width | Controls the width setting. | numeric integer | 1200 | 0 Inf |
| coffeewheel | height | Controls the height setting. | numeric integer | 800 | 0 Inf |
| visNetwork | node.border.color | Controls the nodes border color. | character |  |  |
| diagramer | node.border.color | Controls the nodes border color. | character |  |  |
| diagramer | node.border.width | Controls the nodes border width. | numeric integer | 2 | 0 10 |
| echarts | cloud.shape | Controls the cloud shape. | character | circle | circle |
| echarts | size.min | Controls the size min. | numeric integer | 12 | 0 100 |
| echarts | size.max | Controls the size max. | numeric integer | 60 | 0 200 |
| echarts | colorize | Whether the wrapper assigns colors automatically. | logical | T | T F |
| echarts | additionalColumns | Extra data columns retained for use by the chart. | character |  |  |
| plotly | xAxis.enabled | Controls the horizontal axis enabled. | logical | T | T F |
| plotly | yAxis.enabled | Controls the vertical axis enabled. | logical | T | T F |
| plotly | xAxis.label | Controls the horizontal axis label. | character |  |  |
| plotly | yAxis.label | Controls the vertical axis label. | character |  |  |
| plotly | barMode | Whether bars are grouped, stacked, or shown as proportions, as supported by the wrapper. | character | group | group stack relative |
| plotly | xAxis.grid.enabled | Controls the horizontal axis grid enabled. | logical | T | T F |
| plotly | yAxis.grid.enabled | Controls the vertical axis grid enabled. | logical | T | T F |
| plotly | xAxis.tick.label.enabled | Controls the horizontal axis tick label enabled. | logical | T | T F |
| plotly | yAxis.tick.label.enabled | Controls the vertical axis tick label enabled. | logical | T | T F |
| plotly | legend.border.color | Controls the legend border color. | character |  |  |
| plotly | legend.border.width | Controls the legend border width. | numeric integer |  | 0 Inf |
| plotly | legend.orientation | Controls the legend orientation. | character |  |  |
| plotly | legend.size | Controls the legend size. | numeric integer |  | 0 Inf |
| plotly | legend.color | Controls the legend color. | character |  |  |
| plotly | legend.font | Controls the legend font. | character |  |  |
| plotly | xAxis.margin.bottom | Controls the horizontal axis margin bottom. | numeric integer |  | 0 Inf |
| plotly | xAxis.margin.top | Controls the horizontal axis margin top. | numeric integer |  | 0 Inf |
| plotly | yAxis.margin.right | Controls the vertical axis margin right. | numeric integer |  | 0 Inf |
| plotly | yAxis.margin.left | Controls the vertical axis margin left. | numeric integer |  | 0 Inf |
| plotly | yAxis.tick.angle | Controls the vertical axis tick angle. | numeric integer |  |  |
| plotly | xAxis.tick.angle | Controls the horizontal axis tick angle. | numeric integer |  |  |
| visNetwork | node.color | Controls the nodes color. | character |  |  |
| diagramer | node.color | Controls the nodes color. | character |  |  |
| diagramer | direction | Controls the direction setting. | character | up.down | up.down down.up right.left left.right |
| visNetwork | direction | Controls the direction setting. | character | up.down | up.down left.right right.left down.up |
| visNetwork | node.highlight.color | Controls the nodes highlight color. | character |  |  |
| visNetwork | node.label | Controls the nodes label. | character |  |  |
| visNetwork | node.label.font | Controls the nodes label font. | character |  |  |
| diagramer | node.label.size | Controls the nodes label size. | numeric integer | 30 |  |
| visNetwork | layout | Controls the layout setting. | character | random | random hierarchical |
| diagramer | layout | Controls the layout setting. | character | random | random hierarchical neato twopi circo |
| visNetwork | link.hidden | Controls the links hidden. | logical | F | T F |
| visNetwork | link.hover.width | Controls the links hover width. | numeric integer | 12 |  |
| visNetwork | link.label | Controls the links label. | character |  |  |
| visNetwork | link.label.color | Controls the links label color. | character |  |  |
| visNetwork | link.label.highlight.bold | Controls the links label highlight bold. | logical | T | T F |
| visNetwork | link.label.size | Controls the links label size. | numeric integer |  |  |
| visNetwork | link.length | Controls the links length. | numeric integer |  |  |
| visNetwork | link.length.max | Controls the links length max. | numeric integer | 50 |  |
| visNetwork | link.length.min | Controls the links length min. | numeric integer | 1 |  |
| visNetwork | link.physics | Controls the links physics. | logical | F | T F |
| visNetwork | link.physics.enabled | Controls the links physics enabled. | logical | F | T F |
| visNetwork | link.size | Controls the links size. | numeric integer |  |  |
| visNetwork | link.smooth.type | Controls the links smooth type. | character | curvedCCW | dynamic continuous discrete diagonalCross straightCross horizontal vertical curvedCW curvedCCW cubicBezier |
| visNetwork | link.tooltip | Controls the links tooltip. | character |  |  |
| visNetwork | link.width | Controls the links width. | numeric integer | 5 |  |
| visNetwork | link.width.max | Controls the links width max. | numeric integer | 10 |  |
| diagramer | link.width.max | Controls the links width max. | numeric integer | 12 |  |
| visNetwork | link.width.min | Controls the links width min. | numeric integer | 1 |  |
| diagramer | link.width.min | Controls the links width min. | numeric integer | 2 |  |
| visNetwork | node.physics.enabled | Controls the nodes physics enabled. | logical | F | T F |
| visNetwork | node.shadow.enabled | Controls the nodes shadow enabled. | logical | T | T F |
| visNetwork | node.shape | Controls the nodes shape. | character | rectangle | square delta curl icon triangleDown point triangle box rectangle dot bubble circle bubble star ellipse cylinder diamond rhombus image circularImage database text |
| diagramer | node.shape | Controls the nodes shape. | character | rectangle | square delta icon triangle rectangle box dot point egg oval text bubble circle star ellipse cylinder diamond rhombus |
| diagramer | shinyInput.click | Controls the shinyInput click. | character |  |  |
| diagramer | shinyInput.click | Controls the shinyInput click. | character |  |  |
| grviz | node.label.size | Controls the nodes label size. | numeric integer | 14 |  |
| visNetwork | node.size.max | Size of the largest nodes of a graph (pixels), when a size column is given. | numeric integer | 40 |  |
| diagramer | node.size.ratio | Controls the nodes size ratio. | numeric integer | 0.6 | 0 1 |
| diagramer | node.size | Size of the nodes of a graph (pixels), when no size column is given. | numeric integer | 3 |  |
| diagramer | node.size.max | Size of the largest nodes of a graph (pixels), when a size column is given. | numeric integer | 4 |  |
| visNetwork | node.size.min | Size of the smallest nodes of a graph (pixels), when a size column is given. | numeric integer | 15 |  |
| diagramer | node.size.min | Size of the smallest nodes of a graph (pixels), when a size column is given. | numeric integer | 2 |  |
| diagramer | title | Chart title text. | character |  |  |
| visNetwork | tooltip | Tooltip configuration or tooltip field, as supported by the wrapper. | character |  |  |
| diagramer | tooltip | Tooltip configuration or tooltip field, as supported by the wrapper. | character |  |  |
| networkD3 | link.tooltip.suffix | Controls the links tooltip suffix. | character |  |  |
| networkD3 | width | Controls the width setting. | numeric integer | 30 |  |
| networkD3 | label.size | Controls the label size. | numeric integer | 30 |  |
| plotly | size.min | Controls the size min. | numeric integer | 10 |  |
| plotly | size.max | Controls the size max. | numeric integer | 500 |  |
| amCharts | direction | Controls the direction setting. | character | up.down | up.down down.up |
| amCharts | thetaAxis.tick.step | Controls the thetaAxis tick step. | numeric integer | 20 | 1 Inf |
| amCharts | theta.min | Controls the theta min. | numeric integer | 0 |  |
| amCharts | theta.max | Controls the theta max. | numeric integer | 100 |  |
| amCharts | x.min | Controls the x min. | numeric integer | 0 |  |
| amCharts | x.max | Controls the x max. | numeric integer | 100 |  |
| amCharts | y.min | Controls the y min. | numeric integer | 0 |  |
| amCharts | y.max | Controls the y max. | numeric integer | 100 |  |
| amCharts | xAxis.tick.step | Controls the horizontal axis tick step. | numeric integer | 20 |  |
| amCharts | yAxis.tick.step | Controls the vertical axis tick step. | numeric integer | 20 |  |
| amCharts | xAxis.step.enabled | Controls the horizontal axis step enabled. | logical | T | T F |
| amCharts | yAxis.step.enabled | Controls the vertical axis step enabled. | logical | T | T F |
| d3plus | legend.enabled | Whether to display the legend. | logical | T | T F |
| d3plus | legend.size | Controls the legend size. | numeric integer | 30 | 1 Inf |
| d3plus | legend.tooltip.enabled | Controls the legend tooltip enabled. | logical | T | T F |
| d3plus | tooltip | Tooltip configuration or tooltip field, as supported by the wrapper. | character |  |  |
| d3plus | additionalColumns | Extra data columns retained for use by the chart. | character |  |  |
| d3plus | height | Controls the height setting. | numeric integer character |  |  |
| d3plus | width | Controls the width setting. | numeric integer character |  |  |
| d3plus | xAxis.grid.enabled | Controls the horizontal axis grid enabled. | logical | T | T F |
| d3plus | yAxis.grid.enabled | Controls the vertical axis grid enabled. | logical | T | T F |
| d3plus | xAxis.min | Controls the horizontal axis min. | numeric integer Date |  |  |
| d3plus | xAxis.max | Controls the horizontal axis max. | numeric integer Date |  |  |
| d3plus | label.format | Controls the label format. | character |  |  |
| d3plus | currency.symbol | Controls the currency symbol. | character | US\$ |  |
| d3plus | shape | Controls the shape setting. | character | line | line bar area |
| morrisjs | yAxis.tick.suffix | Controls the vertical axis tick suffix. | character |  |  |
| morrisjs | yAxis.tick.prefix | Controls the vertical axis tick prefix. | character |  |  |
| dygraphs | subchart.enabled | Whether to show a secondary overview or range selector. | logical | T | T F |
| dygraphs | subchart.height | Height of the secondary overview or range selector. | numeric | 40 |  |
| dygraphs | subchart.background | Controls the overview panel background. | character | A7B1C4 |  |
| dygraphs | subchart.color | Controls the overview panel color. | character | 808FAB |  |
| dygraphs | subchart.zoom.enabled | Controls the overview panel zoom enabled. | logical | T | T F |
| highcharter | color.min | Controls the color min. | character | white |  |
| highcharter | color.max | Colour of the largest values of a heat map. | character | blue |  |
| billboarder | title.position | Controls the title position. | character | left-top | left-top right-top |
| billboarder | title.padding.top | Controls the title padding top. | integer | 0 |  |
| billboarder | title.padding.right | Controls the title padding right. | integer | 0 |  |
| billboarder | title.padding.left | Controls the title padding left. | integer | 0 |  |
| billboarder | title.padding.bottom | Controls the title padding bottom. | integer | 20 |  |
| amCharts | xAxis.label.rotation | Controls the horizontal axis label rotation. | numeric | 0 | -90 90 |
| amCharts | barMode | Whether bars are grouped, stacked, or shown as proportions, as supported by the wrapper. | character | group | group stack |
| amCharts | legend.enabled | Whether to display the legend. | logical | T | T F |
| amCharts | theme | Name of the chart theme (for apexcharter: light or dark). | character |  | light dark patterns chalk |
| amCharts | legend.position | Where to place the legend. | character | bottom | right left |
| amCharts | title | Chart title text. | character |  |  |
| amCharts | title.color | Controls the title color. | character | black |  |
| amCharts | title.size | Controls the title size. | integer | 15 | 1 100 |
| amCharts | xAxis.scrollbar.enabled | Controls the horizontal axis scrollbar enabled. | logical | F | T F |
| amCharts | yAxis.scrollbar.enabled | Controls the vertical axis scrollbar enabled. | logical | T | T F |
| amCharts | xAxis.scrollbar.width | Controls the horizontal axis scrollbar width. | numeric | 10 | 5 100 |
| amCharts | yAxis.scrollbar.width | Controls the vertical axis scrollbar width. | numeric | 10 | 5 100 |
| dimple | legend.enabled | Whether to display the legend. | logical | T | T F |
| dc | yAxis.label | Controls the vertical axis label. | character |  |  |
| dc | xAxis.label | Controls the horizontal axis label. | character |  |  |
| dc | xAxis.min | Controls the horizontal axis min. | numeric |  |  |
| dc | xAxis.max | Controls the horizontal axis max. | numeric |  |  |
| dc | legend.enabled | Whether to display the legend. | logical | T | T F |
| dc | legend.position.x | Controls the legend position x. | numeric | 20 |  |
| dc | legend.position.y | Controls the legend position y. | numeric | 20 |  |
| dc | legend.width | Controls the legend width. | numeric | 140 |  |
| dc | legend.height | Controls the legend height. | numeric | 140 |  |
| diagramer | link.label.size | Controls the links label size. | numeric integer | 30 |  |
| diagramer | link.arrow.size | Controls the links arrow size. | numeric integer | 1.5 |  |
| echarts | title | Chart title text. | character |  |  |
| echarts | subtitle | Chart subtitle text. | character |  |  |
| echarts | legend.enabled | Whether to display the legend. | logical | T | T F |
| echarts | legend.position | Where to place the legend. | character | bottom | top bottom left right |
| echarts | tooltip.enabled | Whether to show a tooltip when hovering over the chart. | logical | T | T F |
| echarts | xAxis.label | Controls the horizontal axis label. | character |  |  |
| echarts | yAxis.label | Controls the vertical axis label. | character |  |  |
| echarts | label.enabled | Whether to show data labels on the chart. | logical | F | T F |
| echarts | stack.enabled | Whether compatible series are stacked. | logical | F | T F |
| echarts | zoom.enabled | Whether to add a zoom slider to the axes. | logical | F | T F |
| echarts | theme | Name of the chart theme (for apexcharter: light or dark). | character |  |  |
| echarts | pie.donut | Whether a pie chart is drawn as a donut (a ring). | logical | F | T F |
| echarts | graph.layout | Layout of the nodes of a graph: force-directed, circular or none (positions given). | character | force | force circular none |
| apexcharter | title | Chart title text. | character |  |  |
| apexcharter | subtitle | Chart subtitle text. | character |  |  |
| apexcharter | xAxis.label | Controls the horizontal axis label. | character |  |  |
| apexcharter | yAxis.label | Controls the vertical axis label. | character |  |  |
| apexcharter | legend.enabled | Whether to display the legend. | logical | T | T F |
| apexcharter | legend.position | Where to place the legend. | character | bottom | top right bottom left |
| apexcharter | label.enabled | Whether to show data labels on the chart. | logical |  | T F |
| apexcharter | stack.enabled | Whether compatible series are stacked. | logical | F | T F |
| apexcharter | theme | Name of the chart theme (for apexcharter: light or dark). | character |  | light dark |
| apexcharter | line.shape | Shape of the lines: smooth, straight or stepline. | character |  | smooth straight stepline |
| apexcharter | pie.donut | Whether a pie chart is drawn as a donut (a ring). | logical | F | T F |
| apexcharter | color.max | Colour of the largest values of a heat map. | character |  |  |
| apexcharter | additionalColumns | Extra data columns retained for use by the chart. | character |  |  |
| echarts | node.size | Size of the nodes of a graph (pixels), when no size column is given. | numeric integer | 15 | 0 Inf |
| echarts | node.size.min | Size of the smallest nodes of a graph (pixels), when a size column is given. | numeric integer | 10 | 0 Inf |
| echarts | node.size.max | Size of the largest nodes of a graph (pixels), when a size column is given. | numeric integer | 40 | 0 Inf |

## Additional settings read by wrappers

Some wrappers read settings directly from `config` that are not present
in the validation metadata table above. This source inventory includes
literal `config$name` and `config[["name"]]` accesses. These settings
may not be type-checked centrally; consult the named wrapper
implementation for its expected values.

| Wrapper | Key | Meaning / source note |
|:---|:---|:---|
| `D3TableFilter` | `column.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.color.auto` | Source-level setting for color auto; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.filter` | Source-level setting for filter; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.filter.enabled` | Source-level setting for filter enabled; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.footer.font` | Source-level setting for footer font; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `column.type` | Source-level setting for type; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `dimclass` | Source-level setting for dimclass; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `extensions` | Source-level setting for extensions; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `height` | Source-level setting for height; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `selection.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `selection.mode` | Source-level setting for mode; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `table.style` | Source-level setting for style; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `width` | Source-level setting for width; see the wrapper listed here for its accepted values and effect. |
| `D3TableFilter` | `withRowNames` | Source-level setting for with row names; see the wrapper listed here for its accepted values and effect. |
| `DT` | `links` | Source-level setting for links; see the wrapper listed here for its accepted values and effect. |
| `DT` | `paging.length` | Source-level setting for length; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.color.auto` | Source-level setting for color auto; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.editable` | Source-level setting for editable; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.filter` | Source-level setting for filter; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.filter.enabled` | Source-level setting for filter enabled; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.footer` | Source-level setting for footer; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.footer.font` | Source-level setting for footer font; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `column.type` | Source-level setting for type; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `dimclass` | Source-level setting for dimclass; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `extensions` | Source-level setting for extensions; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `footer.font.adjust` | Source-level setting for font adjust; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `footer.font.format` | Source-level setting for font format; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `footer.font.weight` | Source-level setting for font weight; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `height` | Source-level setting for height; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `paging.length` | Source-level setting for length; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `row.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `selected` | Source-level setting for selected; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `selection.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `selection.mode` | Source-level setting for mode; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `sort` | Source-level setting for sort; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `sort_config` | Source-level setting for sort_config; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `sort_types` | Source-level setting for sort_types; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `table.style` | Source-level setting for style; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `width` | Source-level setting for width; see the wrapper listed here for its accepted values and effect. |
| `TFD3` | `withRowNames` | Source-level setting for with row names; see the wrapper listed here for its accepted values and effect. |
| `amCharts` | `aggregator` | Aggregation function applied to values. |
| `amCharts` | `colorder` | Color order used by the chart. |
| `amCharts` | `legend` | Whether and how the legend is displayed. |
| `amCharts` | `line.width` | Width of plotted lines. |
| `amCharts` | `point.size` | Size of plotted points. |
| `amCharts` | `thetaAxis.zone` | Value ranges and colors for gauge bands. |
| `amCharts` | `yAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `point.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `point.label.enabled` | Source-level setting for label enabled; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `stack.groups` | Source-level setting for groups; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `title.padding.buttom` | Source-level setting for padding buttom; see the wrapper listed here for its accepted values and effect. |
| `billboarder` | `yAxis.tick.label.format` | Source-level setting for tick label format; see the wrapper listed here for its accepted values and effect. |
| `bpexploder` | `feedAdditionalColumns` | Whether additional columns are included in the box plot data. |
| `bpexploder` | `point.size` | Size of plotted points. |
| `bpexploder` | `point.tooltip` | Tooltip field or setting for points. |
| `bubbles` | `point.label.threshold` | Source-level setting for label threshold; see the wrapper listed here for its accepted values and effect. |
| `c3` | `aggregator` | Aggregation function applied to values. |
| `c3` | `shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `c3` | `stack.enabled` | Source-level setting for enabled; see the wrapper listed here for its accepted values and effect. |
| `c3` | `stack.groups` | Source-level setting for groups; see the wrapper listed here for its accepted values and effect. |
| `c3` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `calheatmap` | `colorize` | Source-level setting for colorize; see the wrapper listed here for its accepted values and effect. |
| `d3plus` | `currency` | Source-level setting for currency; see the wrapper listed here for its accepted values and effect. |
| `d3plus` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `legend.height` | Source-level setting for height; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `legend.horizontalAlign` | Source-level setting for horizontal align; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `legend.position.x` | Source-level setting for position x; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `legend.position.y` | Source-level setting for position y; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `legend.width` | Source-level setting for width; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `maxSizeOverride` | Source-level setting for max size override; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `xAxis.tick.label.format` | Source-level setting for tick label format; see the wrapper listed here for its accepted values and effect. |
| `dimple` | `yAxis.tick.label.format` | Source-level setting for tick label format; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `RangeSelector` | Range selector settings for the time axis. |
| `dygraphs` | `Roller` | Smoothing roller settings for time-series data. |
| `dygraphs` | `color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `colors` | Palette used for series. |
| `dygraphs` | `fillGraph` | Whether to fill the area below a line. |
| `dygraphs` | `legend` | Whether and how the legend is displayed. |
| `dygraphs` | `shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `size` | Source-level setting for size; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `stackedGraph` | Source-level setting for stacked graph; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `xAxis.max` | Source-level setting for max; see the wrapper listed here for its accepted values and effect. |
| `dygraphs` | `xAxis.min` | Source-level setting for min; see the wrapper listed here for its accepted values and effect. |
| `echarts` | `aggrigator.function` | Source-level setting for function; see the wrapper listed here for its accepted values and effect. |
| `echarts` | `point.size.max` | Source-level setting for size max; see the wrapper listed here for its accepted values and effect. |
| `echarts` | `point.size.min` | Source-level setting for size min; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `hist.bar.center` | Source-level setting for bar center; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `hist.bar.width` | Source-level setting for bar width; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `point.border.color` | Source-level setting for border color; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `point.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `point.opacity` | Source-level setting for opacity; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `point.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `ggvis` | `point.size` | Size of plotted points. |
| `ggvis` | `point.tooltip` | Tooltip field or setting for points. |
| `googleVis` | `greenFrom` | Source-level setting for green from; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `greenTo` | Source-level setting for green to; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `max` | Source-level setting for max; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `min` | Source-level setting for min; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `redFrom` | Source-level setting for red from; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `redTo` | Source-level setting for red to; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `theta.max` | Source-level setting for max; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `theta.min` | Source-level setting for min; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `thetaAxis.zone` | Value ranges and colors for gauge bands. |
| `googleVis` | `y2Axis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `yAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `yellowFrom` | Source-level setting for yellow from; see the wrapper listed here for its accepted values and effect. |
| `googleVis` | `yellowTo` | Source-level setting for yellow to; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `direction` | Source-level setting for direction; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `layout` | Source-level setting for layout; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.arrow.head` | Source-level setting for arrow head; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.arrow.size` | Source-level setting for arrow size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.arrow.tail` | Source-level setting for arrow tail; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.direction` | Source-level setting for direction; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.label.color` | Source-level setting for label color; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.label.font` | Source-level setting for label font; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.label.size` | Source-level setting for label size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.length.max` | Source-level setting for length max; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.length.min` | Source-level setting for length min; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.width.max` | Source-level setting for width max; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `link.width.min` | Source-level setting for width min; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.border.color` | Source-level setting for border color; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.border.peripheries` | Source-level setting for border peripheries; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.distortion` | Source-level setting for distortion; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.fixedSize` | Source-level setting for fixed size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.height` | Fixed node height. |
| `grviz` | `node.highlight.color` | Source-level setting for highlight color; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.label.color` | Source-level setting for label color; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.label.font` | Source-level setting for label font; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.label.size` | Source-level setting for label size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.shadow.enabled` | Source-level setting for shadow enabled; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.shadow.size` | Source-level setting for shadow size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.size` | Source-level setting for size; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.size.max` | Source-level setting for size max; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.size.min` | Source-level setting for size min; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.size.ratio` | Ratio used to convert node size to node height. |
| `grviz` | `node.tooltip` | Source-level setting for tooltip; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `node.width` | Fixed node width. |
| `grviz` | `shinyInput.click` | Source-level setting for click; see the wrapper listed here for its accepted values and effect. |
| `grviz` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `aggregator` | Aggregation function applied to values. |
| `highcharter` | `background.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `barMode` | Source-level setting for bar mode; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `border.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `border.radius` | Source-level setting for radius; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `border.width` | Source-level setting for width; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `legend` | Whether and how the legend is displayed. |
| `highcharter` | `legend.item.color` | Source-level setting for item color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `legend.item.font` | Source-level setting for item font; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `legend.item.hover.color` | Source-level setting for item hover color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `legend.item.hover.font` | Source-level setting for item hover font; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `link.smooth.enabled` | Source-level setting for smooth enabled; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `subtitle` | Source-level setting for subtitle; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `subtitle.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `subtitle.font` | Source-level setting for font; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `title.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `title.font` | Source-level setting for font; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `tooltip` | Source-level setting for tooltip; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `xAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `yAxis.gridLine.width` | Source-level setting for grid line width; see the wrapper listed here for its accepted values and effect. |
| `highcharter` | `yAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `highcharts` | `horizontal` | Source-level setting for horizontal; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `resize` | Source-level setting for resize; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `smooth` | Source-level setting for smooth; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `tooltip` | Source-level setting for tooltip; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `yAxis.max` | Source-level setting for max; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `yAxis.min` | Source-level setting for min; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `yAxis.tick.label.prefix` | Source-level setting for tick label prefix; see the wrapper listed here for its accepted values and effect. |
| `morrisjs` | `yAxis.tick.label.suffix` | Source-level setting for tick label suffix; see the wrapper listed here for its accepted values and effect. |
| `networkD3` | `node.label.size` | Source-level setting for label size; see the wrapper listed here for its accepted values and effect. |
| `networkD3` | `node.width` | Fixed node width. |
| `nvd3` | `donut` | Source-level setting for donut; see the wrapper listed here for its accepted values and effect. |
| `nvd3` | `palette` | Source-level setting for palette; see the wrapper listed here for its accepted values and effect. |
| `nvd3` | `xAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `nvd3` | `yAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `nvd3` | `zoomWindow` | Source-level setting for zoom window; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `legend.background` | Source-level setting for background; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `legend.enabled` | Source-level setting for enabled; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `palette.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `palette.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `point.border.color` | Source-level setting for border color; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `point.border.width` | Source-level setting for border width; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `point.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `point.size` | Size of plotted points. |
| `plotly` | `point.size.max` | Source-level setting for size max; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `point.size.min` | Source-level setting for size min; see the wrapper listed here for its accepted values and effect. |
| `plotly` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `rCharts` | `horizontal` | Source-level setting for horizontal; see the wrapper listed here for its accepted values and effect. |
| `rbokeh` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `rbokeh` | `tooltip` | Source-level setting for tooltip; see the wrapper listed here for its accepted values and effect. |
| `rbokeh` | `xAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `rbokeh` | `yAxis.label` | Source-level setting for label; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `column.acceptor` | Source-level setting for acceptor; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `column.editable` | Source-level setting for editable; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `column.filter` | Source-level setting for filter; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `column.footer` | Source-level setting for footer; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `column.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `row.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `selected` | Source-level setting for selected; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `selection.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `rscripts` | `withRowNames` | Source-level setting for with row names; see the wrapper listed here for its accepted values and effect. |
| `sankeytree` | `aggregator` | Aggregation function applied to values. |
| `sankeytree` | `height` | Source-level setting for height; see the wrapper listed here for its accepted values and effect. |
| `sankeytree` | `node.height` | Fixed node height. |
| `sankeytree` | `node.label.size.max` | Source-level setting for label size max; see the wrapper listed here for its accepted values and effect. |
| `sankeytree` | `title` | Source-level setting for title; see the wrapper listed here for its accepted values and effect. |
| `sankeytree` | `width` | Source-level setting for width; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `aggregator` | Aggregation function applied to values. |
| `streamgraph` | `shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `stack.enabled` | Source-level setting for enabled; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `stack.groups` | Source-level setting for groups; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `xAxis.tick.format` | Source-level setting for tick format; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `xAxis.tick.interval` | Source-level setting for tick interval; see the wrapper listed here for its accepted values and effect. |
| `streamgraph` | `xAxis.tick.unit` | Source-level setting for tick unit; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.color.inherit` | Source-level setting for color inherit; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.color.opacity` | Source-level setting for color opacity; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.dashed` | Source-level setting for dashed; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.highlight.color` | Source-level setting for highlight color; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.hover.color` | Source-level setting for hover color; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.label.background` | Source-level setting for label background; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.label.font` | Source-level setting for label font; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.loop.size` | Source-level setting for loop size; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.selection.width` | Source-level setting for selection width; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.shape` | Source-level setting for shape; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `link.smooth` | Link curve or smoothing setting. |
| `visNetwork` | `node.label.background` | Source-level setting for label background; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `node.label.color` | Source-level setting for label color; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `node.label.size` | Source-level setting for label size; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `node.shadow.size` | Source-level setting for shadow size; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `node.size` | Source-level setting for size; see the wrapper listed here for its accepted values and effect. |
| `visNetwork` | `node.tooltip` | Source-level setting for tooltip; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `additionalColumns` | Source-level setting for additional columns; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `color` | Source-level setting for color; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `colorize` | Source-level setting for colorize; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `dimClass` | Source-level setting for dim class; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `dimclass` | Source-level setting for dimclass; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `multiples` | Source-level setting for multiples; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `palette` | Source-level setting for palette; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `presort` | Source-level setting for presort; see the wrapper listed here for its accepted values and effect. |
| `visgen` | `withRowNames` | Source-level setting for with row names; see the wrapper listed here for its accepted values and effect. |

## General interpretation

Axis keys control labels, ticks, grid lines, ranges, and scrollbars.
Legend keys control visibility, placement, and appearance. `title` and
`subtitle` set chart headings. Graph wrappers use `node.*` and `link.*`
keys for network elements. Keys such as `additionalColumns` retain extra
columns for custom tooltips or formatting. The exact wrapper behavior is
implemented in that wrapper’s manual and source code; values and
defaults in the table are the package’s current validation metadata.

When a key is not present in this table, check the wrapper’s function
documentation and the `...` arguments it forwards to its underlying
plotting package.
