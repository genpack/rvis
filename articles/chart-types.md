# Chart type and plotter reference

This reference documents the chart methods callable through
`rvisPlot(type, plotter, ...)`. Dispatch uses the exact function name
`plotter.type`; spelling and capitalization matter. The method inventory
below is parsed from the wrapper source rather than inferred from the
older compatibility matrix.

Each table gives the exact wrapper function and its formal arguments.
Dimension values usually refer to columns in `obj`, while graph and
table methods can accept specialized objects. `config` is a named list
of wrapper-specific settings; the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md)
lists all keys currently recorded by the package.

Rendered examples of every chart type and plotter are in the topic
guides, each built on its own dataset. Each section below links to the
examples of its chart type.

| Guide | Chart types | Data |
|:---|:---|:---|
| [Comparing categories](https://genpack.github.io/rvis/articles/comparing-categories.md) | bar, line, area, area range, combo | `VADeaths` |
| [Parts of a whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.md) | pie, funnel, pyramid, gauge, bullet, word cloud | `HairEyeColor` |
| [Relationships and distributions](https://genpack.github.io/rvis/articles/relationships-distributions.md) | scatter, bubble, histogram, box | `iris` |
| [Time series](https://genpack.github.io/rvis/articles/time-series.md) | time series line, area and bar, stock, calendar | `airquality` |
| [Networks and hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.md) | graph, sankey, sunburst, tree, treemap | `Titanic` |
| [Tables](https://genpack.github.io/rvis/articles/tables.md) | interactive, filterable and pivot tables | `USArrests` |

Packages that are not wrapped yet, and how to add a plotter, are in
[Adding
plotters](https://genpack.github.io/rvis/articles/adding-plotters.md).

## Common argument conventions

| Argument | Common meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by the wrapper. |
| `x`, `y`, `y2`, `z` | Axis or numeric dimensions. |
| `label`, `theta`, `group`, `color`, `shape`, `size` | Category, measure, grouping, and visual mappings. |
| `low`, `high` | Range bounds or interval endpoints. |
| `source`, `target`, `linkWidth`, `key` | Network links and identifiers. |
| `config` | Named list of settings supported by the selected wrapper. |
| `...` | Additional arguments forwarded by wrappers that support forwarding. |

## `bar` charts

Call `rvisPlot(type = "bar", plotter = "<wrapper>", ...)` to select this
chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.bar` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |
| `billboarder` | `billboarder.bar` | `obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `plotly` | `plotly.bar` | `..., config = NULL` |
| `c3` | `c3.bar` | `obj, x = NULL, y = NULL, color = NULL, config = NULL` |
| `highcharter` | `highcharter.bar` | `obj, x = NULL, y = NULL, color = NULL, config = NULL` |
| `d3plus` | `d3plus.bar` | `obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `googleVis` | `googleVis.bar` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `echarts` | `echarts.bar` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |
| `apexcharter` | `apexcharter.bar` | `obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `nvd3` | `nvd3.bar` | `obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `candela` | `candela.bar` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |
| `dc` | `dc.bar` | `obj, x = NULL, y = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#bar).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `group` | Series or category grouping field. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `bar.molten` charts

Call `rvisPlot(type = "bar.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.bar.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#bar-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `pie` charts

Call `rvisPlot(type = "pie", plotter = "<wrapper>", ...)` to select this
chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.pie` | `obj, label = NULL, theta = NULL, config = NULL, ...` |
| `billboarder` | `billboarder.pie` | `obj, label = NULL, theta = NULL, config = NULL, ...` |
| `plotly` | `plotly.pie` | `obj, label = NULL, theta = NULL, config = NULL, ...` |
| `c3` | `c3.pie` | `obj, theta = NULL, label = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.pie` | `obj, label = NULL, theta = NULL, color = NULL, config = NULL` |
| `morrisjs` | `morrisjs.pie` | `obj = data.frame(), theta = NULL, label = NULL, color = NULL, config = NULL, ...` |
| `nvd3` | `nvd3.pie` | `obj, theta = NULL, label = NULL, config = NULL, ...` |
| `apexcharter` | `apexcharter.pie` | `obj, label = NULL, theta = NULL, color = NULL, config = NULL, ...` |
| `echarts` | `echarts.pie` | `obj, label = NULL, theta = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#pie).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `label` | Category or text label field. |
| `theta` | Numeric measure for slice size, gauge value, or angular position. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `color` | Color mapping field or value. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `pie.molten` charts

Call `rvisPlot(type = "pie.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function          | Formal arguments                        |
|:----------------|:------------------|:----------------------------------------|
| `nvd3`          | `nvd3.pie.molten` | `obj, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#pie-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `scatter` charts

Call `rvisPlot(type = "scatter", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `billboarder` | `billboarder.scatter` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |
| `plotly` | `plotly.scatter` | `obj, x, y, z = NULL, size = NULL, shape = NULL, color = NULL, label = NULL, tooltip = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.scatter` | `obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...` |
| `d3plus` | `d3plus.scatter` | `obj, x = NULL, y = NULL, size = NULL, label = NULL, config = NULL, ...` |
| `ggvis` | `ggvis.scatter` | `obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...` |
| `nvd3` | `nvd3.scatter` | `obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...` |
| `candela` | `candela.scatter` | `obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, config = NULL, ...` |
| `dc` | `dc.scatter` | `obj, x = NULL, y = NULL, group = NULL, shape = NULL, config = NULL, ...` |
| `dimple` | `dimple.scatter` | `obj, x = NULL, y = NULL, t = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...` |
| `rbokeh` | `rbokeh.scatter` | `obj, x = NULL, y = NULL, color = NULL, shape = NULL, config = NULL, ...` |
| `rCharts` | `rCharts.scatter` | `obj, x = NULL, y = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...` |
| `apexcharter` | `apexcharter.scatter` | `obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `echarts` | `echarts.scatter` | `obj, x = NULL, y = NULL, size = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#scatter).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `z` | Third numeric dimension, often depth or bubble size. |
| `size` | Mark or node size field or value. |
| `shape` | Shape mapping field or value. |
| `color` | Color mapping field or value. |
| `label` | Category or text label field. |
| `tooltip` | Field or value displayed in hover text. |
| `t` | Time or grouping field. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `scatter.molten` charts

Call `rvisPlot(type = "scatter.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `c3` | `c3.scatter.molten` | `obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.scatter.molten` | `obj, x = NULL, y = NULL, size = NULL, group = NULL, color = NULL, config = NULL, ...` |
| `highcharts` | `highcharts.scatter.molten` | `obj, x = NULL, y = NULL, size = NULL, shape = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#scatter-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `shape` | Shape mapping field or value. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `combo` charts

Call `rvisPlot(type = "combo", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `plotly` | `plotly.combo` | `obj, x = NULL, y = NULL, shape = NULL, color = NULL, config = NULL, tooltip = NULL, ...` |
| `c3` | `c3.combo` | `obj, x = NULL, y = NULL, shape = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.combo` | `obj, x = NULL, y = NULL, size = NULL, low = NULL, high = NULL, color = NULL, linkColor = NULL, shape = NULL, config = NULL` |
| `dimple` | `dimple.combo` | `obj, x = NULL, y = NULL, t = NULL, size = NULL, group = NULL, shape = NULL, color = NULL, config = NULL, ...` |
| `dygraphs` | `dygraphs.combo` | `obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#combo).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `shape` | Shape mapping field or value. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `tooltip` | Field or value displayed in hover text. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

## Legacy chart aliases without dispatch methods

The previous compatibility matrix lists `combo.molten`, `heatmap`, and
`map`. The current wrapper source does not define `dimple.combo.molten`
or `highcharts.heatmap`, and Leaflet’s function is named
`leaflet.map.plot`. Since
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
dispatches by exact function name, the first two pairs are not callable
through
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md) as
currently implemented; use `type = "map.plot"` for the existing Leaflet
wrapper.

The exported `valid.plot.types` vector also contains `calheat` and
`motion`, but no wrapper methods with those suffixes are currently
defined. The calendar wrapper is dispatched with `type = "calendar"`.

### `calheat`

No `plotter.calheat` wrapper is currently defined. Use the `calendar`
chart method for the `calheatmap` wrapper.

### `motion`

No wrapper method with the `motion` suffix is currently defined.

### `combo.molten`

There is no current function named `dimple.combo.molten`. Use a
non-molten `combo` method that appears in the chart table, or reshape
the data to the input format supported by that method.

### `heatmap`

There is no current function named `highcharts.heatmap`. The capability
table lists this pairing, but
`rvisPlot(type = "heatmap", plotter = "highcharts")` cannot dispatch to
a wrapper today.

### `map`

The capability table calls the Leaflet method `map`, while the function
is `leaflet.map.plot`. Use the exact wrapper dispatch shown in the
[`map.plot` section](#map-plot).

## `box` charts

Call `rvisPlot(type = "box", plotter = "<wrapper>", ...)` to select this
chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.box` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `plotly` | `plotly.box` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |
| `bpexploder` | `bpexploder.box` | `obj, y = NULL, group = NULL, config = NULL, ...` |

The current `bpexploder.box()` implementation declares `group` but
refers to an undefined `x` while preparing its data. That method may
fail unless an unrelated `x` object exists in the calling environment.
Use `amCharts.box()` or `plotly.box()` for this example until the
wrapper is repaired.

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#box).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `group` | Series or category grouping field. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `line` charts

Call `rvisPlot(type = "line", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `c3` | `c3.line` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.line` | `obj, x = NULL, y = NULL, color = NULL, config = NULL` |
| `googleVis` | `googleVis.line` | `obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#line).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `color` | Color mapping field or value. |
| `y2` | Secondary vertical-axis measure or series. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `line.molten` charts

Call `rvisPlot(type = "line.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.line.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#line-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `area` charts

Call `rvisPlot(type = "area", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `c3` | `c3.area` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.area` | `obj, x = NULL, y = NULL, color = NULL, config = NULL` |
| `googleVis` | `googleVis.area` | `obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#area).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `color` | Color mapping field or value. |
| `y2` | Secondary vertical-axis measure or series. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `area.molten` charts

Call `rvisPlot(type = "area.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.area.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |
| `xCharts` | `xCharts.area.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#area-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `area.range` charts

Call `rvisPlot(type = "area.range", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `highcharter` | `highcharter.area.range` | `obj, x = NULL, low = NULL, high = NULL, color = NULL, config = NULL` |

### Examples

Examples for every plotter of this chart type are in [Comparing
categories](https://genpack.github.io/rvis/articles/comparing-categories.html#area-range).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `low` | Lower bound for a range or interval. |
| `high` | Upper bound for a range or interval. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `graph` charts

Call `rvisPlot(type = "graph", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `grviz` | `grviz.graph` | `obj, key = NULL, label = NULL, shape = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL, labelColor = NULL, source = NULL, target = NULL, linkColor = NULL, linkTooltip = NULL, linkWidth = NULL, linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL, config = NULL, ...` |
| `visNetwork` | `visNetwork.graph` | `obj, key = NULL, label = NULL, shape = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL, source = NULL, target = NULL, linkColor = NULL, linkLength = NULL, linkWidth = NULL, linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.html#graph).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `key` | Identifier field for rows or graph elements. |
| `label` | Category or text label field. |
| `shape` | Shape mapping field or value. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `borderColor` | Column or color value used for the outline of a node or mark. |
| `tooltip` | Field or value displayed in hover text. |
| `labelColor` | Column or color value for text labels. |
| `source` | Source-node field for a network edge. |
| `target` | Target-node field for a network edge. |
| `linkColor` | Column or color value for network links. |
| `linkTooltip` | Column displayed as hover text for network links. |
| `linkWidth` | Field or value controlling network link width. |
| `linkLabel` | Column used as text on network links. |
| `linkLabelColor` | Column or color value for network link labels. |
| `linkLabelSize` | Column or value controlling network link label size. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `linkLength` | Column or value controlling the visual length of network links. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `graphChart` charts

[`visNetwork.graphChart()`](https://genpack.github.io/rvis/reference/rvis-defunct.md)
is currently an exported placeholder: its body returns an undefined
object (`v`).
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md) can
resolve the function name, but it does not currently produce a chart.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `visNetwork` | `visNetwork.graphChart` | `nodes, links, size = NULL, sizeLegend = NULL, shape = NULL, shapeLegend = NULL, color = NULL, colorLegend = NULL, label = NULL, linkColor = NULL, linkLength = NULL, linkLengthLegend = NULL, linkSource = NULL, linkTarget = NULL, linkWidth = NULL, ...` |

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `nodes` | Data frame of network nodes. |
| `links` | Data frame of network links or edges. |
| `size` | Mark or node size field or value. |
| `sizeLegend` | Legend settings for the size dimension. |
| `shape` | Shape mapping field or value. |
| `shapeLegend` | Legend settings or labels for point shapes. |
| `color` | Color mapping field or value. |
| `colorLegend` | Legend settings or labels for color categories. |
| `label` | Category or text label field. |
| `linkColor` | Column or color value for network links. |
| `linkLength` | Column or value controlling the visual length of network links. |
| `linkLengthLegend` | Legend settings for link length. |
| `linkSource` | Source field used to define network links. |
| `linkTarget` | Target field used to define network links. |
| `linkWidth` | Field or value controlling network link width. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `bubble` charts

Call `rvisPlot(type = "bubble", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `highcharter` | `highcharter.bubble` | `obj, x = NULL, y = NULL, size = NULL, color = NULL, config = NULL` |
| `d3plus` | `d3plus.bubble` | `obj, size = NULL, label = NULL, config = NULL, ...` |
| `bubbles` | `bubbles.bubble` | `obj, size = NULL, color = NULL, label = NULL, labelColor = NULL, tooltip = NULL, config = bubbles.defset, ...` |

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#bubble).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `label` | Category or text label field. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `labelColor` | Column or color value for text labels. |
| `tooltip` | Field or value displayed in hover text. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `bubble.molten` charts

Call `rvisPlot(type = "bubble.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.bubble.molten` | `obj, size = NULL, label = NULL, group = NULL, config = NULL, ...` |
| `bubbleCloud` | `bubbleCloud.bubble.molten` | `obj, size = NULL, group = NULL, tooltip = NULL, config = bubbleCloud.bubble.molten.defset, ...` |

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#bubble-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `size` | Mark or node size field or value. |
| `label` | Category or text label field. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `tooltip` | Field or value displayed in hover text. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tree` charts

Call `rvisPlot(type = "tree", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.tree` | `obj, label = NULL, size = NULL, color = NULL, config = NULL, ...` |
| `sankeytree` | `sankeytree.tree` | `obj, label = NULL, size = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.html#tree).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `label` | Category or text label field. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `treemap` charts

Call `rvisPlot(type = "treemap", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `highcharter` | `highcharter.treemap` | `obj, label = NULL, size = NULL, color = NULL, config = NULL` |
| `d3plus` | `d3plus.treemap` | `obj, label = NULL, size = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.html#treemap).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `label` | Category or text label field. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `table` charts

Call `rvisPlot(type = "table", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `DT` | `DT.table` | `obj, label = NULL, config = NULL, sessionobj = NULL, ...` |
| `D3TableFilter` | `D3TableFilter.table` | `obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...` |
| `pivot` | `pivot.table` | `obj, ...` |
| `TFD3` | `TFD3.table` | `obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in
[Tables](https://genpack.github.io/rvis/articles/tables.html#table).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `label` | Category or text label field. |
| `config` | Named list of settings supported by this wrapper. |
| `sessionobj` | Session-specific object used by the table wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `color` | Color mapping field or value. |
| `shape` | Shape mapping field or value. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `table.molten` charts

Call `rvisPlot(type = "table.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `TFD3` | `TFD3.table.molten` | `obj, label = NULL, color = NULL, shape = NULL, group = NULL, config = NULL` |

### Examples

Examples for every plotter of this chart type are in
[Tables](https://genpack.github.io/rvis/articles/tables.html#table-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning                                              |
|:---------|:-----------------------------------------------------|
| `obj`    | Input data frame or object accepted by this wrapper. |
| `label`  | Category or text label field.                        |
| `color`  | Color mapping field or value.                        |
| `shape`  | Shape mapping field or value.                        |
| `group`  | Series or category grouping field.                   |
| `config` | Named list of settings supported by this wrapper.    |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `Table` charts

Call `rvisPlot(type = "Table", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `DT` | `DT.Table` | `x, links = NULL, session = NULL, options = list(), rownames = T` |

### Examples

Examples for every plotter of this chart type are in
[Tables](https://genpack.github.io/rvis/articles/tables.html#Table).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `links` | Data frame of network links or edges. |
| `session` | Shiny session used for interactive table behavior. |
| `options` | Named list of DataTables options. |
| `rownames` | Whether row names are shown in the table. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `histogram` charts

Call `rvisPlot(type = "histogram", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function          | Formal arguments                    |
|:----------------|:------------------|:------------------------------------|
| `ggvis`         | `ggvis.histogram` | `obj, x = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Relationships and
distributions](https://genpack.github.io/rvis/articles/relationships-distributions.html#histogram).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `map.plot` charts

The Leaflet function is named `leaflet.map.plot`, so it dispatches as
`type = "map.plot"`. The legacy compatibility matrix labels it `map`,
but `rvisPlot(type = "map", plotter = "leaflet")` does not match the
current function name.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `leaflet` | `leaflet.map.plot` | `x, stores = rownames(sg$spec), tiles = F, icon = NULL` |

### Input requirement

This function only handles objects inheriting from `STORE.GROUP`; it
reads store specifications through a global `sg` object and does not
accept an ordinary data frame. There is no self-contained synthetic
example for this legacy wrapper. Its current implementation should be
repaired before it is used as a general
[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md) map
example.

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `stores` | Store identifiers selected from a STORE.GROUP object. |
| `tiles` | Whether to add a map tile layer. |
| `icon` | Leaflet marker icon definition. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsline` charts

Call `rvisPlot(type = "tsline", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.tsline` | `obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, high = NULL, low = NULL, config = NULL, ...` |
| `billboarder` | `billboarder.tsline` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |
| `plotly` | `plotly.tsline` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `c3` | `c3.tsline` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `d3plus` | `d3plus.tsline` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |
| `morrisjs` | `morrisjs.tsline` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |
| `dygraphs` | `dygraphs.tsline` | `obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsline).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `shape` | Shape mapping field or value. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `high` | Upper bound for a range or interval. |
| `low` | Lower bound for a range or interval. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `group` | Series or category grouping field. |
| `y2` | Secondary vertical-axis measure or series. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsline.molten` charts

Call `rvisPlot(type = "tsline.molten", plotter = "<wrapper>", ...)` to
select this chart method. The series come from the values of `group`.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.tsline.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsline-molten).

### Arguments

| Argument | Meaning                                                  |
|:---------|:---------------------------------------------------------|
| `obj`    | Input data frame in long format.                         |
| `x`      | Time column.                                             |
| `y`      | Measure column.                                          |
| `group`  | Column whose values give the series.                     |
| `config` | Named list of settings supported by this wrapper.        |
| `...`    | Additional arguments forwarded to the plotting function. |

## `tsLineChart` charts

Call `rvisPlot(type = "tsLineChart", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `billboarder` | `billboarder.tsLineChart` | `obj, x = NULL, y = NULL, color = NULL, shape = "spline", config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsLineChart).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `color` | Color mapping field or value. |
| `shape` | Shape mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsline.stock` charts

Call `rvisPlot(type = "tsline.stock", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `highcharter` | `highcharter.tsline.stock` | `obj, x = NULL, y = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsline-stock).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsarea` charts

Call `rvisPlot(type = "tsarea", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `billboarder` | `billboarder.tsarea` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |
| `c3` | `c3.tsarea` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `streamgraph` | `streamgraph.tsarea` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsarea).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `group` | Series or category grouping field. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsarea.molten` charts

Call `rvisPlot(type = "tsarea.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.tsarea.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsarea-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsbar` charts

Call `rvisPlot(type = "tsbar", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `plotly` | `plotly.tsbar` | `obj, x = NULL, y = NULL, config = NULL, ...` |
| `morrisjs` | `morrisjs.tsbar` | `obj, x = NULL, y = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsbar).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `color` | Color mapping field or value. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `tsbar.molten` charts

Call `rvisPlot(type = "tsbar.molten", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `d3plus` | `d3plus.tsbar.molten` | `obj, x = NULL, y = NULL, group = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#tsbar-molten).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `group` | Series or category grouping field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `sankey` charts

Call `rvisPlot(type = "sankey", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `googleVis` | `googleVis.sankey` | `obj, key = NULL, source = NULL, target = NULL, linkWidth = NULL, config = NULL` |
| `networkD3` | `networkD3.sankey` | `obj, key = NULL, label = NULL, tooltip = NULL, source = NULL, target = NULL, linkWidth = NULL, linkTooltip = NULL, config = NULL` |

### Examples

Examples for every plotter of this chart type are in [Networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.html#sankey).

### Arguments

Common meanings for the arguments on these methods:

| Argument      | Meaning                                              |
|:--------------|:-----------------------------------------------------|
| `obj`         | Input data frame or object accepted by this wrapper. |
| `key`         | Identifier field for rows or graph elements.         |
| `source`      | Source-node field for a network edge.                |
| `target`      | Target-node field for a network edge.                |
| `linkWidth`   | Field or value controlling network link width.       |
| `config`      | Named list of settings supported by this wrapper.    |
| `label`       | Category or text label field.                        |
| `tooltip`     | Field or value displayed in hover text.              |
| `linkTooltip` | Column displayed as hover text for network links.    |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `wordCloud` charts

Call `rvisPlot(type = "wordCloud", plotter = "<wrapper>", ...)` to
select this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `echarts` | `echarts.wordCloud` | `obj, label = NULL, size = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#wordCloud).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `label` | Category or text label field. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `funnel` charts

Call `rvisPlot(type = "funnel", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.funnel` | `obj, y = NULL, label = NULL, config = NULL, ...` |
| `highcharter` | `highcharter.funnel` | `obj, label = NULL, size = NULL, color = NULL, config = NULL` |
| `echarts` | `echarts.funnel` | `obj, label = NULL, size = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#funnel).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `y` | Vertical measure or named series. |
| `label` | Category or text label field. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `size` | Mark or node size field or value. |
| `color` | Color mapping field or value. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `gauge` charts

Call `rvisPlot(type = "gauge", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.gauge` | `obj = data.frame(), theta = NULL, config = NULL, ...` |
| `c3` | `c3.gauge` | `theta, config = NULL, ...` |
| `googleVis` | `googleVis.gauge` | `obj = data.frame(), theta = NULL, label = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#gauge).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `theta` | Numeric measure for slice size, gauge value, or angular position. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |
| `label` | Category or text label field. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `bullet` charts

Call `rvisPlot(type = "bullet", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `amCharts` | `amCharts.bullet` | `obj = data.frame(), x = NULL, y = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#bullet).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `x` | Horizontal axis, time, or primary input; interpretation is wrapper-specific. |
| `y` | Vertical measure or named series. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `sunburst` charts

Call `rvisPlot(type = "sunburst", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `coffeewheel` | `coffeewheel.sunburst` | `obj, theta = NULL, label = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Networks and
hierarchies](https://genpack.github.io/rvis/articles/networks-hierarchies.html#sunburst).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `theta` | Numeric measure for slice size, gauge value, or angular position. |
| `label` | Category or text label field. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `calendar` charts

Call `rvisPlot(type = "calendar", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `calheatmap` | `calheatmap.calendar` | `obj, t = NULL, color = NULL, config = NULL, ...` |

### Examples

Examples for every plotter of this chart type are in [Time
series](https://genpack.github.io/rvis/articles/time-series.html#calendar).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning |
|:---|:---|
| `obj` | Input data frame or object accepted by this wrapper. |
| `t` | Time or grouping field. |
| `color` | Color mapping field or value. |
| `config` | Named list of settings supported by this wrapper. |
| `...` | Additional arguments forwarded when the wrapper supports forwarding. |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## `pyramid` charts

Call `rvisPlot(type = "pyramid", plotter = "<wrapper>", ...)` to select
this chart method.

### Wrapper methods

| `plotter` value | Function | Formal arguments |
|:---|:---|:---|
| `highcharter` | `highcharter.pyramid` | `obj, label = NULL, size = NULL, color = NULL, config = NULL` |

### Examples

Examples for every plotter of this chart type are in [Parts of a
whole](https://genpack.github.io/rvis/articles/parts-of-a-whole.html#pyramid).

### Arguments

Common meanings for the arguments on these methods:

| Argument | Meaning                                              |
|:---------|:-----------------------------------------------------|
| `obj`    | Input data frame or object accepted by this wrapper. |
| `label`  | Category or text label field.                        |
| `size`   | Mark or node size field or value.                    |
| `color`  | Color mapping field or value.                        |
| `config` | Named list of settings supported by this wrapper.    |

### Configuration

Use only settings accepted by the selected wrapper. Configuration keys
are not universal: a key supported by one plotter may be ignored or
rejected by another. See the [configuration
reference](https://genpack.github.io/rvis/articles/configuration.md) for
key types, defaults, allowed values, and a plain-language description.

## Dispatch caveats

[`rvisPlot()`](https://genpack.github.io/rvis/reference/rvisPlot.md)
constructs and evaluates the wrapper function name; it does not use
`data/plotters.csv` to validate the selection. The CSV includes
historical aliases, so the exact names listed above are authoritative
for dispatch. Wrapper methods can also differ in required input shape
even when they share a chart type.
