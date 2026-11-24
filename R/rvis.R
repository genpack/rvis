# rvis.R ----------------------------------------------------------------

#' rvis: This package is a tool-box for visualization in R and making word-class and elegant shiny dashboards.
#'
#' @section Class DASHBOARD:
#' rvis provides a Reference class named as DASHBOARD.
#' You can design your interactive dashboard layout and assign multiple R objects into your dashboard.
#' It supports various visualization engines like graphics, ggplot2, ggvis, googlevis, amCharts, rCharts, dygraphs and many other plotter engines.
#'
#' @docType package
#' @name rvis
#'
#' @import rutils
#' @import magrittr
#'
#' @include visgen.R
#' @include dashtools.R
#' @include dashboard.R
#' @include dialogs.R
#' @include jscripts.R
#' @include rscripts.R
#' @include bubbles.R
#' @include bpexploder.R
#' @include bubblecloud.R
#' @include c3.R
#' @include coffeewheel.R
#' @include candela.R
#' @include d3plus.R
#' @include tfd3.R
#' @include dimple.R
#' @include dt.R
#' @include d3tablefilter.R
#' @include dc.R
#' @include dfvis.R
#' @include grviz.R
#' @include pivot.R
#' @include csscripts.R
#' @include calheatmap.R
#' @include sankeytree.R
#' @include streamgraph.R
#' @include dygraphs.R
#' @include echarts.R
#' @include ggvis.R
#' @include googlevis.R
#' @include highcharter.R
#' @include highcharts.R
#' @include leaflet.R
#' @include morrisjs.R
#' @include networkd3.R
#' @include nvd3.R
#' @include plotly.R
#' @include amcharts.R
#' @include rbokeh.R
#' @include rcharts.R
#' @include visnetwork.R
#' @include xcharts.R

#' @include rvisplot.R

# Version History:
# 1.1.0 (04 July 2016)      - dashboard.R changed to version 1.6
# 1.2.0 (20 July 2016)      - nira_plotters.R added (Version 1.1.0)
# 1.2.1 (08 September 2016) - dashboard.R changed to version 1.6.2
# 1.2.3 (07 September 2016) - nira_plotters.R changed to version 1.1.3
# 1.2.7 (08 September 2016) - tools.R renamed to dashtools.R and changed to version 1.7
# 1.3.0 (29 September 2016) - deviset.R transferred from package timser
# 1.3.1 (29 September 2016) - visgen.R transferred from package niragen
# 1.3.2 (29 September 2016) - nira_plotters.R changed to version 1.1.4
# 1.4.0 (06 October 2016)   - nira_plotters.R changed to version 1.2.1 and renamed to: rvisPlotter.R
# 1.4.1 (12 October 2016)   - dashboard.R changed to version 1.6.3
# 1.5.0 (14 October 2016)   - highcharterPlotters.R (version 1.0.0) added to the package
# 1.6.0 (18 October 2016)   - bubblesPlotters.R (version 1.0.0) added to the package
# 1.7.0 (25 November 2016)  - rChartsPlotters.R (version 1.1.0) added to the package
# 1.8.0 (25 November 2016)  - plotlyPlotters.R (version 1.0.0) added to the package
# 2.0.0 (28 November 2016)  - networkD3Plotters.R (version 1.0.0) added to the package
# 2.1.0 (28 November 2016)  - rAmChartsPlotters.R (version 1.0.0) added to the package
# 2.2.0 (29 November 2016)  - googleVisPlotters.R (version 1.2.0) added to the package
# 2.2.1 (25 December 2016)  - DT.R added
# 2.2.2 (29 December 2016)  - All plotter files renamed: 'Plotter' removed from the name. For example 'googleVisPlotters.R' renamed to 'googleVis.R'.
# 2.2.3 (29 December 2016)  - dygraphs.R added
# 2.2.4 (29 December 2016)  - leaflet.R added
# 2.2.5 (29 December 2016)  - plotly.R added
# 2.2.6 (02 February 2017)  - dygraphs.R updated to version 1.1.0
# 2.3.0 (28 March 2017)     - DT.R updated to version 1.1.1
# 2.4.0 (31 March 2017)     - dygraphs.R updated to version 1.2.2
# 2.4.1 (31 March 2017)     - bubbles.R updated to version 1.1.2
# 2.5.0 (02 April 2017)     - googleVis.R updated to version 1.3.0
# 2.6.0 (13 April 2017)     - c3.R added (version 0.0.1)
# 2.6.1 (14 April 2017)     - coffeewheel.R added (version 0.0.1)
# 2.6.2 (14 April 2017)     - d3plus.R added and updated to version 1.1.2
# 2.6.3 (14 April 2017)     - ggvis.R added (version 0.0.1)
# 2.6.4 (14 April 2017)     - rbokeh.R added (version 0.0.1)
# 2.6.5 (19 April 2017)     - amCharts.R updated to version 1.1.2
# 2.6.6 (20 April 2017)     - visNetwork.R updated to version 1.1.1
# 2.7.0 (12 May 2017)       - rCharts.R added and updated to version 1.1.3
# 2.7.1 (12 May 2017)       - dimple.R added (version 1.0.0)
# 2.7.2 (12 May 2017)       - highcharts.R added (version 1.0.0)
# 2.7.3 (12 May 2017)       - nvd3.R added and updated to version 1.0.3
# 2.7.4 (12 May 2017)       - xCharts.R added (version 1.0.0)
# 2.7.6 (13 May 2017)       - networkD3.R updated to version 1.1.0
# 2.8.0 (23 May 2017)       - rscripts.R added (Version 0.0.1)
# 2.8.1 (25 May 2017)       - plotly.R updated to version 1.2.5
# 2.8.2 (26 May 2017)       - morrisjs.R added and updated to version 0.0.4
# 2.9.0 (01 June 2017)      - dashboard.R updated to version 1.9.3
# 2.9.1 (01 June 2017)      - highcharter.R updated to version 1.2.1
# 2.9.2 (13 June 2017)      - jscripts.R added (Version 0.0.1) (Small codes collected from various plotter engine files into one file plus more code added for D3TableFilter)
# 3.0.0 (14 June 2017)      - D3TableFilter.R added and updated to version 0.0.3
# 3.0.1 (13 July 2017)      - visgen.R updated to version 1.2.6
# 3.1.0 (13 July 2017)      - rvisplot.R added and replaced rvisPlotter
# 3.1.2 (20 July 2017)      - coffeewheel embedded to the package! coffeewheel.R and dahboard.R modified
# 3.2.5 (25 July 2017)      - D3TableFilter triggers to re-plot if the sync table changes structure from server side.
#                             Files modified: dashboard.R to version 1.9.5, rscripts.R to version 0.1.1
# 3.2.7 (03 August 2017)    - dashboard.R modified to version 1.9.7
# 3.2.8 (29 August 2017)    - rscripts.R modified to version 0.1.2
# 3.2.1 (29 August 2017)    - coffeewheel.R modified to version 0.0.3
# 3.2.9 (31 August 2017)    - dashboard.R modified to version 1.9.8
# 3.3.0 (31 August 2017)    - D3TableFilter.R renamed to TFD3.R and modified to version 0.0.4
# 3.3.1 (12 September 2017) - TFD3.R modified to version 0.0.5
# 3.4.2 (11 October 2017)   - dashboard.R modified to version 1.10.2
# 3.4.3 (13 October 2017)   - dashboard.R modified to version 1.10.3
# 3.4.4 (16 October 2017)   - dashboard.R modified to version 1.10.4
# 3.5.0 (17 October 2017)   - pivot added to plotters: pivot.R added (ver 0.0.1), dashboard.R modified to version 1.10.5
# 3.5.2 (23 October 2017)   - rscripts.R changed to version 0.1.5
# 3.5.4 (27 October 2017)   - rscripts.R changed to version 0.1.7
# 3.5.5 (01 November 2017)  - TFD3.R changed to version 0.0.6
# 3.6.0 (10 November 2017)  - dashboard.R changed to version 2.0.0
# 3.6.1 (22 November 2017)  - csscript.R added (version 0.0.1)
# 3.7.1 (22 November 2017)  - dashboard.R changed to version 2.1.0
# 3.7.4 (27 November 2017)  - dashboard.R changed to version 2.1.4
# 3.7.5 (27 November 2017)  - rscripts.R changed to version 0.1.8
# 3.7.6 (27 November 2017)  - dashboard.R changed to version 2.1.5
# 3.8.7 (29 November 2017)  - dashboard.R changed to version 2.2.5
# 3.8.8 (29 November 2017)  - csscript.R changed to version 0.0.2
# 3.8.9 (29 November 2017)  - dashboard.R changed to version 2.2.6
# 3.9.0 (06 December 2017)  - rscripts.R changed to version 0.1.9
# 3.9.1 (06 December 2017)  - dashboard.R changed to version 2.2.7
# 4.0.1 (08 December 2017)  - rscripts.R changed to version 0.2.1
# 4.0.2 (08 December 2017)  - dashboard.R changed to version 2.2.8
# 4.1.3 (15 December 2017)  - dashboard.R changed to version 2.3.1
# 4.1.4 (19 December 2017)  - dashboard.R changed to version 2.3.2
# 4.1.5 (15 January 2018)   - TFD3.R changed to version 0.0.7
# 4.1.6 (16 January 2018)   - dashboard.R changed to version 2.3.3
# 4.1.7 (29 January 2018)   - dashboard.R changed to version 2.3.4
# 4.1.8 (12 February 2018)  - dashboard.R changed to version 2.3.5
# 4.2.0 (12 February 2018)  - rscripts.R changed to version 0.2.3
# 4.2.1 (13 February 2018)  - candela.R added (Version 0.0.1)
# 4.2.2 (13 February 2018)  - visgen.R modified to (Version 1.2.7)
# 4.2.3 (14 February 2018)  - plotly.R modified to (Version 1.2.6)
# 4.2.4 (14 February 2018)  - highcharter.R modified to (Version 1.2.2)
# 4.2.7 (23 February 2018)  - DT.R modified to (Version 1.1.4)
# 4.2.9 (23 February 2018)  - jscripts.R modified to (Version 0.0.3)
# 4.3.2 (28 February 2018)  - TFD3.R modified to (Version 0.0.8) & DT.R to version (1.1.6)
# 4.3.3 (05 March 2018)     - c3.R modified to (Version 0.0.2)
# 4.3.4 (05 March 2018)     - billboarder.R added (Version 0.0.1)
# 4.4.4 (28 April 2018)     - grviz.R added and developed to version 0.1.0
# 4.4.7 (23 May 2018)       - bpexploder.R added and developed to version 0.0.1
# 4.4.8 (29 May 2018)       - dimple.R updated to version 1.0.2
# 4.4.0 (29 May 2018)       - visgen.R updated to version 1.2.9
# 4.4.1 (01 June 2018)      - dialogs.R initiated and updated to version 0.1.1
# 4.4.8 (04 June 2018)      - dashboard.R changed to version 2.4.2
# 4.7.0 (05 June 2018)      - highcharter.R changed to version 1.2.4
# 4.7.1 (05 June 2018)      - TFD3.R changed to version 0.0.9
# 4.7.9 (16 June 2018)      - dashboard.R changed to version 2.5.0
# 4.8.0 (19 June 2018)      - visgen.R changed to version 1.3.0
# 4.9.0 (19 June 2018)      - echarts.R added and developed to version 0.1.0
# 4.9.1 (19 June 2018)      - highcharter.R changed to version 1.2.5
# 4.9.5 (20 June 2018)      - googleVis.R changed to version 1.3.4
# 4.9.6 (20 June 2018)      - amCharts.R changed to version 1.1.4
# 5.0.0 (21 June 2018)      - bubbleCloud.R added and developed to version 0.1.0
# 5.0.1 (22 June 2018)      - dashboard.R changed to version 2.5.1
# 5.0.7 (25 June 2018)      - billboarder.R changed to version 0.0.7
# 5.0.9 (25 June 2018)      - plotly.R changed to version 1.3.1
# 5.1.1 (27 June 2018)      - visNetwork changed to version 1.1.5
# 5.1.2 (30 June 2018)      - calheatmap.R added (version 0.0.1)
# 5.1.5 (30 June 2018)      - d3plus.R changed to version 1.1.5
# 5.1.7 (30 June 2018)      - plotly.R changed to version 1.3.3
# 5.2.1 (30 June 2018)      - amCharts.R changed to version 1.1.8
# 5.2.2 (01 July 2018)      - d3plus.R changed to version 1.1.5 +1
# 5.2.3 (02 July 2018)      - plotly.R changed to version 1.3.4 +1
# 5.2.4 (18 July 2018)      - billboarder.R changed to version 0.0.8
# 5.2.7 (19 July 2018)      - dygraphs.R changed to version 1.2.4, c3.R changed to version 0.0.5, candela.R changed to ver 0.0.2
# 5.3.2 (20 July 2018)      - highcharter.R changed to version 1.3.0
# 5.3.7 (23 July 2018)      - nvd3.R changed to version 1.0.5, visgen.R changed to version 1.3.3
# 5.4.1 (24 July 2018)      - amCharts.R changed to version 1.1.9, billboarder.R changed to version 0.1.0, c3.R changed to version 0.0.6
# 5.5.2 (24 July 2018)      - dimple.R changed to ver 1.0.4, highcharter.R changed to version 1.3.9
# 5.5.3 (25 July 2018)      - d3plus.R changed to version 1.1.6
# 5.5.4 (27 July 2018)      - dimple.R changed to ver 1.0.5
# 5.5.5 (28 July 2018)      - dc.R added (ver 0.0.1)
# 5.5.6 (11 September 2018) - sankeyTree.R added (ver 0.0.1)
# 5.5.8 (17 September 2018) - billboarder.R changed to ver 0.1.1, c3.R changed to version 0.1.0
# 5.5.9 (18 September 2018) - dashboard.R changed to ver 2.5.2
# 5.6.0 (19 September 2018) - dashtools.R changed to ver 0.0.5
# 5.6.1 (25 September 2018) - streamgraph.R added (ver 0.0.1)
# 5.6.2 (16 October 2018)   - plotly.R changed to ver 1.3.5
# 5.6.3 (18 October 2018)   - plotly.R changed to ver 1.3.6
# 5.6.4 (18 October 2018)   - grviz.R changed to ver 0.1.1
# 5.6.5 (18 October 2018)   - visgen.R changed to ver 1.3.4
# 5.6.7 (29 October 2018)   - dashboard.R changed to ver 2.5.4
# 5.7.2 (05 November 2018)  - grviz.R changed to ver 0.1.7
# 5.7.3 (05 November 2018)  - dashboard.R changed to ver 2.5.5
# 5.7.4 (06 November 2018)  - grviz.R changed to ver 0.1.7
# 5.7.4 (06 November 2018)  - dashboard.R changed to ver 2.5.6
# 5.7.6 (24 February 2019)  - visgen.R changed to ver 1.3.6
# 5.7.7 (25 February 2019)  - networkd3.R changed to ver 1.1.3
# 5.7.8 (21 March 2019)     - visgen.R changed to ver 1.3.7
# 5.8.0 (21 June 2019)      - grviz.R changed to ver 0.1.8, dashboard.R changed to ver 2.5.7
# 5.8.1 (27 June 2019)      - plotly.R changed to ver 1.3.7
# 5.8.3 (30 July 2021)      - revisit to update
# 5.8.4 (04 Spetember 2021) - sankeytree changed to version 0.0.2
# 5.8.6 (13 October 2021)   - imageOutput added to dashboard
# 5.8.7 (03 January 2023)   - visgen.R updated to version 1.3.8
# 5.8.9 (05 January 2023)   - dashboard.R updated to version 2.5.9
# 5.9.2 (02 February 2023)  - d3tablefilter.R added. (We had an embedded htmlwidget for  the javascript package but we found a similar existing htmlwidget in the github and decided to embed that as well)
# 5.9.3 (03 January 2025)   - visgen.R updated to version 1.3.9
# 5.9.10 (23 November 2026) - Documentation: roxygen documentation of rvisPlot(), README, pkgdown site and vignettes
#                             (getting started, chart types, configuration, amCharts) added. grviz.R fixed to render again.
# 5.10.0 (23 November 2026) - Code review release (bug fixes, clean-up and speed-ups; working charts produce identical output):
#                             * Package: Imports and Remotes added to DESCRIPTION (rutils, magrittr, htmlwidgets, crosstalk, dplyr, stringr);
#                               visgen.R is collated first and is the single definition of 'defset' (was duplicated in amcharts.R);
#                               rvisPlot() dispatches without eval(parse()) and reports unknown wrappers clearly.
#                             * Speed: verifyConfig() parses and caches the properties table once per plotter instead of loading
#                               data('properties') on every plot; gndcd() vectorised.
#                             * Bug fixes in wrappers that failed on every call: amCharts.tsline (shape lookup overwritten by
#                               amCharts.bullet()), c3.bar, nvd3.scatter, googleVis.line/area options, candela.bar/scatter,
#                               bpexploder.box, rCharts.scatter, highcharts.scatter.molten, highcharter.tsline.stock,
#                               echarts (non-exported echarts4r helpers, list data), D3plusR wrappers (d3plus() masked by
#                               package d3plus), d3plus.bubble/scatter, networkD3.sankey without labels, streamgraph without
#                               config or with a single series, df2Network and other functions calling dplyr verbs unqualified.
#                             * Other bug fixes: highcharter drilldown with numeric x, visNetwork node size scaling, ggvis point
#                               shape, billboarder 'bottom' legend, TFD3/D3TableFilter radio buttons, dashboard box footer,
#                               navbarPage fluid/icon, pivotOutput rendering, header title CSS for non-blue skins, skin hover font,
#                               verifyConfigDimProperties() processing only the first dimension, bubbleforce widget name.
#                             * Clean-up: duplicated code merged (TFD3.prepare() shared by TFD3 and D3TableFilter tables,
#                               DT.addLinks(), dygraphs series helper, grviz helpers), dead commented code and an unused debug
#                               object removed, comments and error messages made clearer. Unused man/hello.Rd removed.
#                               See the version history of each file for details.
# 5.10.1 (23 November 2026) - Charts that were created without error but could not be displayed (found by evaluating every
#                             example of the chart types vignette and checking each chart in a browser):
#                             * TFD3 tables: widget was created without package = 'rvis' so its javascript never loaded; it also
#                               needed d3 version 3, broke single-column tables, and threw an error outside shiny that stopped
#                               other widgets of the page from rendering. Widget assets fixed (missing font and image files,
#                               invalid yaml entries). TFD3.table.molten() now draws a table (it was unfinished).
#                             * coffeewheel: its d3 dependency pointed to a missing file; d3 3.5.15 bundled in inst/lib/d3v3.
#                             * d3plus.scatter/bubble/bubble.molten/tree ported from the old GitHub 'd3plus' API to D3plusR.
#                             * echarts word clouds: plugin dependency and size range added.
#                             * knitr/R Markdown printing added for rCharts and googleVis objects (knit_print methods).
#                             * Vignette chart-types: every example is evaluated when its plotting package is installed.
# 5.10.2 (23 November 2026) - Security: bundled jQuery (inst/lib/jquery, used by TFD3 tables) updated from 1.12.4 to 3.7.1,
#                             fixing CVE-2015-9251 and CVE-2019-11358 (GitHub Dependabot alerts #2 and #5).
# 5.11.0 (23 November 2026) - Documentation: six chart guides with rendered examples of every chart type and plotter, each on
#                             its own R dataset: comparing categories (VADeaths), parts of a whole (HairEyeColor), relationships
#                             and distributions (iris), time series (airquality), networks and hierarchies (Titanic) and tables
#                             (USArrests). Vignette 'adding-plotters' lists the plotters covered, the htmlwidgets packages planned
#                             and how to write a wrapper. Vignette 'chart-types' is now a reference linking to the guides (its
#                             examples moved there), vignette 'amcharts' merged into the guides. Bug fixes found by the guides:
#                             * xCharts: formula method of rCharts::xPlot() called directly (it is not registered); rCharts charts
#                               embed their libraries from package rCharts in knitr (the xCharts CDN no longer exists).
#                             * c3: column names with dots (c3 renames them) broke combo and scatter.molten charts.
#                             * highcharter: bubble charts with numeric x; stock chart (tsline.stock) drew no data.
#                             * pivot: tables are drawn by package rpivotTable (the bundled widget had no javascript libraries).
#                             * TFD3: filter row missing in self-contained pages; tables overflowed their widget.
#                             * echarts: config properties size.min/size.max recorded as numbers.
#                             * d3plus: helper d3plus.bubbles() renamed d3plusBubbles() (it was dispatchable as a chart type).
#                             See the version history of each file for details.
# 5.12.0 (23 November 2026) - Dashboards (dashboard.R 2.7.0, new dashconfig.R 0.1.0):
#                             * DASHBOARD code review: the user interface is built by calling shiny functions directly
#                               instead of parsing generated R code (about 2.5 times faster), server code is parsed once
#                               per app instead of once per session; same user interface and services (checked on 15
#                               test dashboards). Many bugs fixed (tabPanel cloth, several king items, tooltips, tutor
#                               lessons and shinyjs methods that had no javascript on the page, observers of synced
#                               tables added again in each session, ...).
#                             * New item types: conditionalPanel, navlistPanel, sidebarMenu, menuItem, menuSubItem,
#                               tabItems, tabItem; new settings for date, slider, button, plot and text items; themes.
#                             * Dashboards from YAML or JSON files or R lists: rvisDashboard(), runDashboard(),
#                               rvisDashboardExample() with 38 example dashboards (inst/dashboards): 16 basic features,
#                               20 apps of the Shiny gallery rebuilt from YAML, and two large showcase dashboards.
#                             * Five dashboard vignettes: getting started, layouts, item reference, gallery, Shiny packages.
#                             See the version history of each file for details.
# 5.13.0 (24 November 2026) - Dashboard items and clothes of type 'package::function' (like shinyWidgets::pickerInput or
#                             shinydashboard::box) call that function directly with the item fields as arguments; inputs,
#                             outputs and containers are detected from the function arguments, outputs are rendered by the
#                             package's renderFoo function and inputs updated by its updateFoo function (dashboard.R).
# 5.14.0 (24 November 2026) - New plotter apexcharter (apexcharter.R, version 0.1.0): bar, line, area, scatter, bubble, pie,
#                             tsline, tsarea, tsbar, heatmap and radar. echarts.R changed to version 0.2.0: line, area, tsline,
#                             tsarea, tsbar, scatter, bubble, pie, box, heatmap, calendar, gauge, sankey, treemap, sunburst and
#                             graph added; standard config properties (title, legend, tooltip, axis labels, stacking, zoom).
#                             Examples in the chart guides; both plotters work as generic dashboard outputs.
# 5.15.0 (24 November 2026) - Clicks on chart items as shiny inputs: new function rvisClickEvents() (clickevents.R, version 0.1.0)
#                             adds javascript to an htmlwidget with htmlwidgets::onRender(), without copying the widget's own
#                             javascript. Supported: Graphviz (DiagrammeR, rvis grviz), networkD3, echarts4r, apexcharter, plotly,
#                             highcharter, and any widget with a CSS selector. Input <outputId>_item_click. Dashboard outputs
#                             with field click.events: true; example dashboard 'click-events'.
#                             grviz.R changed to version 0.2.0: grviz uses the grViz widget of DiagrammeR (the copied widget
#                             javascript and libraries, 2.1 MB, removed from inst).
#                             Dashboard items of generic type get the layout fields weight and offset only if their function has
#                             such an argument.
NULL
#> NULL
