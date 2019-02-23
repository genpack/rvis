# viser.R ----------------------------------------------------------------

#' viser: This package is a tool-box for visualization in R and making word-class and elegant shiny dashboards.
#'
#' @section Class DASHBOARD:
#' viser provides a Reference class named as DASHBOARD.
#' You can design your interactive dashboard layout and assign multiple R objects into your dashboard.
#' It supports various visualization engines like graphics, ggplot2, ggvis, googlevis, amCharts, rCharts, dygraphs and many other plotter engines.
#'
#' @docType package
#' @name viser
#'
#' @import gener
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
#' @include dialogs.R
#' @include dt.R
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

#' @include viserplot.R

# Version History:
# 1.1.0 (04 July 2016)      - dashboard.R changed to version 1.6
# 1.2.0 (20 July 2016)      - nira_plotters.R added (Version 1.1.0)
# 1.2.1 (08 September 2016) - dashboard.R changed to version 1.6.2
# 1.2.3 (07 September 2016) - nira_plotters.R changed to version 1.1.3
# 1.2.7 (08 September 2016) - tools.R renamed to dashtools.R and changed to version 1.7
# 1.3.0 (29 September 2016) - deviset.R transferred from package timser
# 1.3.1 (29 September 2016) - visgen.R transferred from package niragen
# 1.3.2 (29 September 2016) - nira_plotters.R changed to version 1.1.4
# 1.4.0 (06 October 2016)   - nira_plotters.R changed to version 1.2.1 and renamed to: viserPlotter.R
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
# 3.1.0 (13 July 2017)      - viserplot.R added and replaced viserPlotter
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
NULL
#> NULL

