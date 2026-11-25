# Example dashboards

Lists the example dashboard configurations that come with rvis, or
returns the path of one of them.

## Usage

``` r
rvisDashboardExample(name = NULL)
```

## Arguments

- name:

  Name of an example (file name without extension). If `NULL`, the
  examples are listed.

## Value

A data frame of the examples (name, group, title and file) if `name` is
`NULL`, otherwise the path of the example file.

## Examples

``` r
rvisDashboardExample()
#>                 name    group                                           title
#> 1             charts   basics                                     rvis charts
#> 2       click-events   basics                                    Click events
#> 3            clothes   basics                                         Clothes
#> 4            dynamic   basics                               Dynamic interface
#> 5      generic-types   basics                              Generic item types
#> 6              hello   basics                                      Hello rvis
#> 7             inputs   basics                                     Input items
#> 8        layout-grid   basics                                     Layout grid
#> 9              login   basics                                           Login
#> 10   modals-tooltips   basics                             Modals and tooltips
#> 11            navbar   basics                                     Navbar page
#> 12           outputs   basics                                    Output items
#> 13          reactive   basics                                 Reactive values
#> 14    shinydashboard   basics                                     Earthquakes
#> 15      shinywidgets   basics                                    shinyWidgets
#> 16      synced-table   basics                                    Synced table
#> 17              tabs   basics                                            Tabs
#> 18             tutor   basics                                     Guided tour
#> 19   basic-datatable  gallery                                 Basic DataTable
#> 20 conditional-panel  gallery                              Conditional panels
#> 21             dates  gallery                           Dates and date ranges
#> 22          diamonds  gallery                               Diamonds Explorer
#> 23          download  gallery                                Downloading Data
#> 24        dynamic-ui  gallery Dynamically generated user interface components
#> 25          faithful  gallery                                        Faithful
#> 26           isolate  gallery                                 isolate example
#> 27            kmeans  gallery                         Iris k-means clustering
#> 28      modal-dialog  gallery                                    Modal dialog
#> 29      more-widgets  gallery                                    More Widgets
#> 30    navbar-example  gallery                                         Navbar!
#> 31  plot-interaction  gallery                        Plot interaction - basic
#> 32          progress  gallery                            Progress bar example
#> 33        reactivity  gallery                                      Reactivity
#> 34       shinythemes  gallery                                     shinythemes
#> 35           sliders  gallery                                         Sliders
#> 36           tabsets  gallery                                         Tabsets
#> 37        telephones  gallery                            Telephones by region
#> 38             timer  gallery                                           Timer
#> 39     data-explorer showcase                                   Data explorer
#> 40         us-states showcase                              US states explorer
#>                                                                             file
#> 1              /home/runner/work/_temp/Library/rvis/dashboards/basics/charts.yml
#> 2        /home/runner/work/_temp/Library/rvis/dashboards/basics/click-events.yml
#> 3             /home/runner/work/_temp/Library/rvis/dashboards/basics/clothes.yml
#> 4             /home/runner/work/_temp/Library/rvis/dashboards/basics/dynamic.yml
#> 5       /home/runner/work/_temp/Library/rvis/dashboards/basics/generic-types.yml
#> 6               /home/runner/work/_temp/Library/rvis/dashboards/basics/hello.yml
#> 7              /home/runner/work/_temp/Library/rvis/dashboards/basics/inputs.yml
#> 8         /home/runner/work/_temp/Library/rvis/dashboards/basics/layout-grid.yml
#> 9               /home/runner/work/_temp/Library/rvis/dashboards/basics/login.yml
#> 10    /home/runner/work/_temp/Library/rvis/dashboards/basics/modals-tooltips.yml
#> 11             /home/runner/work/_temp/Library/rvis/dashboards/basics/navbar.yml
#> 12            /home/runner/work/_temp/Library/rvis/dashboards/basics/outputs.yml
#> 13           /home/runner/work/_temp/Library/rvis/dashboards/basics/reactive.yml
#> 14     /home/runner/work/_temp/Library/rvis/dashboards/basics/shinydashboard.yml
#> 15       /home/runner/work/_temp/Library/rvis/dashboards/basics/shinywidgets.yml
#> 16       /home/runner/work/_temp/Library/rvis/dashboards/basics/synced-table.yml
#> 17               /home/runner/work/_temp/Library/rvis/dashboards/basics/tabs.yml
#> 18              /home/runner/work/_temp/Library/rvis/dashboards/basics/tutor.yml
#> 19   /home/runner/work/_temp/Library/rvis/dashboards/gallery/basic-datatable.yml
#> 20 /home/runner/work/_temp/Library/rvis/dashboards/gallery/conditional-panel.yml
#> 21             /home/runner/work/_temp/Library/rvis/dashboards/gallery/dates.yml
#> 22          /home/runner/work/_temp/Library/rvis/dashboards/gallery/diamonds.yml
#> 23          /home/runner/work/_temp/Library/rvis/dashboards/gallery/download.yml
#> 24        /home/runner/work/_temp/Library/rvis/dashboards/gallery/dynamic-ui.yml
#> 25          /home/runner/work/_temp/Library/rvis/dashboards/gallery/faithful.yml
#> 26           /home/runner/work/_temp/Library/rvis/dashboards/gallery/isolate.yml
#> 27            /home/runner/work/_temp/Library/rvis/dashboards/gallery/kmeans.yml
#> 28      /home/runner/work/_temp/Library/rvis/dashboards/gallery/modal-dialog.yml
#> 29      /home/runner/work/_temp/Library/rvis/dashboards/gallery/more-widgets.yml
#> 30    /home/runner/work/_temp/Library/rvis/dashboards/gallery/navbar-example.yml
#> 31  /home/runner/work/_temp/Library/rvis/dashboards/gallery/plot-interaction.yml
#> 32          /home/runner/work/_temp/Library/rvis/dashboards/gallery/progress.yml
#> 33        /home/runner/work/_temp/Library/rvis/dashboards/gallery/reactivity.yml
#> 34       /home/runner/work/_temp/Library/rvis/dashboards/gallery/shinythemes.yml
#> 35           /home/runner/work/_temp/Library/rvis/dashboards/gallery/sliders.yml
#> 36           /home/runner/work/_temp/Library/rvis/dashboards/gallery/tabsets.yml
#> 37        /home/runner/work/_temp/Library/rvis/dashboards/gallery/telephones.yml
#> 38             /home/runner/work/_temp/Library/rvis/dashboards/gallery/timer.yml
#> 39    /home/runner/work/_temp/Library/rvis/dashboards/showcase/data-explorer.yml
#> 40        /home/runner/work/_temp/Library/rvis/dashboards/showcase/us-states.yml
if (FALSE) { # \dontrun{
runDashboard(rvisDashboardExample("faithful"))
} # }
```
