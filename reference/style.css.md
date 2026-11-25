# Inline CSS style

Converts named arguments into an inline CSS style text, for the `style`
argument of html tags or of dashboard items.

## Usage

``` r
style.css(...)
```

## Arguments

- ...:

  Named CSS properties and their values.

## Value

A character string like `"color: red; width: 10px"`.

## Examples

``` r
style.css(color = "red", width = "10px")
#> [1] "color: red; width: 10px"
shiny::div(style = style.css(`background-color` = "#eee", padding = "8px"), "Some text")
#> <div style="background-color: #eee; padding: 8px">Some text</div>
```
