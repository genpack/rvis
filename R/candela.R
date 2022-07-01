# candela.R ----------------------------------------------------------------


# Header
# Filename:       candela.R
# Description:    Contains functions for plotting various dimple charts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     13 February 2018
# Last Revision:  19 July 2018
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     13 February 2018    Initial issue
# 0.0.2     19 July 2018        bar.molten renamed to bar

candela.bar.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor'),
    y     = c('numeric','integer'),
    color = c('numeric','integer', 'character', 'factor')),
  multiples = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

candela.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric','integer'),
    y     = c('numeric','integer'),
    color = c('numeric','integer', 'character', 'factor'),
    shape = c('character', 'factor'),
    size  = c('numeric','integer')),
  essentials = c('x', 'y'),
  colorize   = F
)

candela.type = c(numeric = 'quantitative', integer = 'quantitative', character = 'nominal', factor = 'nominal')

# color is a grouping dimension, so it is a molten chart:
candela.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package candela is not installed!")
  config = candela.bar.defset %<==>% (config %>% verify('list', default = list()))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color) #, extend = 'y')
  L = a$labels
  A = a$aesthetics %>% list.remove(color)

  obj %<>% prepare4Plot(A, config = config)

  # todo: check for other possible values for this argument

  candela('BarChart', data = obj, x = L$x, y = L$y, color = L$color,
          xType = candela.type[class(obj[, L$x])[1]] %>% unname,
          yType = candela.type[class(obj[, L$y])[1]] %>% unname,
          colorType = candela.type[class(obj[, L$color])[1]] %>% unname,
          aggregate = config$aggregator.function.string %>% verify('character', domain = c('value', 'mean', 'sum'), default = 'value')
  )
  # Todos:
  # How to change xLabel and yLabel?
  # add other charts: line, ...

}

candela.scatter = function(obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, config = NULL, ...){
  if (shape == 'bar'){return(candela.bar(x = x, y = y, color = color, config = config))}

  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package 'candela' is not installed!")
  config = candela.scatter.defset %<==>% (config %>% verify('list', default = list()))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  # todo: check for other possible values for this argument
  # todo: add extra columns to the object

  candela('ScatterPlot', data = obj, x = L$x, y = L$y, color = L$color, size = L$size, shape = L$shape,
          xType = candela.type[class(obj[, L$x])[1]] %>% unname,
          yType = candela.type[class(obj[, L$y])[1]] %>% unname,
          sizeType = candela.type[class(obj[, L$size])[1]] %>% unname,
          shapeType = candela.type[class(obj[, L$shape])[1]] %>% unname,
          colorType = candela.type[class(obj[, L$color])[1]] %>% unname)

}




