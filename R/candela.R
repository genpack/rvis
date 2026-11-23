# candela.R ----------------------------------------------------------------


# Header
# Filename:       candela.R
# Description:    Contains functions for plotting various charts from candela package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     13 February 2018
# Last Revision:  23 November 2026
# Version:        0.0.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     13 February 2018    Initial issue
# 0.0.2     19 July 2018        bar.molten renamed to bar
# 0.0.3     23 November 2026    Bug fixes: candela.bar() removed the colour dimension using an unquoted name (always failed),
#                               candela.scatter() failed when 'shape' was NULL and passed no data to candela.bar() for shape 'bar'.

#' @include visgen.R

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

# candela variable type associated with each column class
candela.type = c(numeric = 'quantitative', integer = 'quantitative', character = 'nominal', factor = 'nominal')

# Returns the candela type of column 'col' of table 'obj' (NULL if col is NULL)
candela.coltype = function(obj, col){
  if (is.null(col)){return(NULL)}
  candela.type[class(obj[, col])[1]] %>% unname
}

# color is a grouping dimension, so it is a molten chart:
candela.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package candela is not installed!")
  config = candela.bar.defset %<==>% (config %>% verify('list', default = list()))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config = config)

  candela('BarChart', data = obj, x = L$x, y = L$y, color = L$color,
          xType = candela.coltype(obj, L$x),
          yType = candela.coltype(obj, L$y),
          colorType = candela.coltype(obj, L$color),
          aggregate = config$aggregator.function.string %>% verify('character', domain = c('value', 'mean', 'sum'), default = 'value')
  )
}

candela.scatter = function(obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, config = NULL, ...){
  if (identical(shape, 'bar')){return(candela.bar(obj, x = x, y = y, color = color, config = config))}

  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package 'candela' is not installed!")
  config = candela.scatter.defset %<==>% (config %>% verify('list', default = list()))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  candela('ScatterPlot', data = obj, x = L$x, y = L$y, color = L$color, size = L$size, shape = L$shape,
          xType = candela.coltype(obj, L$x),
          yType = candela.coltype(obj, L$y),
          sizeType = candela.coltype(obj, L$size),
          shapeType = candela.coltype(obj, L$shape),
          colorType = candela.coltype(obj, L$color))
}
