
# dimple.R ----------------------------------------------------------------


# Header
# Filename:       dimple.R
# Description:    Contains functions for plotting various dimple charts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     12 May 2017
# Last Revision:  27 July 2017
# Version:        1.0.5
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R
# 1.0.1     29 May 2018         Function dimple.bar.molten() added
# 1.0.2     29 May 2018         config property 'barMode' applied.
# 1.0.3     24 July 2018        Function dimple.applyConfig() added.
# 1.0.4     24 July 2018        Function dimple.combo() added.
# 1.0.5     27 July 2018        Function dimple.scatter() added.

dimple.combo.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c("character", "factor", "Date", "POSIXct", "numeric", "integer"),
    y     = c("numeric", "integer", "character", "factor", "Date", "POSIXct"),
    t     = 'factor',
    group = 'factor',
    size  = 'numeric'),
  multiples  = c('x', 'y', 'size', 'group'),
  essentials = c('x', 'y'),
  legend.enabled = F,
  colorize = F
)

dimple.scatter.defset = defset %<==>% list(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = "numeric",
    y     = "numeric",
    color = 'factor',
    t     = 'factor',
    size  = 'numeric'),
  multiples  = c('y', 'size', 'shape', 'color'),
  essentials = c('x', 'y')
)

dimple.applyConfig = function(rc, config){
  if(config$legend.enabled %>% verify('logical', default = T)){
    rc$legend(x = chif(is.null(config$legend.position.x), 0, config$legend.position.x),
              y = chif(is.null(config$legend.position.y), 0, config$legend.position.y),
              width = chif(is.null(config$legend.width), 100, config$legend.width),
              height = chif(is.null(config$legend.height), 20, config$legend.height),
              horizontalAlign = config$legend.horizontalAlign %>% verify('character', domain = c('right', 'left'), default = 'right'))
  }
  return(rc)
}


dimple.combo = function(obj, x = NULL, y = NULL, t = NULL, size = NULL, group = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}

  if(is.null(shape)){shape = 'line'}

  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = dimple.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, t = t, size = size, group = group, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'color')

  if ('rownames' %in% c(L$group, L$x, L$y)){obj %<>% rownames2Column('rownames') %>% as.data.frame}

  obj %<>% prepare4Plot(A, config = config)

  xnum = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% is.numeric}) %>% sum
  xdte = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('Date')}) %>% sum
  xtim = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('POSIXct')}) %>% sum
  ynum = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% is.numeric}) %>% sum
  ydte = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('Date')}) %>% sum
  ytim = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('POSIXct')}) %>% sum

  if(xdte | xtim){for (col in L$x){obj[, col] %<>% as.character}}
  if(ydte | ytim){for (col in L$y){obj[, col] %<>% as.character}}

  ycat = !ynum & !ydte & !ytim
  hor  = xnum & ycat
  Ly   = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x);
  Lgroup  = L$group

  if(!is.null(L$group)){clrvect = getColorVect(obj[, L$group %>% last] %>% unique, L$color, config)}

  N = length(Ly)
  if(N > 1){
    obj %<>% reshape2::melt(id.vars = Lx, measure.vars = Ly)
    series = obj[, 'variable'] %>% unique
    L$shape %<>% vect.extend(length(series))
    shapes = L$shape %>% unique
    nshape = length(shapes)

    dp <- dPlot(x      = chif(hor, 'value', Lx),
                y      = chif(hor, Lx, 'value'),
                z      = L$size[1],
                data   = obj %>% filter(variable %in% series[which(L$shape == shapes[1])]),
                groups = 'variable',
                type   = shapes[1], ...)
    if(nshape > 1){
      for(i in 2:nshape){
        dp$layer(x      = chif(hor, 'value', Lx),
                 y      = chif(hor, Lx, 'value'),
                 z      = L$size[i %>% min(length(L$size))],
                 data   = obj %>% filter(variable %in% series[which(L$shape == shapes[i])]),
                 groups = 'variable',
                 type   = shapes[i])
      }
    }
    clrvect = getColorVect(obj[, 'variable'] %>% unique, L$color, config)
    Lgroup  = 'variable'
  } else {
    dp <- dPlot( x      = L$x,
                 y      = L$y,
                 z      = L$size,
                 data   = obj,
                 groups = L$group,
                 type   = L$shape, ...)
    if(is.null(L$group)){clrvect = getColorVect(L$y, L$color, config)}
  }

  if      (xnum){dp$xAxis(type = "addMeasureAxis")}
  else if (xdte){
    dp$xAxis(type = "addTimeAxis", inputFormat = "%Y-%m-%d", outputFormat = chif(is.null(config$xAxis.tick.label.format), "%Y-%m-%d", config$xAxis.tick.label.format))
  }
  else if (xtim){
    dp$xAxis(type = "addTimeAxis",
             inputFormat  = "%Y-%m-%d %H:%M:%S",
             outputFormat = chif(is.null(config$xAxis.tick.label.format), "%Y-%m-%d %H:%M:%S", config$xAxis.tick.label.format))
  }
  else {for(col in L$x){obj[, col] %<>% as.character}; dp$xAxis(type = "addCategoryAxis")}

  if      (ynum){dp$yAxis(type = "addMeasureAxis")}
  else if (ydte){
    dp$yAxis(type = "addTimeAxis", inputFormat = "%Y-%m-%d", outputFormat = chif(is.null(config$yAxis.tick.label.format), "%Y-%m-%d", config$yAxis.tick.label.format))
  }
  else if (ytim){
    dp$yAxis(type = "addTimeAxis",
             inputFormat  = "%Y-%m-%d %H:%M:%S",
             outputFormat = chif(is.null(config$yAxis.tick.label.format), "%Y-%m-%d %H:%M:%S", config$yAxis.tick.label.format))
  }
  else {for(col in L$y){obj[, col] %<>% as.character}; dp$yAxis(type = "addCategoryAxis")}

  if(!is.null(size)){
    # Felan hamash numerice
    if(inherits(obj[, L$size], c('numeric', 'integer'))){dp$zAxis(type = "addMeasureAxis", overrideMax = config$maxSizeOverride)} else {dp$zAxis(type = "addCategoryAxis")}
  }
  dp %<>% dimple.applyConfig(config)
  dp$setTemplate(afterScript = dimple.js(field_name = Lgroup))
  if(!is.null(L$t)){dp$set(storyboard = L$t)}
  if(!is.null(clrvect)){dp$defaultColors(clrvect)}
  dp
}

dimple.scatter = function(obj, x = NULL, y = NULL, t = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if(is.null(shape)){shape = 'bubble'}
  if(is.null(color)){color = 'Series 1'}

  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = dimple.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, t = t, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config = config);  if (is.empty(obj)){return(NULL)}

  obj$ID = obj %>% nrow %>% sequence
  clrvect = NULL


  N = length(L$y)
  if(N > 1){
    obj %<>% reshape2::melt(id.vars = c(L$x, L$size), measure.vars = L$y)
    obj$ID = obj %>% nrow %>% sequence

    dp = dPlot(
      x = L$x,
      y = 'value',
      z = L$size[1],
      groups = c('ID', 'variable'),
      data = obj,
      type = L$shape
    )
    Lgroup = 'variable'
  } else{
    dp = dPlot(
      x = L$x,
      y = L$y[1],
      z = L$size[1],
      groups = c('ID', L$color),
      data = obj,
      type = L$shape
    )
    Lgroup = L$color
  }

  dp$xAxis(type = "addMeasureAxis")
  dp$yAxis(type = "addMeasureAxis")
  dp %<>% dimple.applyConfig(config)
  dp$setTemplate(afterScript = dimple.js(field_name = Lgroup))
  if(!is.null(L$t)){dp$set(storyboard = L$t)}
  if(!is.null(clrvect)){dp$defaultColors(clrvect)}
  dp
}


show = function(rc)
{
  dir.create(temp_dir <- tempfile(pattern = "rCharts"))
  writeLines(rc$render(static = F), tf <- file.path(temp_dir, "index.html"))
  suppressMessages(copy_dir_(rc$LIB$url, file.path(temp_dir, rc$LIB$name)))
  visfun = options('viewer')[[1]]
  visfun(tf)
}

