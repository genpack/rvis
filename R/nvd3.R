# nvd3.R ----------------------------------------------------------------

# Header
# Filename:       nvd3.R
# Description:    Contains functions for plotting various nvd3 charts from rCharts package using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     12 May 2017
# Last Revision:  23 July 2018
# Version:        1.0.5
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R
# 1.0.2     12 May 2017         Applies color$config, added horizontal plot for nvd3.bar.molten
# 1.0.3     12 May 2017         Added nvd3.pie.molten()
# 1.0.4     23 July 2018        nvd3.scatter.molten() embedded in new added function added nvd3.scatter()
# 1.0.5     23 July 2018        nvd3.bar.molten() embedded in new added function added nvd3.bar()

nvd3.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric','integer', 'character', 'factor'),
    group = 'factor'),
  multiples  = c('x','y', 'group'),
  essentials = c('x', 'y'),
  palette = list(color = NULL)
)

nvd3.pie.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    group = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = 'group',
  palette = list(color = NULL)
)

nvd3.pie.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c('numeric', 'integer'),
    label = c('factor', 'character')),
  multiples  = c(),
  essentials = c('theta', 'label'),
  palette = list(color = NULL)
)

nvd3.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric', 'integer'),
    y     = 'numeric',
    shape = 'character',
    group = 'factor'),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

nvd3.type = function(shape, config){
  shape %<>% verify('character', lengths = 1, domain = c('line', 'point', 'bubble', 'circle', 'bar'), default = 'point', varname = 'shape', err_src = 'nvd2.type')
  config$zoomWindow %<>% verify('logical', lengths = 1, domain = c(T, F), default = F, varname = 'config$zoomWindow', err_src = 'nvd2.type')
  if(shape == 'line'){
    if(config$zoomWindow){return('lineWithFocusChart')} else {return('lineChart')}
  } else if (shape == 'bar'){
    return('multiBarChart')
  }
  else {
    return('scatterChart')
  }
}

nvd3.applyConfig = function(np, config){
  np$xAxis(axisLabel = config$xAxis.label)
  np$yAxis(axisLabel = config$yAxis.label)
  if(!is.null(config$palette$color)){np$chart(color = config$palette$color)}
  return(np)
}

nvd3.applyColors = function(np, clrvect){
  np$chart(color = clrvect)
  return(np)
}

nvd3.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')

  obj %<>% prepare4Plot(A, config = config)

  hor = isHorizontal(obj, L$x, L$y)
  Ly = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)

  if(!is.null(L$group)){
    clrvect = getColorVect(Ly = obj[, L$group] %>% as.character %>% unique, Lcolor = L$color, config = config)

    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    nv = nPlot(data = obj, x = Lx, y = Ly, group = L$group, type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
  } else {
    if(length(Ly) > 1){

      nv = nPlot(data = obj %>% reshape2::melt(id.vars = Lx, measure.vars = Ly),
                 x = Lx, y = 'value', group = 'variable', type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
    } else {
      nv = nPlot(data = obj, x = Lx, y = Ly, type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
    }
    clrvect = getColorVect(Ly, L$color, config)
  }
  nv %>% nvd3.applyConfig(config) %>% nvd3.applyColors(clrvect)
}

nvd3.pie.molten = function(obj, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.pie.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  scr = paste("nPlot(~", L$group, ", data = obj, type = 'pieChart')")
  np = parse(text = scr) %>% eval
  np$chart(donut = config$donut %>% verify('logical', lengths = 1, domain = c(T,F), default = F, varname = 'config$donut'))
  return(np %>% nvd3.applyConfig(config))
}

nvd3.scatter = function(obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  obj %<>% prepare4Plot(A, config = config)

  if(!is.null(L$group)){
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    nv = nPlot(data = obj, x = L$x, y = L$y, group = L$group, type = nvd3.type(shape, config))
  } else {
    if(length(L$y) > 1){
      nv = nPlot(data = obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y),
                 x = L$x, y = 'value', group = 'variable', type = nvd3.type(shape, config))
    } else {
      nv = nPlot(data = obj, x = L$x, y = L$y, type = nvd3.type(shape, config))
    }
  }
  nv %>% nvd3.applyConfig(config) %>% nvd3.applyColors(clrvect)
}

nvd3.pie = function(obj, theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config = config)

  scr = paste("nPlot(", L$theta,"~", L$label, ", data = obj, type = 'pieChart')")
  np = parse(text = scr) %>% eval
  np$chart(donut = config$donut %>% verify('logical', lengths = 1, domain = c(T,F), default = F, varname = 'config$donut'))
  return(np %>% nvd3.applyConfig(config))

}

