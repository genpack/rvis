# highcharter.R ----------------------------------------------------------------

# Header
# Filename:       highcharter.R
# Description:    Contains functions for plotting various charts from package 'highcharter' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     14 October 2016
# Last Revision:  24 July 2018
# Version:        1.3.9
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     14 October 2016    Initial issue
# 1.1.0     26 March 2017      Function highcharter.combo() added
# 1.1.1     02 April 2017      highcharter.combo.defset Added
# 1.1.2     02 April 2017      Function highcharter.combo() Modified
# 1.2.0     14 May 2017        Function highcharter.combo() Modified: drilldown for multiple categorical series added
# 1.2.1     01 June 2017       Function highcharter.combo() Modified: fixed bug in distinguishing whether or not drilldown conditions are met.
# 1.2.2     14 February 2018   Function highcharter.apply() renamed to highcharter.applyConfig() and modified: the background color 'white' defined as default for hc_theme, if thm is empty, nothing will be plotted!
# 1.2.3     04 June 2018       Function highcharter.bar() added.
# 1.2.4     05 June 2018       Function highcharter.applyConfig() modified: only property 'bordRadius' cannot be set to NULL because it ruins the plot!! Package Bug!!
# 1.2.5     19 June 2018       Function highcharter.pie() added.
# 1.2.6     20 July 2018       Function highcharter.tsline() added.
# 1.2.7     20 July 2018       Function highcharter.heatmap() added.
# 1.2.8     20 July 2018       Function highcharter.addSeries.time() added
# 1.2.9     20 July 2018       Function highcharter.addSeries() modified: key column in the data is calculated correctly
# 1.3.0     20 July 2018       Function highcharter.combo() rewritten. addSeries removed
# 1.3.9     24 July 2018       Functions highcharter.line, area, bar, area.range, treemap, pie, pyramid, funnel, bubble, added.


# todo: add sunburst asap! take the example in tutorial


highcharter.line.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor', 'numeric', 'integer'),
    y       = c('numeric', 'integer', 'character', 'factor'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('x', 'y', 'color'),
  essentials = c('x', 'y')
)

highcharter.combo.defset = defset %>% list.edit(
  dimclass   = list(
    x         = c('character', 'factor', 'numeric', 'integer', 'Date', 'POSIXct'),
    y         = c('numeric', 'integer', 'character', 'factor', 'Date', 'POSIXct'),
    size      = c('numeric', 'integer'),
    shape     = c('character'),
    low       = c('numeric', 'integer'),
    high      = c('numeric', 'integer'),
    color     = c('character', 'factor', 'numeric', 'integer'),
    linkColor = c('character', 'factor', 'numeric', 'integer')),

  multiples  = c('x', 'y', 'low', 'high', 'size', 'color', 'linkColor', 'shape'),
  essentials = c('x', 'y')
)

highcharter.bubble.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor', 'numeric', 'integer'),
    y       = c('numeric', 'integer', 'character', 'factor'),
    size    = 'numeric',
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('x', 'y', 'color'),
  essentials = c('x', 'y')
)

highcharter.area.range.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor'),
    low     = c('numeric', 'integer'),
    high    = c('numeric', 'integer'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('low', 'high', 'color'),
  essentials = c('x', 'low', 'high')
)


highcharter.funnel.defset = defset %>% list.edit(
  dimclass   = list(
    label   = c('character', 'factor'),
    size    = c('numeric', 'integer'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c(),
  essentials = c('x', 'y')
)


highcharter.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c('x', 'y', 'size', 'color', 'shape'),
  essentials = c('x', 'y'),
  colorize   = F,
  yAxis.gridLine.width = 1

)

highcharter.scatter.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    group   = c('factor', 'character'),
    color   = valid.classes),
  multiples  = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

highcharter.shape = c(point = 'scatter', line.point = 'line', area.range = 'arearange',
                      line.smooth = 'spline', area.smooth = 'areaspline', area.range.smooth = 'areasplinerange',
                      bar.range = 'columnrange')

tree2RootSeries = function(tr, value.var = 'value', idsuffix = ''){
  tr2 = list()
  for (e in tr){
    e2 = list()
    if      (inherits(e, 'TREE')){e2 = list(name = e$tree_name, y = e[[value.var]], drilldown = tolower(e$tree_name) %++% idsuffix)}
    else if (inherits(e, 'list')){e2 = list(e$leaf_name, e[[value.var]])}
    if(!is.empty(e2)){tr2[[length(tr2) + 1]] = e2}
  }
  return(tr2)
}

tree2DrilldownSeries = function(tr, dd = list(), type = NULL, series_name = NULL, value.var = 'value', idsuffix = ''){
  for (e in tr){
    e2 = list()
    if      (inherits(e, 'TREE')){
      dd %<>% list.add(
        list(
          id   = tolower(e$tree_name) %++% idsuffix,
          type = type,
          name = chif(is.null(series_name), e$tree_name, series_name),
          data = tree2RootSeries(e, value.var = value.var, idsuffix = idsuffix)
        )
      )
      dd = tree2DrilldownSeries(e, dd, type = type, series_name = series_name, value.var = value.var, idsuffix = idsuffix)
    }
  }
  return(dd)
}

highcharter.applyConfig = function(h, config = list()){
  # verifications:
  verify(h, 'highchart', varname = 'h')
  verify(config, 'list', varname = 'config')


  if(!is.null(config$title)){h = hc_title(h, text = config$title)}
  if(!is.null(config$subtitle)){h = hc_subtitle(h, text = config$subtitle)}

  thm = highcharter::hc_theme(
    chart = list(
      # todo: define default for all of them
      backgroundColor = config$background.color %>% verify('character', default = 'white'),
      borderColor     = config$border.color,
      borderRadius    = config$border.radius,
      borderWidth     = config$border.width
    ),
    title = list(
      style = list(
        color = config$title.color,
        fontFamily = config$title.font
      )
    ),
    subtitle = list(
      style = list(
        color = config$subtitle.color,
        fontFamily = config$subtitle.font
      )
    )
  ) %>% list.clean

  config$legend %<>% verify('logical', domain = c(T,F), default = F, varname = 'config$legend')
  if(config$legend){
    thm$legend = list(
      itemStyle = list(
        fontFamily = config$legend.item.font,
        color      = config$legend.item.color
      ),
      itemHoverStyle = list(
        color = config$legend.item.hover.color,
        fontFamily = config$legend.item.hover.font
      )
    )
  }

  if(!is.null(config$yAxis.gridLine.width)){thm$yAxis$gridLineWidth = config$yAxis.gridLine.width}
  if(!is.null(config$tooltip)){h %<>% hc_tooltip(headerFormat = config$tooltip[1],
                                                 pointFormat  = config$tooltip[2])}

  if(!is.empty(thm)){h %<>% highcharter::hc_add_theme(thm)}
  h %<>%
    highcharter::hc_xAxis(title = list(text = config$xAxis.label)) %>%
    highcharter::hc_yAxis(title = list(text = config$yAxis.label)) %>%
    highcharter::hc_legend(enabled = !is.null(config$legend)) %>%
    highcharter::hc_chart(borderColor  = config$border.color,
                          borderWidth  = config$border.width)

  if(!is.null(config$border.radius)){h %<>% highcharter::hc_xAxis(borderRadius = config$border.radius)}

  config$barMode %<>% verify('character', lengths = 1, domain = c('group', 'stack', 'relative'), varname = config$barMode, default = 'group')
  if(config$barMode == 'stack'){h %<>% hc_plotOptions(column = list(stacking = "normal"), bar = list(stacking = "normal"))}
  return(h)
}

highcharter.combo = function(obj, x = NULL, y = NULL, size = NULL, low = NULL, high = NULL, color = NULL, linkColor = NULL, shape = NULL, config = NULL){
  # Preparing Aesthetics:
  config = highcharter.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'highcharter') %>% verifyConfigDimProperties(dims = 'color')

  if(is.null(shape)){shape = 'line'}

  a = prepareAesthetics(x = x, y = y, size = size, low = low, high = high, color = color, linkColor = linkColor, shape = shape, extend = c('shape', 'high', 'low'))

  L = a$labels
  A = a$aesthetics %>% list.remove('shape')

  w = which(L$shape %in% names(highcharter.shape));
  if(length(w) > 0){L$shape[w] <- highcharter.shape[L$shape[w]] %>% unname}

  obj %<>% prepare4Plot(A, config)

  xnum    = is.numeric(obj[, L$x[1]])
  ynum    = chif(is.null(y), T, is.numeric(obj[, L$y[1]]))
  bars    = 'bar' %in% L$shape | 'column' %in% L$shape
  swapxy  = xnum & !ynum & bars
  special = length(c('pie', 'funnel', 'pyramid', 'treemap') %>% intersect(L$shape)) > 0

  if(swapxy){keep = L$y; L$y = L$x; L$x = keep; xnum = F; ynum = T}
  h    = highcharter::highchart()
  drll = F
  if(!xnum){
    w = which(L$shape == 'bar' | L$shape == 'column')
    if(length(w) > 0){L$shape[w] = chif(swapxy, 'bar','column')}
    if(inherits(obj[, L$x[1]], c('character', 'factor'))){
      if(length(L$x) > 1){
        drll = T
        h %<>% highcharter::hc_xAxis(type = 'category')
      } else {
        obj[, L$x] %<>% as.character
        ccn = obj[, L$x] %>% unique
        ccn.indx = (ccn %>% length %>% sequence) - 1; names(ccn.indx) <- ccn
        h %<>% highcharter::hc_xAxis(type = 'category', categories = ccn)
        if(!special){obj[, L$x] <- ccn.indx[obj[, L$x]]}
      }
    } else
      if(inherits(obj[, L$x[1]], c('Date', 'POSIXct'))){
        assert(length(L$x) == 1)
        h %<>% highcharter::hc_xAxis(type = "datetime")
        obj[, L$x] <- datetime_to_timestamp(obj[, L$x])
      } else {stop('Ino dige man nemishnasam!!!!!')}
  } else {
    h %<>% highcharter::hc_xAxis(type = 'linear')
  }

  if(!ynum){
    w = which(L$shape == 'bar' | L$shape == 'column')
    if(length(w) > 0){L$shape[w] = chif(swapxy, 'column','bar')}
    if(inherits(obj[, L$y[1]], c('character', 'factor'))){
      if(length(L$y) > 1){
        drll = T
        h %<>% highcharter::hc_yAxis(type = 'category')
      } else {
        obj[, L$y] %<>% as.character
        ccn = obj[, L$y] %>% unique
        ccn.indx = (ccn %>% length %>% sequence) - 1; names(ccn.indx) <- ccn
        h %<>% highcharter::hc_yAxis(type = 'category', categories = ccn)
        if(!special){obj[, L$y] <- ccn.indx[obj[, L$y]]}
      }
    } else
      if(inherits(obj[, L$y[1]], 'Date')){
        assert(length(L$y) == 1)
        h %<>% highcharter::hc_yAxis(type = "datetime")
        obj[, L$y] <- datetime_to_timestamp(obj[, L$y])
      } else {stop('Ino dige man nemishnasam!!!!!')}
  } else {
    h %<>% highcharter::hc_yAxis(type = 'linear')
  }

  # todo: care for timedate class for x and y axis
  if(ynum & !xnum){
    dd   = list()
    if(!is.null(L$y)){
      sercount  = sequence(length(L$y))
      sernames  = L$y
    } else if (!is.null(L$size)){
      sercount = sequence(length(L$size))
      sernames  = L$size
    } else if (!is.null(L$low)){
      sercount = sequence(length(L$low))
      sernames  = L$low
    }
    seriescolor = NULL
    for (i in sercount){
      # Find series color:
      if(is.null(L$color[i])){
        if(!is.null(config$color)){
          seriescolor = config$color[[sernames[i]]]
        }
      } else {seriescolor = obj[1, L$color[i]]}
      if(!is.null(linkColor[i])){
        if(L$shape[i] == 'line'){L$shape[i] = 'coloredline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'coloredarea'}
      }
      if(config$link.smooth.enabled %>% verify('logical', lengths = 1, domain = c(T,F), default = F)){
        if(L$shape[i] == 'line'){L$shape[i] = 'spline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'areaspline'} else
            if(L$shape[i] == 'arearange'){L$shape[i] = 'areasplinerange'}
      }

      if(drll){
        tr = obj[, c(L$x, L$y[i])] %>% df2tree(L$x, L$y[i], func = config$aggregator %>% verify('character', lengths = 1, default = 'sum'))
        dt = tr %>% tree2RootSeries(value.var = L$y[i], idsuffix = i)
        dd %<>% c(tr %>% tree2DrilldownSeries(value.var = L$y[i], type = L$shape[i], series_name = L$y[i], idsuffix = i))
        h %<>% highcharter::hc_add_series(data = dt, name = L$y[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      } else {
        dt = obj %>% nameColumns(list(x = L$x, y = L$y[i], z = L$size[i], high = L$high[i], low = L$low[i], color = L$color[i], segmentColor = L$linkColor[i]))
        if(special){names(dt)[1] <- 'name'}
        if(L$shape[i] == 'treemap'){names(dt)[[which(names(dt) == 'z')]] <- 'value'}
        h %<>% highcharter::hc_add_series(data = dt, name = L$y[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      }
    }
  } else if (xnum & !ynum){
    for (i in sequence(length(L$x))){
      if(is.null(L$color[i])){
        if(!is.null(config$color)){
          seriescolor = config$color[[L$x[i]]]
        }
      } else {seriescolor = obj[1, L$color[i]]}
      if(!is.null(linkColor[i])){
        if(L$shape[i] == 'line'){L$shape[i] = 'coloredline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'coloredarea'}
      }
      if(config$link.smooth.enabled %>% verify('logical', lengths = 1, domain = c(T,F), default = F)){
        if(L$shape[i] == 'line'){L$shape[i] = 'spline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'areaspline'} else
            if(L$shape[i] == 'arearange'){L$shape[i] = 'areasplinerange'}
      }

      if(drll){
        tr = obj[, c(L$y, L$x[i])] %>% df2tree(L$y, L$x[i], func = config$aggregator %>% verify('character', lengths = 1, default = 'sum'))
        dt = tr %>% tree2RootSeries(value.var = L$x[i], idsuffix = i)
        dd %<>% c(tr %>% tree2DrilldownSeries(value.var = L$x[i], type = L$shape[i], series_name = L$x[i], idsuffix = i))
        h %<>% highcharter::hc_add_series(data = dt, name = L$x[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      } else {
        dt = obj %>% nameColumns(list(x = L$x[i], y = L$y, z = L$size[i], high = L$high[i], low = L$low[i], color = L$color[i], segmentColor = L$linkColor[i]))
        if(special){names(dt)[2] <- 'name'}
        if(L$shape[i] == 'treemap'){names(dt)[[which(names(dt) == 'z')]] <- 'value'}
        h %<>% highcharter::hc_add_series(data = dt, name = L$x[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      }
    }
  } else {
    stop('either x or y must be categorical!')
  }
  if(drll){h %<>% hc_drilldown(allowPointDrilldown = TRUE, series = dd)}

  return(h %>% highcharter.applyConfig(config))
}

highcharter.line = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){

  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'spline', 'line'), x = x, y = y, color = color, config = config)
}

highcharter.area = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){

  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'areaspline', 'area'), x = x, y = y, color = color, config = config)
}

highcharter.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){

  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = 'column', x = x, y = y, color = color, config = config)
}

highcharter.bubble = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, config = NULL){

  config = highcharter.bubble.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    list.edit(dimclass = highcharter.bubble.defset$dimclass %>% list.edit(z = 'numeric', size = NULL))

  obj %>% highcharter.combo(shape = 'bubble', x = x, y = y, size = size, color = color, config = config)
}

highcharter.area.range = function(obj, x = NULL, low = NULL, high = NULL, color = NULL, config = NULL){

  config = highcharter.area.range.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    list.edit(dimclass = highcharter.area.range.defset$dimclass %>% list.edit(y = 'numeric'), multiples = c('y', 'low', 'high', 'color'))

  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'areasplinerange', 'arearange'), x = x, low = low, high = high, color = color, config = config)
}

highcharter.funnel = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = 'funnel', x = label, y = size, color = color, config = config)
}

highcharter.pyramid = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = 'pyramid', x = label, y = size, color = color, config = config)
}

highcharter.pie = function(obj, label = NULL, theta = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = 'pie', x = label, y = theta, color = color, config = config)
}

highcharter.treemap = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  obj %>% highcharter.combo(shape = 'treemap', x = label, size = size, color = color, config = config)
}

highcharter.scatter = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])

  config = highcharter.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color, extend = c('y', 'x', 'size', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'color')

  obj %<>% prepare4Plot(A, config)

  if(is.null(L$shape)){L$shape = 'point'}
  L$shape = highcharter.shape[L$shape]

  names(L$shape) <- NULL

  hc <- highchart(...)
  for (i in seq(L$y)){
    if(is.na(L$shape[i])){L$shape[i] <- 'scatter'}
    if (L$shape[i] == 'bubble'){
      if (is.null(L$size[i])){
        dta = cbind(obj[, L$x[i]], obj[, L$y[i]], rep(3,nrow(obj)))
      } else {
        dta = cbind(obj[, L$x[i]], obj[, L$y[i]], obj[, L$size[i]])
      }
    } else {dta = cbind(obj[, L$x[i]], obj[, L$y[i]])}

    hc %<>% highcharter::hc_add_series(data    = dta, name = L$y[i],
                                       type    = L$shape[i],
                                       color   = L$color[i])
  }

  hc %>% highcharter.applyConfig(config)

}

# http://rstudio-pubs-static.s3.amazonaws.com/230276_25781805506b41478e93da2a641ad108.html
# hcaes

highcharter.scatter.molten = function(obj, x = NULL, y = NULL, size = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])

  config = highcharter.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, color = color, group = group)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  # Implementation:
  scr = paste0("hchart(obj, 'scatter', hcaes(x = ",
               L$x, ", y = ", L$y,
               chif(is.null(L$size), "", ", size = " %++% L$size),
               chif(is.null(L$group), "", ", group = " %++% L$group),
               chif(is.null(L$color) | inherits(obj[,L$color], "character"), "", ", color = " %++% L$color),"),",
               chif(inherits(obj[,L$color], 'character'), "color = '" %++% obj[1,L$color] %++% "',", ""),
               "...)"
  )


  eval(parse(text = scr)) %>% highcharter.applyConfig(config)
}

# This function is incomplete and does not work! We need to resolve host for quantmod::getSymbol() function to get data first
highcharter.tsline.stock = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])

  config = highcharter.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  hc = highcharter::highchart(type = 'stock') %>% highcharter::hc_add_series(L$x)
  for (i in L$y){
    hc %<>% highcharter::hc_add_series(i)
  }
  return(hc)
}




