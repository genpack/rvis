# echarts.R ----------------------------------------------------------------



# Header
# Filename:       echarts.R
# Description:    Contains functions for plotting various charts from js package 'echarts' using standrad inputs.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     19 June 2018
# Last Revision:  19 June 2018
# Version:        0.1.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     19 June 2018       Initial issue with bar, funnel, wordCloud and gauge

echarts.bar.defset = defset %<==>% list(
  dimclass   = list(
    x       = 'character',
    y       = 'numeric'),
  multiples  = 'y',
  essentials = c('x', 'y')
)

echarts.funnel.defset = defset %<==>% list(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric'),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.wordCloud.defset = defset %<==>% list(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric',
    color    = c('character', 'numeric', 'integer', 'factor')),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.build_data <- function(e, ...){
  e$x$data %>%
    dplyr::select(...) %>%
    unname(.) -> data

  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  })

}
echarts.add_bind <- function(e, l, bind, col = "name"){
  e$x$data %>%
    dplyr::select(bind) %>% unname() %>% unlist() -> bind

  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

echarts.get_data <- function(e, serie){
  e$x$data %>%
    dplyr::select(serie) %>%
    unname() %>%
    .[[1]]
}


ebar <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)

  if(missing(serie))
    stop("must pass serie", call. = FALSE)

  if(is.null(name)) # defaults to column name
    name <- serie

  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)

  if(x.index != 0)
    e <- .set_x_axis(e, x.index)

  # build JSON data
  echarts.build_data(e, e$x$mapping$x, serie) -> vector

  if(!is.null(bind))
    vector <- echarts.add_bind(e, vector, bind)

  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )

  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

efunnel <- function(e, values, labels, name = NULL, legend = TRUE, rm.x = TRUE, rm.y = TRUE, ...){

  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)

  # remove axis
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")

  # build JSON data
  funnel <- echarts.build_data(e, values)

  funnel <- echarts.add_bind(e, funnel, labels)

  serie <- list(
    name = name,
    type = "funnel",
    data = funnel,
    ...
  )

  # addlegend

  if(isTRUE(legend)){
    legend <- echarts.get_data(e, labels) %>% as.character()
    e$x$opts$legend$data <- append(e$x$opts$legend$data, legend)
  }

  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e

}

ecloud <- function(e, word, freq, color = NULL, rm.x = TRUE, rm.y = TRUE, ...){

  if(missing(e))
    stop("missing e", call. = FALSE)

  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")

  data <- echarts.build_data(e, freq)
  data <- echarts.add_bind(e, data, word)

  if(!is.null(color)){
    color <- echarts.get_data(e, color)
    for(i in 1:length(data)){
      col <- list(
        normal = list(
          color = color[i]
        )
      )
      data[[i]]$textStyle <- col
    }
  }

  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )

  e$x$opts$series <- append(e$x$opts$series, list(serie))

  e

}

egauge <- function(e, theta, label, config = NULL, ...){
  if(missing(theta) || missing(label))
    stop("missing label, or theta", call. = FALSE)

  if(!inherits(theta, "numeric"))
    stop("must pass numeric or interger", call. = FALSE)

  # remove axis
  e %<>% .rm_axis(T, "x") %>% .rm_axis(T, "y")

  e$x$opts$series <- list(
    list(
      type = "gauge",
      data = list(list(value = theta, name = label)),
      ...
    )
  )
  e
}

echarts.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])

  config = echarts.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  obj %>% e_charts_(L$x) -> e
  for(i in L$y){
    e %<>% ebar(i)
  }
  return(e)
}

echarts.funnel = function(obj, label = NULL, size = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])

  config = echarts.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)
  # todo: apply grioup_by with  config$aggrigator.function, not required if NULL
  # todo: check for column names with space
  obj %>% e_charts() %>% efunnel(L$size, L$label, ...)
}

echarts.wordCloud = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])

  config = echarts.wordCloud.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    verifyConfig(plotter = 'echarts')

  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics

  obj %<>% prepare4Plot(A, config)

  obj %>% e_charts() %>% ecloud(L$label, L$size, L$color, shape = config$cloud.shape, sizeRange = c(config$point.size.min, config$point.size.max))
}


