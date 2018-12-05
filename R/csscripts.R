# csscripts.R -------------------------------------------------------------------


# Header
# Filename:       csscripts.R
# Description:    Contains functions generating various R scripts.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     22 November 2017
# Last Revision:  29 November 2017
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     22 November 2017    Initial issue
# 0.0.2     29 November 2017    Function style.css() added and exported


valid.font.weights  = c('normal', 'bold', 'bolder', 'lighter')
serif.fonts         = c('georgia', 'times', 'times new roman', 'palatino linotype', 'book antiqua', 'palatino')
sans_serif.fonts    = c('arial', 'arial black', 'verdana', 'geneva', 'trebuchet ms', 'helvetica', 'tahoma', 'geneva', 'lucida sans unicode', 'lucida grande', 'impact', 'charcoal', 'comic sans ms', 'cursive')
monospace.fonts     = c('courier', 'courier new', 'lucida console', 'monaco', 'monospace')
valid.fonts         = c(serif.fonts, sans_serif.fonts, monospace.fonts)
font.generic        = c(rep('serif', length(serif.fonts)), rep('sans-serif', length(sans_serif.fonts)), rep('monospace', length(monospace.fonts)))
names(font.generic) = valid.fonts

main.header.style.css = function(font = NULL, weight = NULL, size = NULL, color, background, hover.color, hover.background){
  font %<>% verify('character', default = 'times new roman', varname = 'font', err_src = 'main.header.font.css') %>%
    tolower %>% verify(domain = valid.fonts)

  weight %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 'normal') %>% as.character
  size   %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 24) %>% as.character

  gnr = font.generic[font[1]]
  if(is.na(gnr)){gnr = 'sans-serif'}
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: ' %++% paste('\"', font, '\"', collapse = ', ') %++% gnr %++% ';
                            font-weight: ' %++% weight %++% ';
                            font-size: '%++% size %++% 'px;
                            }
                            ')))
}

skin.style.css = function(skin = 'blue', font = NULL, font.weight = NULL, font.size = 'NULL', color = NULL, background.color = NULL, hover.font = NULL, hover.font.weight = NULL, hover.font.size = 'NULL', hover.color = NULL, hover.background.color = NULL){
  color %>% verify('character') %>% col2Hex
  background.color %>% verify('character') %>% col2Hex
  scr = '.skin-' %++% skin %++% ' .main-header .logo {'
  if(!is.null(color)){scr %<>% paste0('color: ', color, '; \n')}
  if(!is.null(background.color)){scr %<>% paste0('background-color: ', background.color, '; \n')}

  if(!is.empty(font)){
    font %<>% verify('character') %>% tolower
    gnr = font.generic[font[1]]
    if(is.na(gnr)){gnr = 'sans-serif'}
    fontscr = paste('\"', font, '\"', collapse = ', ') %++% ', '  %++% gnr
    scr %<>% paste0('font-family: ', fontscr, '; \n')
  }

  if(!is.empty(font.weight)){
    font.weight %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-weight: ', font.weight, '; \n')
  }

  if(!is.empty(font.size)){
    font.size %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-size: ', font.size, '; \n')
  }

  scr %<>% paste0('} \n')

  hover.color %>% verify('character') %>% col2Hex
  hover.background.color %>% verify('character') %>% col2Hex
  scr %<>% paste0('.skin-blue .main-header .logo:hover {')
  if(!is.null(hover.color)){scr %<>% paste0('color: ', hover.color, '; \n')}
  if(!is.null(hover.background.color)){scr %<>% paste0('background-color: ', hover.background.color, '; \n')}

  if(!is.empty(hover.font)){
    hover.font %<>% verify('character') %>% tolower
    gnr = font.generic[hover.font[1]]
    if(is.na(gnr)){gnr = 'sans-serif'}
    font.scr = paste('\"', hover.font, '\"', collapse = ', ')  %++% ', ' %++% gnr
    scr %<>% paste0('font-family: ', fontscr, '; \n')
  }

  if(!is.empty(hover.font.weight)){
    hover.font.weight %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-weight: ', hover.font.weight, '; \n')
  }

  if(!is.empty(hover.font.size)){
    hover.font.size %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-size: ', hover.font.size, '; \n')
  }
  scr %<>% paste0('} \n')

  return(scr)
}

# Simple function: needs to be modified later
#' @export
style.css = function(...){
  arglist = list(...)
  arglist %>% names %>% paste0(': ', arglist %>% unlist %>% as.character) %>% paste(collapse = '; ')
}


