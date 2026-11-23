# csscripts.R -------------------------------------------------------------------


# Header
# Filename:       csscripts.R
# Description:    Contains functions generating CSS scripts used by the dashboard builder.
# Author:         Nicolas Berta
# Email :         nicolas.berta@gmail.com
# Start Date:     22 November 2017
# Last Revision:  23 November 2026
# Version:        0.0.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     22 November 2017    Initial issue
# 0.0.2     29 November 2017    Function style.css() added and exported
# 0.0.3     23 November 2026    Bug fixes in skin.style.css(): hover font used the (possibly undefined) non-hover font string,
#                               and the hover rule was always written for skin 'blue' instead of the given skin.
#                               main.header.style.css(): missing comma before the generic font family added.


valid.font.weights  = c('normal', 'bold', 'bolder', 'lighter')
serif.fonts         = c('georgia', 'times', 'times new roman', 'palatino linotype', 'book antiqua', 'palatino')
sans_serif.fonts    = c('arial', 'arial black', 'verdana', 'geneva', 'trebuchet ms', 'helvetica', 'tahoma', 'geneva', 'lucida sans unicode', 'lucida grande', 'impact', 'charcoal', 'comic sans ms', 'cursive')
monospace.fonts     = c('courier', 'courier new', 'lucida console', 'monaco', 'monospace')
valid.fonts         = c(serif.fonts, sans_serif.fonts, monospace.fonts)
font.generic        = c(rep('serif', length(serif.fonts)), rep('sans-serif', length(sans_serif.fonts)), rep('monospace', length(monospace.fonts)))
names(font.generic) = valid.fonts

# Returns the CSS font-family value for the given font(s) followed by their generic family
font.family.css = function(font){
  gnr = font.generic[font[1]]
  if(is.na(gnr)){gnr = 'sans-serif'}
  paste('\"', font, '\"', collapse = ', ') %++% ', ' %++% gnr
}

main.header.style.css = function(font = NULL, weight = NULL, size = NULL, color, background, hover.color, hover.background){
  font %<>% verify('character', default = 'times new roman', varname = 'font', err_src = 'main.header.font.css') %>%
    tolower %>% verify(domain = valid.fonts)

  weight %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 'normal') %>% as.character
  size   %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 24) %>% as.character

  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: ' %++% font.family.css(font) %++% ';
                            font-weight: ' %++% weight %++% ';
                            font-size: '%++% size %++% 'px;
                            }
                            ')))
}

# Generates CSS rules for the logo (title) of the shinydashboard header for the given skin, normal and hover states.
skin.style.css = function(skin = 'blue', font = NULL, font.weight = NULL, font.size = 'NULL', color = NULL, background.color = NULL, hover.font = NULL, hover.font.weight = NULL, hover.font.size = 'NULL', hover.color = NULL, hover.background.color = NULL){
  # Colour verifications (col2Hex() fails for invalid colours):
  color %>% verify('character') %>% col2Hex
  background.color %>% verify('character') %>% col2Hex
  hover.color %>% verify('character') %>% col2Hex
  hover.background.color %>% verify('character') %>% col2Hex

  font.rules = function(font, font.weight, font.size){
    scr = ''
    if(!is.empty(font)){
      font %<>% verify('character') %>% tolower
      scr %<>% paste0('font-family: ', font.family.css(font), '; \n')
    }
    if(!is.empty(font.weight)){
      font.weight %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
      scr %<>% paste0('font-weight: ', font.weight, '; \n')
    }
    if(!is.empty(font.size)){
      font.size %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
      scr %<>% paste0('font-size: ', font.size, '; \n')
    }
    return(scr)
  }

  scr = '.skin-' %++% skin %++% ' .main-header .logo {'
  if(!is.null(color)){scr %<>% paste0('color: ', color, '; \n')}
  if(!is.null(background.color)){scr %<>% paste0('background-color: ', background.color, '; \n')}
  scr %<>% paste0(font.rules(font, font.weight, font.size), '} \n')

  scr %<>% paste0('.skin-', skin, ' .main-header .logo:hover {')
  if(!is.null(hover.color)){scr %<>% paste0('color: ', hover.color, '; \n')}
  if(!is.null(hover.background.color)){scr %<>% paste0('background-color: ', hover.background.color, '; \n')}
  scr %<>% paste0(font.rules(hover.font, hover.font.weight, hover.font.size), '} \n')

  return(scr)
}

# Converts named arguments into an inline CSS style string. Example: style.css(color = 'red', width = '10px')
#' @export
style.css = function(...){
  arglist = list(...)
  arglist %>% names %>% paste0(': ', arglist %>% unlist %>% as.character) %>% paste(collapse = '; ')
}
