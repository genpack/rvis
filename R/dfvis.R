# dfvis.R ----------------------------------------------------------------

# Header
# Filename:       dfvis.R
# Description:    Quick visualisations for exploring data frames.
# Author:         Nima Ramezani
# Email :         nima.ramezani@gmail.com
# Last Revision:  23 November 2026
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1                         Initial issue with function df_densities()
# 0.0.2     23 November 2026    Header added, dplyr::pull() namespaced (failed when dplyr was not attached)

# Shows densities of numeric features of a data frame in one chart.
# scaling: how each feature is scaled before computing its density ('zfactor' is an alias of 'standard')
df_densities = function(df, features = numerics(df), plotter = 'plotly', scaling = c('none', 'zfactor','standard', 'minmax')){

  plotter = match.arg(plotter)
  scaling = match.arg(scaling)
  if(scaling == 'zfactor') scaling = 'standard'
  nums  = numerics(df) %^% features
  res = NULL
  for(fn in nums){
    den = df %>% dplyr::pull(fn)
    switch(scaling,
           'none' = {den},
           'standard' = {scale(den)},
           'minmax' = {vect.map(den)}) %>% density -> den
    res %<>% rbind(data.frame(x = den$x, y = den$y, feature = fn))
  }
  if(plotter == 'plotly'){
    plotly::plot_ly(data = res, x = ~x, y = ~y, color = ~feature, type = 'scatter', mode = 'lines', fill = 'tozeroy')
  }
}
