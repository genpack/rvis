# Shows densities of numeric features 
df_densities = function(df, features = numerics(df), plotter = 'plotly', scaling = c('none', 'zfactor','standard', 'minmax')){

  plotter = match.arg(plotter)
  scaling = match.arg(scaling)
  if(scaling == 'zfactor') scaling = 'standard'
  nums  = numerics(df) %^% features
  res = NULL
  for(fn in nums){
    den = df %>% pull(fn)
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

####


