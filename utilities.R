#Utility file

RegressionPlots <- function(fit){
  #This function is based on a similar example from the plotly website
  
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)
  
  # Plot using Plotly
  
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  
  plt1 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(color="lightblue",size = 10, opacity = 0.5,line = list(color = 'rgba(75, 0,75, .8)',
                                                               width = 2)), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "lines", name = "Smooth",
              line = list(color="orange",width = 2),marker = list(color = 'red', opacity=0)) %>% 
    
    layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6")
  
  # QQ Pot
  plt2 <- regMat %>% 
    plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(color="lightblue",size = 10, opacity = 0.5,line = list(color = 'rgba(75, 0,75, .8)',
                                                               width = 2)), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "lines", name = "",
              line = list(color="orange",width = 2),marker = list(color = 'red', opacity=0)) %>% 
    
    layout(title = "Q-Q Plot", plot_bgcolor = "white")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(color="lightblue",size = 10, opacity = 0.5,line = list(color = 'rgba(75, 0,75, .8)',
                                                               width = 2)), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "lines", name = "Smooth",
              line = list(color="orange",width = 2),marker = list(color = 'red', opacity=0)) %>% 
    
    layout(title = "Scale Location", plot_bgcolor = "#e6e6e6")
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = Leverage, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(color="lightblue",size = 10, opacity = 0.5,line = list(color = 'rgba(75, 0,75, .8)',
                                                               width = 2)), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "lines", name = "Smooth",
              line = list(color="orange",width = 2),marker = list(color = 'red', opacity=0)) %>% 
    
    layout(title = "Diagnostic plots for fit", plot_bgcolor = "#e6e6e6") %>% 
    
  layout(annotations = list(
    list(x = -1 , y = 2.05, text = "Residuals vs Fitted Values", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.1 , y = 2.05, text = "Q-Q Plot", showarrow = F, xref='paper', yref='paper'),
    list(x = -1 , y = 0.9, text = "Scale Location", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.2 , y = 0.9, text = "Leverage vs Residuals", showarrow = F, xref='paper', yref='paper'))
  )
  
  # plt = list(plt1, plt2, plt3, plt4)
  plt <- subplot(plt1, plt2,plt3,plt4,titleX = TRUE, titleY = TRUE,nrows=2)
  return(plt)
}
