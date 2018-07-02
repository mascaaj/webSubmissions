library(shiny)
library(cluster)
library(tidyverse)
library(DT)


server <- function(input, output) {
  
  test <- reactive({
    set.seed(100)
    y <- runif(100,0,input$Ymax)
    
    if(input$radio==1){
      if(input$negative==1){
        x <- -log(y^5)+runif(100,-input$Noise,input$Noise)+input$offset
      } else{
        x <- log(y^5)+runif(100,-input$Noise,input$Noise)+input$offset
      }
    } else if(input$radio==2){
      if(input$negative==1){
        x <- -exp(y)+runif(100,-input$Noise,input$Noise)+input$offset
      } else{
        x <- exp(y)+runif(100,-input$Noise,input$Noise)+input$offset
      }
      
    } else if(input$radio==3){
      if(input$negative==1){
        x <- -y+runif(100,-input$Noise,input$Noise)+input$offset
      } else{
        x <- y+runif(100,-input$Noise,input$Noise)+input$offset
      }
    }
    
    test_temp <- data.frame(x,y)
  })
  
  brush<- reactive({
    user_input <- input$brush1
    brushed_data <- brushedPoints(test(),user_input,xvar="x",yvar="y")
  })
  
  output$tab1 <- DT::renderDataTable({
    DT::datatable(brush())
  })
  
  output$distPlot <- renderPlot({
    data <- test()
    xmin <- min(data$x)
    xmax <- max(data$x)
    
    model1 <- lm(y~x,data=data)
    plot(data$x,data$y, xlab="x",ylab ="y",bty="n",pch=16)
    abline(model1,col="red",lwd=2)
    
    if(input$brushSelect==FALSE){
      data$xprime <- ifelse(data$x-input$kneePoint>0,data$x-input$kneePoint,0)
      model2 <- lm(y~xprime+x,data=data)
      model2lines <- predict(model2,newdata=data.frame(x=xmin:xmax,xprime=ifelse(xmin:xmax-input$kneePoint>0,xmin:xmax-input$kneePoint,0)))
      lines(xmin:xmax,model2lines,col="blue",lwd=2)
    }else {
      if(nrow(brush())>1){
        brushKnee <- pam(brush(),1)$medoids
        data$xprime2 <- ifelse(data$x-brushKnee[1]>0,data$x-brushKnee[1],0)
        model3 <- lm(y~xprime2+x,data=data)
        model3lines <- predict(model3,newdata=data.frame(x=xmin:xmax,xprime2=ifelse(xmin:xmax-brushKnee[1]>0,xmin:xmax-brushKnee[1],0)))
        lines(xmin:xmax,model3lines,col="blue",lwd=2)
      }
    }
    
    if(nrow(brush())>1){
      points(pam(brush(), 1)$medoids, pch = 16, col = "red")
    }
  })
  
}


