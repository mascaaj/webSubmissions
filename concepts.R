codeTest <-6


if(codeTest==1){
  # Proof of concept for medoid.
library(cluster)
df <- data.frame(X = rnorm(100, 0), Y = rpois(100, 2))
plot(df$X, df$Y)
points(pam(df, 1)$medoids, pch = 16, col = "red")
}

if(codeTest==2){
  # Testing out class code for biliniar model fit with knot
param <- 11
mtcars$mpgsp <- ifelse(mtcars$mpg-param>0,mtcars$mpg-param,0)
model1 <- lm(hp~mpg,data=mtcars)
model2 <- lm(hp~ mpgsp+ mpg,data=mtcars)

plot(mtcars$mpg,mtcars$hp, xlab="mpg",bty="n",pch=16)
abline(model1,col="red",lwd=2)

model2lines <- predict(model2,newdata=data.frame(mpg=10:35,mpgsp=ifelse(10:35-param>0,10:35-param,0)))
lines(10:35,model2lines,col="blue",lwd=2)
}

if(codeTest==3){
  # Testing out code for biliniar model fit with knot using random data log data
set.seed(100)
y <- runif(100,0,4)
x <- -log(y^5)+runif(100,-0.5,0.5)+7

xmin <- min(x)
print(min(x))
xmax <- max(x)
test <- data.frame(x,y)
plot(x,y)
param <- 5

test$xprime <- ifelse(test$x-param>0,test$x-param,0)

model1 <- lm(y~x,data=test)
model2 <- lm(y~xprime+x,data=test)

plot(test$x,test$y, xlab="x",bty="n",pch=16)
abline(model1,col="red",lwd=2)

model2lines <- predict(model2,newdata=data.frame(x=xmin:xmax,xprime=ifelse(xmin:xmax-param>0,xmin:xmax-param,0)))
lines(xmin:xmax,model2lines,col="blue",lwd=2)
}

if(codeTest==4){
  # Testing out code for biliniar model fit with knot using random data log data
  set.seed(100)
  offset <- 53
  param <- 44
  y <- runif(100,0,4)
  x <- -exp(y)+runif(100,-0.5,0.5)+offset
  
  xmin <- min(x)
  print(min(x))
  xmax <- max(x)
  test <- data.frame(x,y)
  plot(x,y)

  
  test$xprime <- ifelse(test$x-param>0,test$x-param,0)
  
  model1 <- lm(y~x,data=test)
  model2 <- lm(y~xprime+x,data=test)
  
  plot(test$x,test$y, xlab="x",bty="n",pch=16)
  abline(model1,col="red",lwd=2)
  
  model2lines <- predict(model2,newdata=data.frame(x=xmin:xmax,xprime=ifelse(xmin:xmax-param>0,xmin:xmax-param,0)))
  lines(xmin:xmax,model2lines,col="blue",lwd=2)
}

if(codeTest==5){
  library(shiny)
  library(miniUI)
  library(DT)
  library(cluster)
  BrushGadgetTest <- function(){
    ui <- miniPage(
      gadgetTitleBar("Brushed Select Gadget"),
      plotOutput("plot1",brush="brush1"),
      dataTableOutput("tab1")
      
    )
    server<- function(input,output,session){
      
      brush<- reactive({
        user_input <- input$brush1
        brushed_data <- brushedPoints(test,user_input,xvar="x",yvar="y")
      })
      
      output$plot1 <- renderPlot({
        model1 <- lm(y~x,data=test)

        plot(test$x,test$y,pch=16,cex=1.5,bty="n")
        if(nrow(brush())>1){
        points(pam(brush(), 1)$medoids, pch = 16, col = "red")
        }


        abline(model1,col="red",lwd=2)
        if(nrow(brush())>1){
        a<-pam(brush(), 1)$medoids
        test$xprime <- ifelse(test$x-param>0,test$x-param,0)
        model2 <- lm(y~xprime+x,data=test)
        model2lines <- predict(model2,newdata=data.frame(x=xmin:xmax,xprime=ifelse(xmin:xmax-a[1]>0,xmin:xmax-a[1],0)))
        lines(xmin:xmax,model2lines,col="blue",lwd=2)
        }
      })
      
      output$tab1 <- DT::renderDataTable({
        DT::datatable(brush())
      })
      
      observeEvent(input$done,{
        stopApp()
      })
    }
    runGadget(ui,server)
  }
}

if(codeTest==6){
  # Testing out code for biliniar model fit with knot using random data log data
  set.seed(100)
  offset <- 53
  param <- 44
  y <- runif(100,0,4)
  x <- -exp(y)+runif(100,-0.5,0.5)+offset
  x1 <- -log(y^5)+runif(100,-0.5,0.5)+offset
  x2 <- -y+runif(100,-0.5,0.5)+offset
  test <- data.frame(x,y)

  par(mfrow=c(1,3))
  plot(x,y,main="ScatterPlot Negative Exponential Relationship")
  plot(x1,y,xlab="x",main="ScatterPlot Negative Log Relationship")
  plot(x2,y,xlab="x",main="ScatterPlot Negative Linear Relationship")
}















