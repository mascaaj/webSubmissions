
library(shiny)
library(cluster)
library(tidyverse)
library(DT)


ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive fitting of a Bilinear model"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data parameters"),
      h5("Change this value to increase the maximum y limit of the data"),
      sliderInput("Ymax",
                  "Maximum Y Value:",
                  min = 0,
                  max = 100,
                  value = 4),
      h5("Change this value to add an x offset to the data"),
      sliderInput("offset",
                  "Data Offset:",
                  min = 0,
                  max = 100,
                  value = 7),
      h5("Change this value to add uniform noise to the data"),
      sliderInput("Noise",
                  "Data Noise:",
                  min = 0,
                  max = 10,step=0.5,
                  value = 0.5),
      h3("Bilinear fit parameters"),
      h5("The knee point can be selected by 2 methods "),
      h5("Method 1: Move the slider below and adjust the knee point"),
      sliderInput("kneePoint",
                  "Knee Point:",
                  min = 0,
                  max = 50,step=0.25,
                  value = 6),
      h5("Method 2: Select the checkbox below and use the brush command to select an area on the plot in which the knee point resides"),
      checkboxInput("brushSelect",label="Knee Point by Brush Selection", value = FALSE),
      h5("The following 2 options allow for manipulation of the interaction between the x and y variables. The invert data negates the slope of the data & the relationship option varies the relationship between the x & y variable"),
      # checkboxInput("negative",label="Invert Data", value = TRUE),
      radioButtons("negative", label = h4("Invert Data : Change the slope"),
                   choices = list("Negative" = 1, "Positive" = 2), 
                   selected = 1),
      radioButtons("radio", label = h4("Vary the relationship of the x and y variables"),
                   choices = list("Log" = 1, "Exp" = 2, "Normal" = 3), 
                   selected = 1)


    ),
    

    mainPanel(
      h3("Xy data plot"),
      plotOutput("distPlot",brush="brush1"),
      h5("Here the red line is a linear fit to the data & the blue line is the bilinear fit to the data"),
      h3("Table of brush selected points"),
      dataTableOutput("tab1")
    )
  )
)
