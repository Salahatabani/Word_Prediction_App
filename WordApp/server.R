library(shiny)
library(data.table)
source('../Wpredict.R')
shinyServer(function(input, output) {
  
  dataInput <- reactive({Wpredict(input$text)})
  output$text <- renderText({dataInput()[1]})
  output$more1<- renderText({dataInput()[2]})
  output$more2<- renderText({dataInput()[3]})
  output$more3<- renderText({dataInput()[4]})
  })
