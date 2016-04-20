#EH Shiny Dashboard
#R.Phillips
#160420

#Goal of this script is to display some interactive dashboard basedo n the eharmony data
#following tutorial: http://www.r-bloggers.com/playing-with-r-shiny-dashboard-and-google-analytics-data/
###
#figure out how to make a dashboard appear
#figure out how to make a particular box appear
#pick a visualization to put in that box
#implement that visualization.
###
#housekeeping
setwd("~/Desktop/Eharm")
library(shiny)
library(shinydashboard)
husb_data<-read.csv('all_baseline_husband_w_avg.csv')
###
#figure out how to make a dashboard appear
ui <- fluidPage(
  selectInput(inputId = "feature", 
              label = "Choose a feature", 
              choices = names(husb_data),
              multiple = FALSE),
  plotOutput("scatter"),
  textOutput("text")
)

server <- function(input, output) {
  output$scatter <- renderPlot({
    plot(x = husb_data[["csi_avg"]],y = husb_data[[input$feature]])
  })
  output$text<-renderText({length(input$feature)})
}

shinyApp(ui = ui, server = server)
#figure out how to make a particular box appear
#pick a visualization to put in that box
#implement that visualization.
###