#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  # Application title
  titlePanel(uiOutput("title")),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Pick Your Variable of Interest:"),
      selectInput("pred", "", selected = "gold", choices = c("gold_per_min",
                                                                "net_worth",
                                                                "gold",
                                                                "kills",
                                                                "tower_damage",
                                                                "duration",
                                                                "lane",
                                                                "lane_role")),
      br(),
      #sliderInput("size", "Size of Points on Graph",
                  #min = 1, max = 10, value = 5, step = 1),
      #checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      
      #conditionalPanel(condition = "input.conservation",
      #                 checkboxInput("CB",label=h4("Opacity = REM"),value=FALSE,width="100%"),
     ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("boxPlot")),
        tabPanel("Other Plot",plotOutput("distPlot"))
      ))
 
    
    # Show outputs
    
)))
