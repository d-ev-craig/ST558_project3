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
library(DT)

# Define UI for application that draws a histogram



shinyUI(fluidPage(
  
  # Application title
  titlePanel(uiOutput("title")),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("EDA Page Options:"),
      selectInput("pred", "Pick Variable of Interest", selected = "gold", choices = c("gold_per_min",
                                                                "net_worth",
                                                                "gold",
                                                                "kills",
                                                                "tower_damage",
                                                                "duration",
                                                                "lane",
                                                                "lane_role")),
      selectInput("plotType","Pick Plot Type", selected = "Histogram", choices = c("Histogram","Boxplot")),
      br(),
      h3("Modeling Page Options:"),
      sliderInput("train", "Choose Training Data Size",
                  min = .2, max = .8, value = .3, step = .1),
      checkboxGroupInput("cbgInput","Choose Predictors", selected = "net_worth",choices = list("gold_per_min",
                                                               "net_worth",
                                                               "gold",
                                                               "kills",
                                                               "tower_damage",
                                                               "duration",
                                                               "lane",
                                                               "lane_role")),
      actionButton("modelButton","Train Models")
      #checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      
      #conditionalPanel(condition = "input.conservation",
      #                 checkboxInput("CB",label=h4("Opacity = REM"),value=FALSE,width="100%"),
     ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("histPlot")),
        tabPanel("Modelling",DT::renderDataTable("confusMatr"),
                 verbatimTextOutput("sum")
                 #textOutput("glmStats")
        )
      ))
 
    
    # Show outputs
    
)))
