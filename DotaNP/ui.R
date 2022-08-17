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
library(caret)
library(knitr)
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
      actionButton("modelButton","Train Models"),
      actionButton("predictionButton","Make a prediction")
      #checkboxInput("conservation", h4("Color Code Conservation Status", style = "color:red;")),
      
      #conditionalPanel(condition = "input.conservation",
      #                 checkboxInput("CB",label=h4("Opacity = REM"),value=FALSE,width="100%"),
     ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("histPlot")),
        tabPanel("Modelling",
                 h4("GLM Summary"),
                 verbatimTextOutput("sum"),
                 verbatimTextOutput("confusMatr"),
                 verbatimTextOutput("dataTrain"),
                 h4("Classification Tree Summary"),
                 textOutput("classTreeSumm"),
                 verbatimTextOutput("pruneStats"), #sortofwork
                 
                 plotOutput("pruneGraph"), #works
                 h5("Tree Fit without Pruning"),
                 dataTableOutput("fullTree"), 
                 verbatimTextOutput("accFull"),
                 h5("Tree Fit with Pruning"),
                 #dataTableOutput("pruneTree"),
                 #verbatimTextOutput("accPrune"),
                 h4("Random Forest Summary"),
                 h5("Train Confusion Matrix"),
                 verbatimTextOutput("rfConfMat"),
                 h5("Test Confusion Matrix"),
                 verbatimTextOutput("rfTestConfMat")
                 #textOutput("glmStats")
        ), #end of tabpanel 2
        
        
        tabPanel("Predictions",
                 verbatimTextOutput("userPrediction"),
                 conditionalPanel(condition = "input.cbgInput.includes('net_worth')",
                                  numericInput("userNetworth", label = h4("Net Worth"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('gold_per_min')",
                                  numericInput("userGPM", label = h4("Gold Per Min"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('gold')",
                                  numericInput("userGold", label = h4("Gold"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('kills')",
                                  numericInput("userKills", label = h4("Kills"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('tower_damage')",
                                  numericInput("userTD", label = h4("Tower Damage"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('duration')",
                                  numericInput("userDuration", label = h4("Duration"), value = 1)),
                                  
                 conditionalPanel(condition ="input.cbgInput.includes('lane')",
                                  selectInput("userLane", label = h4("Lane"), 
                                              choices = list("1" = 1, "2" = 2, "3" = 3), 
                                              selected = 1)),
                                  
                 conditionalPanel(condition="input.cbgInput.includes('lane_role')",
                                  selectInput("userLaneRole", label = h4("Lane Role"), 
                                              choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5"=5), 
                                              selected = 1))
                 ),#end of tabPanel3
        
        
        tabPanel("Data Table",
                 dataTableOutput("userTable")
                 )#end of tabPanel4
        
        )#end of tabsetPanel
      )#end of MainPanel
    )#end of sidebar layout
  )#end of FluidPage
)#end of shinyUI
