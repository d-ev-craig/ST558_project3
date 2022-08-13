#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(gbm)
library(ggsci)
library(shinythemes)
library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  getData<- reactive ({
    npData<-read_csv("NPBase.csv")
    npData$gold <- as.numeric(npData$gold)
    npData
    
  })
  
  ghettoData <- reactive ({
    var <- input$pred
  })
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

    output$histPlot<- renderPlot({
      
      var <- ghettoData()
      if (var == 'gold'){
      varHist <- ggplot(npData, aes(x = gold, color = win, fill = win))
      #Dynamic name change still needed
      }
      else if ( var == 'gold_per_min'){
        varHist <- ggplot(npData, aes(x = gold_per_min, color = win, fill = win))
      }
      else if (var == 'kills') {
        varHist <- ggplot(npData, aes(x = kills, color = win, fill = win))
      }
      else if (var == 'net_worth'){
        varHist <- ggplot(npData, aes(x = net_worth, color = win, fill = win))
      }
      else if (var == 'tower_damage') {
        varHist <- ggplot(npData, aes(x = tower_damage, color = win, fill = win))
      }
      else if (var == 'duration'){
        varHist <- ggplot(npData, aes(x = duration, color = win, fill = win))
      }
      else if (var == 'lane') {
        varHist <- ggplot(npData, aes(x = lane, color = win, fill = win))
      }
      else if (var == 'lane_role'){
        varHist <- ggplot(npData, aes(x = lane_role, color = win, fill = win))
      }
      varHist + geom_histogram(aes(y=..count..),alpha = .5, position = "identity")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        #geom_vline(aes(xintercept=mean(input$pred)), linetype = "dashed", size = 1)+
        labs(x = paste0("Amount of ",var), y ="Count of Matches", title = paste0("Matches vs ", var))+
        theme_classic()
    })
    
      
    })
    
    
