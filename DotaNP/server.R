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
  
  plotInput <- reactive ({
    plotType <- input$plotType
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
      plotType <- plotInput()

      if (var == 'gold'){
      varHist <- ggplot(npData, aes(x = gold, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = gold, x = win, color = win, fill = win) )
        }
      }
      else if ( var == 'gold_per_min'){
        varHist <- ggplot(npData, aes(x = gold_per_min, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = gold_per_min, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'kills') {
        varHist <- ggplot(npData, aes(x = kills, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = kills, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'net_worth'){
        varHist <- ggplot(npData, aes(x = net_worth, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = net_worth, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'tower_damage') {
        varHist <- ggplot(npData, aes(x = tower_damage, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = tower_damage, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'duration'){
        varHist <- ggplot(npData, aes(x = duration, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = duration, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'lane') {# should be fixed since these aren't continuous data
        varHist <- ggplot(npData, aes(x = lane, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = lane, x = win, color = win, fill = win) )
        }
      }
      else if (var == 'lane_role'){ # should be fixed since these aren't continuous data
        varHist <- ggplot(npData, aes(x = lane_role, color = win, fill = win))
        if (plotType == 'Boxplot'){
          varHist <- ggplot(npData, aes(y = lane_role, x = win, color = win, fill = win) )
        }
      }
      
if (plotType == 'Histogram'){
   
      varHist + geom_histogram(aes(y=..count..),alpha = .5, position = "identity")+
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
        #geom_vline(aes(xintercept=mean(input$pred)), linetype = "dashed", size = 1)+
        labs(x = paste0("Amount of ",var), y ="Count of Matches", title = paste0("Matches vs ", var))+
        theme_classic()
}
      else if (plotType == 'Boxplot'){
        #boxPlot <- ggplot(npData, aes(x=win,y=gold), fill = gold) +
          varHist +                                                              #Set classic bw plot theme
          #geom_hline(yintercept = median(npData$gold), size = 0.8) +             #Add line for overall median shares
          #geom_point(size = 0.8) +                                                  #Add points
          geom_boxplot(lwd = 0.5, width = 0.5, outlier.size = 2.0, alpha = 0.7) +   #Create boxplot
          xlab("Did Nature's Prophet win?") + ylab(paste0("Amount of ", var)) +
          ggtitle(strong(paste0("Distribution of ",var," across Wins"))) +                                            #Set title
          scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
          scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))                            #Set color theme
    
      }

    })
})
      
    
    
