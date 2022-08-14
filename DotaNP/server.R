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
library(DT)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  getData<- reactive ({
    npData<-read_csv("NPBase.csv")
    npData$gold <- as.numeric(npData$gold)
    npData
    dataModel <- data.frame(npData)
    dataModel$win <- as.factor(dataModel$win)
})

  
### Numerical Summaries
    
### Plotting Area   
  
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


##### Modeling area
##dataModel is located in the first reactive command getData



# 
# class(dataModel$win)
# class(dataModel$gold_per_min)
# class(dataModel$net_worth)
# class(dataModel$gold)
# class(dataModel$kills)
# class(dataModel$tower_damage)
# class(dataModel$duration)
# class(dataModel$lane)
# class(dataModel$lane_role)


getDataModel<- reactive ({
  npData<-read_csv("NPBase.csv")
  npData$gold <- as.numeric(npData$gold)
  npData
  dataModel <- data.frame(npData)
  dataModel$win <- as.factor(dataModel$win)
  if ("account_id" %in% colnames(dataModel)){
    dataModel <- subset(dataModel, select = -account_id) 
  }
  
  if ("match_id" %in% colnames(dataModel)){
    dataModel <- subset(dataModel, select = -match_id)
  }
  
  if ("hero_id" %in% colnames(dataModel)){
    dataModel <- subset(dataModel, select = -hero_id)
  }
  
  if ("leaguename" %in% colnames(dataModel)){
    dataModel <- subset(dataModel, select = -leaguename)
  }
  
  if ("start_time" %in% colnames(dataModel)) {
    dataModel <- subset(dataModel, select = -start_time)
  }
  dataModel
  
})

getTrainSetSize <- reactive({
  trainSetSize <- input$train
})

getModelPreds <- reactive({
  preds <- input$cbgInput
})



summStats <- eventReactive(input$modelButton,{
  

  
dataModel<-getDataModel()
trainSetSize<- getTrainSetSize()
#trainSetSize<- input$train
charvec <- getModelPreds()
#preds <- input$cbgInput

#pre-process: center and scale?


#Filtering out predictors not indicated by user
gold      <- "gold"       %in% charvec
gold_per_min    <- "gold_per_min"     %in% charvec
kills <- "kills"  %in% charvec
tower_damage <- "tower_damage" %in% charvec
duration <- "duration" %in% charvec
lane <- "lane" %in% charvec
lane_role <- "lane_role" %in% charvec
net_worth <- "net_worth" %in% charvec

if (!(gold)){
  dataModel<- dataModel %>% select(-gold) 
}
if (!(gold_per_min)){
  dataModel<- dataModel %>% select(-gold_per_min) 
}
if (!(net_worth)){
  dataModel<- dataModel %>% select(-net_worth) 
}
if (!(kills)){
  dataModel<- dataModel %>% select(-kills) 
}
if (!(tower_damage)){
  dataModel<- dataModel %>% select(-tower_damage) 
}
if (!(duration)){
  dataModel<- dataModel %>% select(-duration) 
}
if (!(lane)){
  dataModel<- dataModel %>% select(-lane) 
}
if (!(lane_role)){
  dataModel<- dataModel %>% select(-lane_role) 
}

#Training Model ---------
  #trainsetsize
  dataIndex <- createDataPartition(dataModel$win, p = trainSetSize, list = FALSE)
  dataTrain <- dataModel[dataIndex, ]
  dataTest <- dataModel[-dataIndex, ]
  
  #Logistic Reg
  glmFit <- train(win ~ ., data = dataTrain, 
                  method = "glm", 
                  family = "binomial",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))
  
  
  # predictions <- predict(glmFit, dataTrain)
  # 
  # devia<-summary(glmFit)$deviance
  # 
  # coef <-summary(glmFit)$coefficients
  # 
  # predictions
  # devia
  # coef
    
  
  ## Class Tree Section
  
  # Classification Tree
  # Change . to user input
  
  classTreeFit <- tree(win ~ ., data = dataTrain) # The '.' means all variables to be used as explanatory variables
  summary(classTreeFit)
  
  
  ###Pruning
  pruneFit <- cv.tree(classTreeFit, FUN = prune.misclass)
  
  
  plot(pruneFit$size, pruneFit$dev, type = "b")
  
  
  pruneFit
  
  dfPruneFit <- cbind(size=pruneFit$size,dev=pruneFit$dev)
  dfPruneFit <- data.frame(dfPruneFit)
  dfPruneFit <- dfPruneFit %>% group_by(size)%>%arrange(size)%>%arrange(dev)
  
  
  bestVal <- dfPruneFit$size[1]
  bestVal
  
  ## Final Prune Fit
  pruneFitFinal <- prune.misclass(classTreeFit, best = bestVal)
  
  fullPred <- predict(classTreeFit, dplyr::select(dataTest, -"win"), type = "class")
  
  prunePred <- predict(pruneFitFinal, dplyr::select(dataTest, -"win"), type = "class")
  
  ## Prune Preds
  fullTbl <- table(data.frame(fullPred, dataTest[, "win"]))
  kable(fullTbl)
  accFull<-sum(diag(fullTbl)/sum(fullTbl))
  print(accFull)
  
  pruneTbl <- table(data.frame(prunePred, dataTest[, "win"]))
  kable(pruneTbl)
  accPrune<-sum(diag(pruneTbl)/sum(pruneTbl))
  print(accPrune)
  
  
})

output$sum <- renderPrint({
  models <- summStats()
  summary(models)
})

#Next steps,
# 1. Figure out the predictions error on Modeling page
# 2. Input all the other models
# 3. Render the summary stats
# 4. Ensure training set size user input


output$confMatr<-DT::renderDataTable({
  confusMatr<-confusionMatrix(data = dataTest$win, reference = predict(glmFit, newdata = dataTest))
  confusMatr
})


output$glmStats <- renderText({
  predictions
  devia
  coef
  predictions
})
})
    
