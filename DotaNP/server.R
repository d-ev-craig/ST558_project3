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
library(knitr)
library(tree)
library(mathjaxr)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  getData<- reactive ({
    npData<-read_csv("NPBase.csv")
    npData$gold <- as.numeric(npData$gold)
    npData
    dataModel <- data.frame(npData)
    dataModel$win <- as.factor(dataModel$win)
    dataModel$lane <- as.factor(dataModel$lane)
    dataModel$lane_role <- as.factor(dataModel$lane_role)
    dataModel
})

## About Page
  #Logo
  output$dotaLogo <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('/images/dotaLogo.jfif'))
    filename
    # Return a list containing the filename and alt text
     list(src = filename,
          alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  #Sources
  
  url <- a("Open Dota, an API for Dota Statistics", href="https://www.opendota.com/")
  output$url <- renderUI({
    tagList("URL link:", url)
  })
  
  url2 <- a("Dotabuff.com, for all needs on replay stats and history", href = "https://www.dotabuff.com/")
  output$url2 <- renderUI({
    tagList("URL link:",url2)
  })
  
  url3 <- a("Dota2.com - Nature's Prophet and Image Source", href = "https://www.dota2.com/hero/nature%27sprophet")
  output$url3 <- renderUI({
    tagList("URL link:",url3)
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
      npData <- getData()
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
    
    getTrainSetSize <- reactive({
      trainSetSize <- input$train
    })
    getModelPreds <- reactive({
      preds <- input$cbgInput
    })
    

getDataModel<- eventReactive (input$modelButton,{
  charvec<-getModelPreds()
  
  dataModel <- getData()
  
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
  dataModel
  
})



dataIndex <- reactive({
  
dataModel<-getDataModel()
#trainSetSize<- input$train
#preds <- input$cbgInput
trainSetSize <- getTrainSetSize()

#pre-process: center and scale?

dataIndex <- createDataPartition(dataModel$win, p = trainSetSize, list = FALSE)
dataIndex
})

dataTrain <- reactive({
  dataModel <- getDataModel()
  dataIndex <- dataIndex()
  
  dataTrain <- dataModel[dataIndex, ]
  dataTrain
})



dataTest <- reactive({
  dataModel <- getDataModel()
  dataIndex <- dataIndex()
  #dataTrain <- dataTrain()
  
  dataTest <- dataModel[-dataIndex, ]
  dataTest
})


glmModel <- eventReactive(input$modelButton,{
#Training Model ---------
  
  
##Math Jax
  

  output$ex2 <- renderUI ({
    withMathJax(helpText('$$log((Psuccess|networth)/1-P(success/networth)) = \beta_0 + \beta_1*networth$$'))
  })
  
  
  
  #trainsetsize
  
  dataModel <- getDataModel()
  trainSetSize<- getTrainSetSize()
  dataTrain <- dataTrain()
  
  
  #dataIndex <- createDataPartition(dataModel$win, p = trainSetSize, list = FALSE)
  #dataTrain <- dataModel[dataIndex, ]
  #dataTest <- dataModel[-dataIndex, ]
  
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
    
})

output$confusMatr<-renderPrint({
  glmFit <- glmModel()
  dataTest <- dataTest()
  confusMatr<-confusionMatrix(data = dataTest$win, reference = predict(glmFit, newdata = dataTest))
  confusMatr
})

output$sum <- renderPrint({
  models <- glmModel()
  summary(models)
})



##------------------------Tree Vars

# ClassTree -> pruneTree -> pruneStats -.



output$dataTrain <- renderPrint({
  
  
  classTreeFit <- classTree()
  summary(classTreeFit)# this works
  #dataTrain <- dataTrain() #this passes output
  #pruneTree <- pruneTree() #this fails for not finding dataTrain object
  #pruneStats <- pruneStats() #this fails for also not finding dataTrain object
})


##Unprune Tree
classTree <- reactive({
  dataTrain <- dataTrain()
  classTreeFit <- tree(win ~ ., data = dataTrain)

})


output$classTreeSumm <-renderText({
  
  classTreeSum <- classTree()
  print(classTreeSum)
  
})


# classTree <- reactive({
#   dataTrain <- dataTrain()
#   classTreeFit <- tree(win ~ ., data = dataTrain)
#   
# })



# pruneTree <- reactive({
#   
#   classTreeFit <- classTree()
#   
#   pruneFit <- cv.tree(classTreeFit, FUN = prune.misclass)
# })

##Prune Tree Fit
pruneTree <-eventReactive(input$modelButton,{

  classTreeFit<-classTree()
  
  pruneFit <- cv.tree(classTreeFit, FUN = prune.misclass)
  pruneFit
})


#------------Choosing Best Pruned Model Fit
##Prune Stats Generate
pruneStats <-reactive({
  pruneFit<- pruneTree()
  #classTreeFit<-classTree()
  #pruneFit
#Ordering things so that the best value is always in the first slot of dfPruneFit$size
dfPruneFit <- cbind(size=pruneFit$size,dev=pruneFit$dev)
dfPruneFit <- data.frame(dfPruneFit)
dfPruneFit <- dfPruneFit %>% group_by(size)%>%arrange(size)%>%arrange(dev)


bestVal <- dfPruneFit$size[1]
bestVal

pruneFitFinal <- prune.misclass(classTreeFit, best = bestVal)
pruneFitFinal
#prunePred <- predict(pruneFitFinal, dplyr::select(dataTest, -"win"), type = "class")
})

#Predictions for Summary Tables on Classification Tree

fullPred <- eventReactive( input$modelButton, {
  classTreeFit<- classTree()
  dataTest <- dataTest()
  
  fullPred <- predict(classTreeFit, dplyr::select(dataTest, -"win"), type = "class")
})


output$fullTree <- renderDataTable({
  
  fullPred <- fullPred()
  dataTest <- dataTest()
  
  ##Generate the Comparison Table
  fullTbl <- table(data.frame(fullPred, dataTest$win))
  fullTbl
  #accFull<-sum(diag(fullTbl)/sum(fullTbl))
  #accFull
})

output$accFull <- renderText({ #checked for dataTrain
  
  fullPred <- fullPred()
  dataTest <- dataTest()
  
  #Make table and print accuracy
  fullTbl <- table(data.frame(fullPred, dataTest$win))
  fullTbl
  accFull<-sum(diag(fullTbl)/sum(fullTbl))
  accFull
})

prunePred <- eventReactive(input$modelButton, { #checked for dataTrain
  pruneFitFinal <- pruneStats()
  dataTest <- dataTest()
  prunePred <- predict(pruneFitFinal, dplyr::select(dataTest, -"win"), type = "class")
})


output$pruneTree <- renderDataTable({ #checked for dataTrain

  prunePred <- prunePred()
  dataTest <- dataTest()
  
  pruneTbl <- table(data.frame(prunePred, dataTest$win))
  pruneTbl
  
})

###Comparison of Pruned Fit

output$accPrune <- renderText({#checked for dataTrain
  
  prunePred <- prunePred()
  dataTest <- dataTest()
  
pruneTbl <- table(data.frame(prunePred, dataTest$win))
kable(pruneTbl)
accPrune<-sum(diag(pruneTbl)/sum(pruneTbl))
accPrune

})

  ##Prune Stat Render
output$pruneStats <- renderPrint({ #checked for dataTrain

  fullPred <- fullPred()
  dataTest <- dataTest()
  
  pruneStats <- pruneStats()
  summary(pruneStats)
})

#----------------------Prune Graphs
##Prune Graph Generate
pruneGraph <- eventReactive (input$modelButton, { #checked for dataTrain
  #classTreeFit<-classTree()
  pruneFit <- pruneTree()
  prunePlot <- plot(pruneFit$size, pruneFit$dev, type = "b")
  prunePlot
})
##Prune Graph Render
output$pruneGraph <- renderPlot({ #checked for dataTrain
  pruneGraph <- pruneGraph()
  pruneGraph
})



##------------------------------- Random Forest

rfModel <- eventReactive(input$modelButton,{
    dataModel <- getDataModel()
    trainSetSize<- getTrainSetSize()
    dataTrain <- dataTrain()
  
trainRFModel <- train(win ~ ., data = dataTrain,
                      method = "rf",
                      trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                      tuneGrid = data.frame(mtry = sqrt(ncol(dataTrain) - 1)))
#trainConMat <- confusionMatrix(trainRFModel, newdata = dataTest)
#testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
trainRFModel
})

output$rfStats <- renderPrint({
  rfModel <- rfModel()
  summary(rfModel)
})

output$rfConfMat <- renderPrint({
  trainRFModel<- rfModel()
  dataTest <- dataTest()
  
  trainConMat <- confusionMatrix(trainRFModel, newdata = dataTest)
  #testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
  #testConMat
  trainConMat
})

output$rfTestConfMat <- renderPrint({
  trainRFModel<- rfModel()
  dataTest <- dataTest()
  
  testConMat <- confusionMatrix(data = dataTest$win, reference = predict(trainRFModel, newdata = dataTest))
  testConMat
})

fullPreds <- eventReactive(input$modelButton,{

  
  fullPred <- predict(classTreeFit, dplyr::select(dataTest, -"win"), type = "class")
  
  
})


## Predictions

#Grab User Inputs
userNetworth<-reactive({ userNetworth <- input$userNetworth})
userGPM <- reactive ({ userGPM <- input$userGPM})
userGold<- reactive ({userGold<-input$userGold})
userKills<- reactive ({userKills<-input$userKills})
userTD<- reactive ({userTD<-input$userTD})
userDuration <- reactive ({userDuration<-input$userDuration})
userLane <- reactive ({userLane<-input$userLane})
userLaneRole <- reactive ({userLaneRole<-input$userLaneRole})


#Upon button press, execute predict function
prediction <- eventReactive(input$predictionButton,{
  
  #Pass in our trained model from earlier
  trainRFModel <- rfModel()
  #Pass in our selected vars
  charvec<-getModelPreds()
  
#Pass in user vars
  userNetworth <- userNetworth()
  userGPM <- userGPM()
  userGold <- userGold()
  userKills <- userKills()
  userTD <- userTD()
  userDuration <- userDuration()
  userLane <- userLane()
  userLaneRole <- userLaneRole()

#Build a dataframe out of values
  userPredVals <- data.frame("gold"=userGold,"gold_per_min"=userGPM,"kills"=userKills,"tower_damage"=userTD,
             "duration"=userDuration,"lane"=userLane,"lane_role"=userLaneRole,"net_worth"=userNetworth)
  
  userPredVals$lane <- as.factor(userPredVals$lane)
  userPredVals$lane_role <- as.factor(userPredVals$lane_role)
  
  gold      <- "gold"       %in% charvec
  gold_per_min    <- "gold_per_min"     %in% charvec
  kills <- "kills"  %in% charvec
  tower_damage <- "tower_damage" %in% charvec
  duration <- "duration" %in% charvec
  lane <- "lane" %in% charvec
  lane_role <- "lane_role" %in% charvec
  net_worth <- "net_worth" %in% charvec
  
  if (!(gold)){
    userPredVals<- userPredVals %>% select(-gold) 
  }
  if (!(gold_per_min)){
    userPredVals<- userPredVals %>% select(-gold_per_min) 
  }
  if (!(net_worth)){
    userPredVals<- userPredVals %>% select(-net_worth) 
  }
  if (!(kills)){
    userPredVals<- userPredVals %>% select(-kills) 
  }
  if (!(tower_damage)){
    userPredVals<- userPredVals %>% select(-tower_damage) 
  }
  if (!(duration)){
    userPredVals<- userPredVals %>% select(-duration) 
  }
  if (!(lane)){
    userPredVals<- userPredVals %>% select(-lane) 
  }
  if (!(lane_role)){
    userPredVals<- userPredVals %>% select(-lane_role) 
  }
    


  
  #Predict
  prediction <- predict(trainRFModel, newdata = userPredVals)
  prediction
})

#render our prediction

output$userPrediction<- renderPrint({
  prediction<-prediction()
  prediction
  
})


#Next steps,
# 1. Figure out the predictions error on Modeling page
# 2. Input all the other models
# 3. Render the summary stats
# 4. Ensure training set size user input
# Frequency table/Set of Box plots/Contingency Table
# Continuous Variables Scatterplots


#Datatable Output


  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "yourData.csv",
    content = function(file) {
      write.csv(dataTrain(), file, row.names = FALSE)
    }
  )



output$userTable <- renderDataTable({
  dataModel <- getDataModel()
  dataModel
})

output$glmStats <- renderPrint({
  predictions
  devia
  coef
  predictions
})
})


    
