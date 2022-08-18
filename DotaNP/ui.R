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
        tabPanel("About",
                 h2("Dota 2: Predicting Wins with Nature's Prophet"),
                 h4("Exposition"),
                 p("Although I could make my project about my current work as an IT Manager, I do have a wealth of informatino about Shortage/Damage Claims on inventory that we ship...
                   I decided to do it about something I'm absolutely fascinated by."),
                 br(),
                 strong("Dota2: Defense of the Ancients"), 
                 p("has a long rich history that can rival the best of college sports rivalries. This video game is the grandfather of a genre called Multiplayer Online Battle Arenas, or MOBA for short.
                   It was the first and original MOBA that started as a custom game from Warcraft 3, a 2002 RTS game by Blizzard, that garned so much popularity it eclipsed the game it was founded in. 
                   Originally began as a mod, its most dedicated and lead developer at the time was approached by Valve to release a fully fledged self-recognized title in Dota 2.
                   The game currently holds the top 6 largest prizepool of all video games ranging from $18 million to $40 million USD. 16 year olds with dreams of becoming millionaires over night has been reality for the professional players of Dota 2.
                   The game is considered one of the greatest strategy games ever created and is held in high regard by other strategy communities such as chess. Magnus Carlsen has exchanged thoughts with professional players and has been known to tune in to watch high profile games.
                   Dota 2's original mod also paved the way for a commercially more successful version of itself called League of Legends which was led by a prior Dota developer. There's quite a bit of bad blood between the communities, but we'll keep it civil inside atleast this Shiny App."),
                 p("Dota 2's professional community is still thriving and as an avid fan I keep up with most professional games. Partially inspired by OpenAI and its attempts to teach computers to compete against human players like the chess machines Stockfish,Lc0, and the better known Deep Blue,
                   I thought I'd atleast take a stab at predicting win probabilities for my favorite hero, Nature's Prophet."),
                 p("A quick summary on how Dota 2 is played is required for better understanding of our data. Dota 2 is a 5v5 game played on a square map with 3 'lanes' serving as the main connection between players. 
                   Each player chooses 1 of these 3 lanes and fights over gaining gold to purchase items to assist in killing the enemy and ultimately destory the opponents' base. This base is called the opponents 'ancient' as it's based off fantasy lore.
                   Each player chooses 1 of 123 heroes (as of writing this on 8.18.22), each with their own unique abilities and playstyles, to compete against each other. My favorite hero to play right now is Nature's Prophet."),
                 p("Nature's Prophet is a hero with abilities that focus on having a global presence, pushing lanes toward the enemy base aggressively, and using summoned units to create pressure on the map.
                   As you can imagine, that may influence certain variables to be more impactful for him than other heros."),
                 br(),
                 h3("The Goal"),
                 p("The goal of this app is to predict whether Nature's Prophet team will win given certain variables and different statistical models. The data we are using is a collection of games gathered from 
                 a group of professional and semi-professional tournaments and leagues that all contained Nature's Prophet. These games were all played on 7.31d. It is important to note because the game can change drastically from patch to patch."),
                 h3("The Variables"),
                 p("The variables used are all stats from the games themselves. The list and a short description of what each means."),
                 # class(dataModel$win)
                 # class(dataModel$gold_per_min)
                 # class(dataModel$net_worth)
                 # class(dataModel$gold)
                 # class(dataModel$kills)
                 # class(dataModel$tower_damage)
                 # class(dataModel$duration)
                 # class(dataModel$lane)
                 # class(dataModel$lane_role)
                 strong("1. gold_per_min"), p("Sometimes shortened to GPM, gold per minute represents how much gold a hero was accuring every minute. This metric can change throughout the game and is a hybrid measurement between a hero's power and player efficiency. A higher GPM means you're likely strong and efficient."),
                 strong("2. net_worth"), p("Another measure of gold, net worth is the final amount of gold a hero is worth at the end of the game. Gold can be spent on items, most appopriately thought of as assets, as well as consumables which disappear after use. Net worth is the 'gold' standard to measure a hero's power in the game."),
                 strong("3. gold"), p("Although it may seem redundant, gold here is just the amount of gold the player had left at the end of the game. Sometimes players are saving for a big item that they hope will change the next fight, other times they get it just in time and have no money left to buy a second life after they die."),
                 strong("4. kills"), p("This variable is a count of kills. The more kills a hero has, the more gold they would have, but it also is a representation of how much impact they had on a game. Some heros thrive on killing other heros, where some want to avoid fighting and killing for quite some time."),
                 strong("5. tower_damage"),p("This variable indicates how much damage a hero caused to the enemy buildings. Some heros excel at destroying buildings rather than killing.. sometimes they're good at both!"),
                 strong("6.duration"),
                 strong("7. lane"),
                 strong("8. lane_role"),
                 h3("How to use the app"),
                 strong("1. Explore the variables you're interested in by using the EDA options and the Plot panel to view key metrics."),
                 strong("2. Pick the variables you'd like to create a model with in the modeling tab options."),
                 strong("3. Click the Train Models button to filter the data using your variables and training a GLM/logistic regression model, classification tree, and a random forest."),
                 strong("4. View the stats and measures of success from your models and play around to see if you can find the variables that will give you the strongest model."),
                 strong("5. Move to the Predictions tab and enter some of your own values you'd like to predict off of. These predictions will be using the randomforest model."),
                 strong("6. Click the Predict button on the left and see if you'd win the game or not."),
                 h4("Sources & Packages"),
                 p("readr
                   dplyr
                   randomForest
                   caret
                   ggplot2
                   gbm
                   ggsci
                   shinythemes
                   shiny
                   DT
                   knitr
                   tree"),
                 uiOutput("url"),
                 uiOutput("url2")),
                   
        
        tabPanel("Plot",plotOutput("histPlot")),
        tabPanel("Modeling",
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
