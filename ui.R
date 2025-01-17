library(shiny)



# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("SET50 Prediction"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    
    checkboxInput("emaCheckBox","EMA",value = FALSE),
    checkboxInput("obvCheckBox","OBV",value = TRUE),
    checkboxInput("macdCheckBox","MACD",value = TRUE),
    checkboxInput("rsiCheckBox","RSI",value = FALSE),
    checkboxInput("evwmaCheckBox","EVWMA",value = TRUE),
    checkboxInput("rocCheckBox","ROC",value = FALSE),
    checkboxInput("cmoCheckBox","CMO",value = FALSE),
    checkboxInput("cciCheckBox","CCI",value = TRUE),
    checkboxInput("wprCheckBox","WPR",value = FALSE),

    sliderInput("lag", 
                "Lagged days", 
                value = 110,
                min = 0,
                max = 200
                ),
    
    # sliderInput("ahead",
    #             "Predict x days from now",
    #             value = 1,
    #             min = 1,
    #             max = 100
    # ),
  
  sliderInput("fold", 
               "N-folds", 
               value = 10,
               min = 2,
               max = 20
              ),
  actionButton("calculateButton", "Calculate")
),
  
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    
 
   
    verbatimTextOutput("txt"),
    h3("SVM"),
    
    radioButtons("kernel", "Kernal Function:",
                 choiceNames = list(
                   "Radial Basis Function",
                   "Polynomial",
                   "Linear"
                 ),
                 inline = TRUE,
                 choiceValues = list(
                   "radial", "polynomial", "linear"
                 )),
    
    uiOutput('matrixSVM'),
    uiOutput('evalSVM'),
    
    ("Average time"),
    htmlOutput("avgTimeSVM"),
    # h5("Precision"),
    # htmlOutput("precisionSVM"),
    # h5("Recall"),
    # htmlOutput("recallSVM"),
    # h5("Accuracy"),
    # htmlOutput("accuracySVM"),
    
    h3("Tree"),
    uiOutput('matrixTree'),
    uiOutput('evalTree'),
    h5("Average time"),
    htmlOutput("avgTimeTree")
    # h5("Precision"),
    # htmlOutput("precisionTree"),
    # h5("Recall"),
    # htmlOutput("recallTree"),
    # h5("Accuracy"),
    # htmlOutput("accuracyTree")
    
    
    
  )
))