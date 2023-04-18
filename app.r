########### Market Mix Model ###############

library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(title = "Market Mix Model", dropdownMenuOutput("msgOutput")),
  dashboardSidebar(
    fileInput(
      "concept",
      "Select CSV file",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")),
    
    sliderInput(
      "Slider1",
      label = h3("Train/Test Split %"),
      min = 0,
      max = 100,
      value = 75
    ),
    textOutput("cntTrain"),
    textOutput("cntTest"),
    br()
    
    
  ),
  dashboardBody(
    fluidPage(
      box(
        selectInput("SelectY", label = "Select variable to predict:", choices = c()),
        solidHeader = TRUE,
        width = "3",
        status = "primary",
        title = "Y variable"
      )
      
      
      
    ),
    
    fluidPage(  
      
      tabBox(
        id = "tabset1",
        height = "1000px",
        width = 12,
        
        tabPanel("Data",
                 box(withSpinner(DTOutput(
                   "Data"
                 )), width = 12,
                 style = "height:600px; overflow-y: scroll;overflow-x: scroll;",)),
        tabPanel(
          "Data Summary",
          box(withSpinner(verbatimTextOutput("Summ")), width = 6),
          box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
        ),
        
        tabPanel("Plots",
                 box(withSpinner(plotOutput(
                   "Corr"
                 )), width = 12)),
        
        tabPanel(
          "Model",
          box(
            withSpinner(verbatimTextOutput("Model")),
            width = 6,
            title = "Model Summary"
          ),
          
          box(
            withSpinner(verbatimTextOutput("ImpVar")),
            width = 5,
            title = "Variable Importance"
          )
        ),
        
        tabPanel(
          "Prediction",
          box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
          box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
        )
      )
    )
  )
)


################ Server Code ########################

server <- function(input, output, session) {
  
  
  
  InputDataset <- reactive({
    req(input$concept)
    
    
    concept<-read.csv(input$concept$datapath, 
                      stringsAsFactors = FALSE
    )
    
    concept <-concept %>% select(where(~!all(is.na(.x))))    
    
    data=concept[,!duplicated(colnames(concept))]
    
  })
  
  
  InputDataset_model <- reactive({
    req(InputDataset())

    if (is.null(input$SelectX)) {
      dt <- InputDataset()
    }
    else{
      dt <- InputDataset()[, c(input$SelectX)]
    }
    
  })
  
  

  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "blue",
          main = "Best Fit Line",
          xlab = "Actual",
          ylab = "Predicted"
        )
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  
}




shinyApp(ui, server)