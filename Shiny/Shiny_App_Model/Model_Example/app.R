library(shiny)
library(DT)
library(tidyverse)
#library(nnet)

#THIS EXAMPLE IS FROM: https://predictivehacks.com/how-to-share-your-machine-learning-models-with-shiny/

# #code for model - multinomial logistic regression
# irisModel<-multinom(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = iris)

#Logistic regression model is built and now save it as “irisModel.rds”.
#saveRDS(irisModel, "irisModel.rds")

#read in the model
irisModel <- readRDS("irisModel.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Iris Dataset Predictions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput( inputId = "file1", 
                 label = "upload csv file here",
                 multiple = FALSE,
                 #acceptable input file types
                 accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), 
      
      
      # Button - once model has produced predictions allow users to download
      downloadButton("downloadData", "Download the Predictions")
    ),
    
    # Show the table with the predictions
    mainPanel(
      DT::dataTableOutput("mytable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  reactiveDF<-reactive({
    #YOU have to upload a file for the thing to work
    req(input$file1)
    #read in the uploaded data
    df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
    # run the model and add a column to hold the predictions
    df$predictions<-predict(irisModel, newdata = iris, type ="class")
    #return the updated df with the new prediction column
    return(df)
    
  })
  
  #display the predictions in a table on the app
  output$mytable = DT::renderDataTable({
    req(input$file1)
    
    return(DT::datatable(reactiveDF(),  options = list(pageLength = 100), filter = c("top")))
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)