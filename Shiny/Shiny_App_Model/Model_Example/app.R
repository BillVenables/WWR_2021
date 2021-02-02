#THIS EXAMPLE IS FROM: https://predictivehacks.com/how-to-share-your-machine-learning-models-with-shiny/


library(shiny)
library(DT)
library(tidyverse)
# model only this wasn't in Bill's original packages script Sorry you may need to install
library(nnet) 

#predict the variety of iris based on 5 variables:
        # Species
        # Sepal.Length
        # Sepal.Width
        # Petal.Length
        # Petal.Width

# #code for multinomial logistic regression model
# irisModel <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)

#Logistic regression model is built and now save it as an .rds file
#??saveRDS
# RDS = Functions to write a single R object to a file, and to restore it
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
      # Input: Select a file and upload it ----
      #??fileInput
      fileInput( inputId = "file_upload1", 
                 label = "upload  your csv file here",
                 multiple = FALSE,
                 #acceptable input file types
                 accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
                 ), # end fileInput
      
      
      # Button - once model has produced predictions allow users to download
      downloadButton("downloadData", "Download the Predictions")
    ),# end sidebarPanel
    
    # Show the table with the predictions
    mainPanel(
      DT::dataTableOutput("pred_table")
    )# end mainPanel
  )# end sidebar Layout
) #end fluidPage (UI)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  reactive_df<-reactive({
    #YOU have to upload a file for the thing to work
    req(input$file_upload1)
    # read in the uploaded data
    # $datapath = The path to a temp file that contains the data that was uploaded. 
    # This file may be deleted if the user performs another upload operation.
    df <- read.csv(input$file_upload1$datapath, stringsAsFactors = TRUE)
    
    # run the model and add a column to hold/display the predictions
    #newdata specifying the first place to look for explanatory variables to be used for prediction
    #type  of predicted value returned = class
         #class gives you the label assigned to that value 
    df$predictions <- predict(irisModel, newdata = iris, type ="class")
    #return the updated df with the new prediction column
    return(df)
    
  })# end reactive_df
  
  #display the predictions in a table on the app
  output$pred_table = DT::renderDataTable({
    req(input$file_upload1)
    
    return(DT::datatable(reactive_df(),  options = list(pageLength = 100), filter = c("top")))
  }) # end pred_table output
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )# end download data output
  
  
  
} # end server

# Run the application 
shinyApp(ui = ui, server = server)