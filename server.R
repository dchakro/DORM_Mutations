library(shiny)
library(shinythemes)

DF <- readRDS("20200819.FrequencyByMutation.RDS")
colnames(DF)[3]  <- "Total Cases"
colnames(DF)[4] <- "Number of mutations in different tissues"

function(input, output,session) {
 TwitterURL <- a("@DeepOnCar",
                  href="https://twitter.com/DeepOnCar")
  output$tweetURL <- renderUI({tagList("Find the developer:",TwitterURL,"on twitter.")})
  output$table <- DT::renderDataTable(
    DT::datatable(
      data <- DF,
      options = list(lengthMenu = c(5, 10, 50, 100, 200), 
                     pageLength = 10 ,
                     columnDefs = list(list(width = '120px', targets = c(0)))),
      rownames = FALSE 
  ))
  # To-Do
  # Add download button to filtered datatable()
  # https://stackoverflow.com/questions/41597062/r-download-filtered-datatable
  session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}