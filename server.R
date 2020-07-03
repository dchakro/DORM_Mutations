# Load the ggplot2 package which provides
# the 'mpg' dataset.
# library(ggplot2)
library(shiny)
library(shinythemes)

DF <- readRDS("20200703.FrequencyByResidue.RDS")
DF <- DF[1:10000,]
colnames(DF)[1] <- "Gene Residue"
colnames(DF)[2]  <- "Total Cases"
colnames(DF)[3] <- "Number of mutations in different tissues"

function(input, output,session) {
#   output$HotspotPlot <- renderImage({
#     return(list(
#       src = "Hotspot_resize.jpg",
#       contentType = "image/jpg",
#       alt = "Hotspot Plot"
#     ))
#   }, deleteFile = FALSE) 
  # Filter data based on selections
  TwitterURL <- a("@DeepOnCar",
                  href="https://twitter.com/DeepOnCar")
  output$tweetURL <- renderUI({tagList("Find the developer:",TwitterURL,"on twitter.")})
  output$table <- DT::renderDataTable(
    DT::datatable(
      data <- DF,
      options = list(lengthMenu = c(5, 10, 50, 100, 200), 
                     pageLength = 50 ,
                     columnDefs = list(list(width = '120px', targets = c(0)))),
      rownames = FALSE 
  ))
  # Add download button to filtered datatable()
  # https://stackoverflow.com/questions/41597062/r-download-filtered-datatable
  session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}