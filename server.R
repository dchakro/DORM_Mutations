library(shiny)
library(shinythemes)

DF <- readRDS("20200918.Frequency_byTissue.RDS")
Tissues <- readRDS("TissueTypes.RDS")
colnames(DF)[3]  <- "Total Cases"
colnames(DF)[4] <- "Number of mutations in different tissues"

function(input, output,session) {
  DF.shiny <- reactiveValues(m=DF)
  # updateSelectizeInput(session, 'TissueTypes', choices = Tissues, server = TRUE)
  observeEvent(input$TissueTypes,{
    column_idx <- which(colnames(DF) == input$"TissueTypes")
    output$options <- renderPrint(input$"TissueTypes")
    if(length(input$"TissueTypes") > 1){
      print("here")
      DF.shiny$m <- DF[which(rowSums(!is.na(DF[,column_idx]),na.rm = T) > 0),c(1,2,column_idx)]
      output$dim <- renderPrint(dim(DF.shiny$m))
      # DF.shiny$m <- DF[rowSums(!is.na(DF[,column_idx]) > 0),c(1,2,column_idx)]
    }
    if(length(input$"TissueTypes") == 1){
        DF.shiny$m <- DF[!is.na(DF[,column_idx]),c(1,2,column_idx)]
        output$dim <- renderPrint(dim(DF.shiny$m))
    }
    output$table <- DT::renderDataTable(DT::datatable(data = DF.shiny$m, options(list(rownames = FALSE))))
    # tmp <- DF[!is.na(DF[,column_idx]),]
    # DF.shiny$m <- tmp[order(tmp[,(3+seq_len(column_idx))],decreasing = T),]
  })
  
  # output$table <- reactive(DT::renderDataTable(
  #   DT::datatable(
  #     data <- DF[,input$"TissueTypes"],
  #     options = list(lengthMenu = c(5, 10, 50, 100, 200), 
  #                    pageLength = 10 ,
  #                    columnDefs = list(list(width = '120px', targets = c(0)))),
  #     rownames = FALSE
  #     )
  #   ))
  
  # To-Do
  # Add download button to filtered datatable()
  # https://stackoverflow.com/questions/41597062/r-download-filtered-datatable
  
  session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}