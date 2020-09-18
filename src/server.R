library(shiny)
library(shinythemes)


# DF <- readRDS("../data/20200819.FrequencyByMutation.RDS")
# write(colnames(DF),file = "../data/ColumnNames.txt",ncolumns = 1)
# write.table(x = DF,file = "../data/Table.csv",col.names = F,row.names = F,sep = ";",quote = F)

# DF <- readRDS("../data/20200918.Frequency_byTissue.RDS")
# write(colnames(DF),file = "../data/ByTissue.ColumnNames.txt",ncolumns = 1)
# write.table(x = DF,file = "../data/ByTissue.table.csv",col.names = F,row.names = F,sep = ";",quote = F)

# searchTerm <- "KRAS G12"

#
# 

# colnames(DF)[3]  <- "Total Cases"
# colnames(DF)[4] <- "Number of mutations in different tissues"
function(input, output,session) {
  observeEvent(input$Search,{
    searchTerm <- input$Search
    output$options <- renderPrint(input$Search)
    if(searchTerm == "*"){
      command <- ""
      DF <- data.frame(vroom::vroom("../data/Table.csv",delim = ";",n_max = 100,col_names = readLines("../data/ColumnNames.txt")),stringsAsFactors = F)
      output$table <- DT::renderDataTable(DT::datatable(data = DF, options(list(rownames = FALSE))))
      # system(command = "rm ../tmp/tmp.csv",intern = F)
      output$plot <- renderPlot({barplot(DF$counts,xlab = "Mutations",ylab="Frequency",col = "#ff5e19")})
    } else {
      if(grepl(pattern = " ",x = searchTerm,fixed = T)){
        searchTerm <- unlist(strsplit(x = searchTerm, split = " ",fixed = T),use.names = F)
        command <- paste0("grep -i ",searchTerm[1]," ../data/Table.csv | ",paste0("grep -i ",searchTerm[2:length(searchTerm)],collapse = " | "), " >| ../tmp/tmp.csv")
      } else {
        if(searchTerm != "") command = paste0("grep -i ",searchTerm," ../data/Table.csv >| ../tmp/tmp.csv")
      }
      if(command != "" ){
        print(command)
        output$options <- renderPrint(command)
        system(command = command,intern = F)
        DF <- data.frame(vroom::vroom("../tmp/tmp.csv",delim = ";",col_names = readLines("../data/ColumnNames.txt")),stringsAsFactors = F)
        output$table <- DT::renderDataTable(DT::datatable(data = DF, options(list(rownames = FALSE))))
        # system(command = "rm ../tmp/tmp.csv",intern = F)
        output$plot <- renderPlot({barplot(DF$counts,xlab = "Mutations",ylab="Frequency",col = "#ff5e19")})
      }
    }
})
    
    
    # DF.shiny$m <- data.frame(vroom::vroom("../tmp/tmp.csv",col_names = readLines("../data/ColumnNames.txt")),stringsAsFactors = F)
    # tmp <- DF[!is.na(DF[,column_idx]),]
    # DF.shiny$m <- tmp[order(tmp[,(3+seq_len(column_idx))],decreasing = T),]
  
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