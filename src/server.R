library(shiny)
library(shinythemes)
if(!file.exists("../data/Table.csv")){
  system(command = "xz -kd ../data/Table.csv.xz",intern = F,wait = T)
}

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
  RunAnalysis <- reactive({list(input$Search,input$size)})
    observeEvent(RunAnalysis(),{
      searchTerm <- input$Search
      plotSize <- as.integer(ifelse(test = (input$size == "all"),
                          yes = 653684,
                          no = input$size))
      # output$options <- renderPrint(input$Search)
      if(searchTerm == ""){
        command <- ""
        DF <- data.frame(vroom::vroom("../data/Table.csv",delim = ";",col_names = readLines("../data/ColumnNames.txt"),col_types = c(counts="i")),stringsAsFactors = F)
        plotSize <- min(plotSize,nrow(DF),na.rm = T)
        output$table <- renderTable(DF[1:plotSize,])
        output$plot <- renderPlot({barplot(DF$counts[1:plotSize],xlab = paste0("Mutations (n=",plotSize,")"),ylab="Frequency",col = "#ff5e19",family="serif")})
      } else {
        if(grepl(pattern = " ",x = searchTerm,fixed = T)){
          searchTerm <- unlist(strsplit(x = searchTerm, split = " ",fixed = T),use.names = F)
          command <- paste0("grep -i '",searchTerm[1],"' ../data/Table.csv | ",paste0("grep -i '",searchTerm[2:length(searchTerm)],"'",collapse = " | "), " >| ../tmp/tmp.csv")
        } else {
          if(searchTerm != "") command = paste0("grep -i '",searchTerm,"' ../data/Table.csv >| ../tmp/tmp.csv")
        }
        if(command != "" ){
          print(command)
          # output$options <- renderPrint(command)
          system(command = command, intern = F, wait=T)
          DF <- data.frame(vroom::vroom("../tmp/tmp.csv",delim = ";",col_names = readLines("../data/ColumnNames.txt"),col_types = c(counts="i")),stringsAsFactors = F)
          # print(dim(DF))
          # print(min(c(plotSize,nrow(DF)),na.rm = T))
          #print(plotSize)
          # print(DF[1:plotSize,])
          plotSize <- min(c(plotSize,nrow(DF)),na.rm = T)
          output$table <- renderTable(DF[1:plotSize,])
          output$plot <- renderPlot({barplot(DF$counts[1:plotSize],xlab = paste0("Mutations (n=",plotSize,")"),ylab="Frequency",col = "#ff5e19",family="serif")})
          # system(command = "rm ../tmp/tmp.csv",intern = F,wait=T)
        }
      }
})
  session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}