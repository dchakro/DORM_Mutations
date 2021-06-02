library(shiny)
library(shinythemes)
library(ggplot2)

# -- Create essential files (initial run)
if(!file.exists("./data/Table.csv")){
  DF <- readRDS("./data/20201006.FrequencyByMutation.RDS")
  write(colnames(DF),file = "./data/ColumnNames.txt",ncolumns = 1)
  write.table(x = DF,file = "./data/Table.csv",col.names = F,row.names = F,sep = ";",quote = F)
}

# Or load from xz archive
# if(!file.exists("./data/Table.csv")){
#   system(command = "xz -kd ./data/Table.csv.xz",intern = F,wait = T)
# }


## -- END --

theme_plot=theme(axis.line = element_line(colour = "black",size=0.5),panel.border = element_blank(),panel.background=element_blank(),panel.grid.major=element_blank(),axis.text.y= element_text(size = rel(1.5),color="black",margin=unit(c(0.3,0.3,0.3,0.3), "cm")),legend.key= element_rect(fill=NA,colour = NA), axis.ticks.y =element_line(colour = "black"), axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.position="none",text=element_text(family="serif"),axis.ticks.length =unit(0.2, "cm"),axis.title.y = element_text(size=rel(1.5),face="italic"),axis.title.x=element_text(size=rel(1.5),face="italic"))


# colnames(DF)[3]  <- "Total Cases"
# colnames(DF)[4] <- "Number of mutations in different tissues"
function(input, output,session) {
  RunAnalysis <- reactive({list(input$Search,input$size)})
    observeEvent(RunAnalysis(),{
      searchTerm <- input$Search
      plotSize <- as.integer(input$size)
      if(searchTerm == ""){
        command <- ""
        DF <- data.frame(vroom::vroom("./data/Table.csv",delim = ";",col_names = readLines("./data/ColumnNames.txt"),col_types = c(counts="i"),n_max = plotSize),stringsAsFactors = F)
        plotSize <- min(plotSize,nrow(DF),na.rm = T)
        output$table <- renderTable(DF[1:plotSize,c(1,2,3,4)])
        DF$mutsID <- paste(DF$Gene,DF$Mutation,sep="_")
        output$plot <- renderPlot({ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))})
    } else {
        if(grepl(pattern = " ",x = searchTerm,fixed = T)){
          searchTerm <- unlist(strsplit(x = searchTerm, split = " ",fixed = T),use.names = F)
          command <- paste0("grep -i '",searchTerm[1],"' ./data/Table.csv | ",paste0("grep -i '",searchTerm[2:length(searchTerm)],"'",collapse = " | "), " >| ./tmp/tmp.csv")
        } else {
          if(searchTerm != "") command = paste0("grep -i '",searchTerm,"' ./data/Table.csv >| ./tmp/tmp.csv")
        }
        if(command != "" ){
          print(command)
          system(command = command, intern = F, wait=T)
          DF <- data.frame(vroom::vroom("./tmp/tmp.csv",delim = ";",col_names = readLines("./data/ColumnNames.txt"),col_types = c(counts="i"),n_max = plotSize),stringsAsFactors = F)
          plotSize <- min(c(plotSize,nrow(DF)),na.rm = T)
          output$table <- renderTable(DF[1:plotSize,c(1,2,3,4)])
          DF$mutsID <- paste(DF$Gene,DF$Mutation,sep="_")
          output$plot <- renderPlot({ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))})
          # system(command = "rm ./tmp/tmp.csv",intern = F,wait=T)
        }
      }
})
  session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}