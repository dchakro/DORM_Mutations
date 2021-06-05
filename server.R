library(shiny)
library(shinythemes)
library(ggplot2)

# Or load from xz archive
# if(!file.exists("./data/Table.csv")){
#   system(command = "xz -kd ./data/Table.csv.xz",intern = F,wait = T)
# }


## -- END --

theme_plot=theme(axis.line = element_line(colour = "black",size=0.5),panel.border = element_blank(),panel.background=element_blank(),panel.grid.major=element_blank(),axis.text.y= element_text(size = rel(1.5),color="black",margin=unit(c(0.3,0.3,0.3,0.3), "cm")),legend.key= element_rect(fill=NA,colour = NA), axis.ticks.y =element_line(colour = "black"), axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.position="none",text=element_text(family="serif"),axis.ticks.length =unit(0.2, "cm"),axis.title.y = element_text(size=rel(1.5),face="italic"),axis.title.x=element_text(size=rel(1.5),face="italic"))


# colnames(DF)[3]  <- "Total Cases"
# colnames(DF)[4] <- "Number of mutations in different tissues"
function(input, output,session) {
  RunAnalysis <- reactive({list(input$Search,input$size,input$tissue)})
    observeEvent(input$reset_input, {
      updateNumericInput(session, "size", value = 100)
      updateTextInput(session, "tissue", value = "all")
      updateSearchInput(session,"Search",value = "",trigger = T)
    })
    observeEvent(RunAnalysis(),{
      searchTerm <- input$Search
      plotSize <- as.integer(input$size)
      targetTissue <- input$tissue
      if(searchTerm == ""){
        command <- ""
        DF <- data.frame(vroom::vroom(paste0("./data/tissue/",targetTissue,".csv"),delim = ";",col_names = readLines("./data/ColumnNames.txt"),col_types = c(counts="i"),n_max = plotSize),stringsAsFactors = F)
        plotSize <- min(plotSize,nrow(DF),na.rm = T)
        if(targetTissue != "all"){
          output$table <- renderTable(DF[1:plotSize,c(1,2,3)])
        } else {
          output$table <- renderTable(DF[1:plotSize,c(1,2,3,4)])
        }
        # Trying top N
        DF$mutsID <- paste(DF$Gene,DF$Mutation,sep="_") 
        
        threshold <- 20
        topN <- DF[1:threshold,c(1,3)]
        colnames(topN) <- c("Gene","count")
        pie_table <- aggregate(topN$count~topN$Gene,FUN=sum)
        colnames(pie_table) <- c("Gene","count")
        pie_table <- pie_table[order(pie_table[,2],decreasing = T),]
        # print(pie_table)
        
        
        output$plot <- renderPlot({
          ggPie <- ggplot(pie_table,aes(x="",y=count,fill=reorder(Gene,-count)))+geom_bar(stat="identity", width=1, color="white") + theme_void() + scale_fill_viridis_d(option = "plasma",direction = 1) + theme(legend.position="right",legend.text=element_text(family="serif",size=10)) + guides(fill = guide_legend(title = "Genes", title.position = "top",title.theme = element_text(family="serif", face = "italic", angle = 0,size=12)))+ coord_polar(theta = "y",direction = -1)
        ggBar <- ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))
        
        plotObject1 <-  gridExtra::grid.arrange(ggBar,ggPie,ncol=2,nrow=1,widths = c(6, 2))
        })
    } else {
        if(grepl(pattern = " ",x = searchTerm,fixed = T)){
          searchTerm <- unlist(strsplit(x = searchTerm, split = " ",fixed = T),use.names = F)
          command <- paste0("grep -i '",searchTerm[1],"' ./data/tissue/",targetTissue,".csv | ",paste0("grep -i '",searchTerm[2:length(searchTerm)],"'",collapse = " | "), " >| ./tmp/tmp.csv")
        } else {
          if(searchTerm != "") command = paste0("grep -i '",searchTerm,"' ./data/tissue/",targetTissue,".csv >| ./tmp/tmp.csv")
        }
        if(command != "" ){
          print(command)
          system(command = command, intern = F, wait=T)
          DF <- data.frame(vroom::vroom("./tmp/tmp.csv",delim = ";",col_names = readLines("./data/ColumnNames.txt"),col_types = c(counts="i"),n_max = plotSize),stringsAsFactors = F)
          plotSize <- min(c(plotSize,nrow(DF)),na.rm = T)
          if(targetTissue != "all"){
            output$table <- renderTable(DF[1:plotSize,c(1,2,3)])
          } else {
            output$table <- renderTable(DF[1:plotSize,c(1,2,3,4)])
          }
          DF$mutsID <- paste(DF$Gene,DF$Mutation,sep="_")
          
          threshold <- 20
          topN <- DF[1:threshold,c(1,3)]
          colnames(topN) <- c("Gene","count")
          pie_table <- aggregate(topN$count~topN$Gene,FUN=sum)
          colnames(pie_table) <- c("Gene","count")
          pie_table <- pie_table[order(pie_table[,2],decreasing = T),]
          print(pie_table)
          
          output$plot <- renderPlot({
            ggPie <- ggplot(pie_table,aes(x="",y=count,fill=reorder(Gene,-count)))+geom_bar(stat="identity", width=1, color="white") + theme_void() + scale_fill_viridis_d(option = "plasma",direction = 1) + theme(legend.position="right",legend.text=element_text(family="serif",size=10)) + guides(fill = guide_legend(title = "Genes", title.position = "top",title.theme = element_text(family="serif", face = "italic", angle = 0,size=12)))+ coord_polar(theta = "y",direction = -1)
            ggBar <- ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))
            
            plotObject1 <-  gridExtra::grid.arrange(ggBar,ggPie,ncol=2,nrow=1,widths = c(6, 2))
          })
          # system(command = "rm ./tmp/tmp.csv",intern = F,wait=T)
        }
      }
})
    session$onSessionEnded(function() {
        gc()
        # file.remove(c("./data/Table.csv","./data/ColumnNames.txt"))
        stopApp()
      })
}