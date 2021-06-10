library(shiny)
library(shinythemes)
library(ggplot2)

# Or load from xz archive
# if(!file.exists("./data/Table.csv")){
#   system(command = "xz -kd ./data/Table.csv.xz",intern = F,wait = T)
# }


## -- END --

theme_bar_plot=theme(axis.line = element_line(colour = "black",size=0.5),panel.border = element_blank(),panel.background=element_blank(),panel.grid.major=element_blank(),axis.text.y= element_text(size = rel(1.5),color="black",margin=unit(c(0.3,0.3,0.3,0.3), "cm")),legend.key= element_rect(fill=NA,colour = NA), axis.ticks.y =element_line(colour = "black"), axis.ticks.x = element_blank(),axis.text.x = element_blank(), legend.position="none",text=element_text(family="serif"),axis.ticks.length =unit(0.2, "cm"),axis.title.y = element_text(size=rel(1.5),face="italic"),axis.title.x=element_text(size=rel(1.5),face="italic"))


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
      searchTerm <- toupper(input$Search)
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
        
        threshold <- min(plotSize,20)
        pie_table_all <- aggregate(DF$count~DF$Gene,FUN=sum)
        colnames(pie_table_all) <- c("Gene","count")
        pie_table_all <- pie_table_all[order(pie_table_all$count,decreasing = T),]
        pie_table <- pie_table_all[1:threshold,]
        pie_table[(threshold+1),] <- list("Others",sum(pie_table_all$count[(threshold+1):length(pie_table_all$count)]))
        rm(pie_table_all);gc()
        sliceColors <- rep(NA,length(pie_table$Gene))
        idx <- which(pie_table$Gene == "Others")
        sliceColors[idx] <- "#c7c7c7"
        sliceColors[-idx] <- viridis::plasma(length(pie_table$Gene[-idx]),direction = 1)
        names(sliceColors) <- pie_table$Gene
        # print(pie_table)
        
        output$plot <- renderPlot({
          ggPie <- ggplot(pie_table,aes(x="",y=count,fill=reorder(Gene,-count)))+geom_bar(stat="identity", width=1, color="white") + theme_void() + scale_fill_manual(values= sliceColors) + theme(legend.position="right",legend.text=element_text(family="serif",size=12),legend.key.size = unit(0.5, "lines")) + guides(fill = guide_legend(title = "Genes", title.position = "top", byrow = T, nrow = (threshold+1),title.theme = element_text(family="serif", face = "italic", angle = 0)))+ coord_polar(theta = "y",direction = -1)
        ggBar <- ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_bar_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))
        
        plotObject1 <-  gridExtra::grid.arrange(ggBar,ggPie,ncol=2,nrow=1,widths = c(2, 2))
        })
    } else {
        if(grepl(pattern = " ",x = searchTerm,fixed = T)){
          resultsFile <- paste0("./tmp/",format(Sys.time(),"%Y%m%d%H%M%s"),"_tmp.csv")
          searchTerm <- unlist(strsplit(x = searchTerm, split = " ",fixed = T),use.names = F)
          command <- paste0("egrep '",searchTerm[1],"' ./data/tissue/",targetTissue,".csv | ",paste0("egrep '",searchTerm[2:length(searchTerm)],"'",collapse = " | "), " >| ",resultsFile)
        } else {
          resultsFile <- paste0("./tmp/",format(Sys.time(),"%Y%m%d%H%M%s"),"_tmp.csv")
          if(searchTerm != "") command = paste0("egrep '",searchTerm,"' ./data/tissue/",targetTissue,".csv >| ",resultsFile)
        }
        if(command != "" ){
          # print(command)
          system(command = command, intern = F, wait=T)
          DF <- data.frame(vroom::vroom(file = resultsFile,delim = ";",col_names = readLines("./data/ColumnNames.txt"),col_types = c(counts="i"),n_max = plotSize),stringsAsFactors = F)
          file.remove(resultsFile) # removes the result file after reading the data
          resultsFile <- ""
          plotSize <- min(c(plotSize,nrow(DF)),na.rm = T)
          if(targetTissue != "all"){
            output$table <- renderTable(DF[1:plotSize,c(1,2,3)])
          } else {
            output$table <- renderTable(DF[1:plotSize,c(1,2,3,4)])
          }
          DF$mutsID <- paste(DF$Gene,DF$Mutation,sep="_")
          
          pie_table_all <- aggregate(DF$count~DF$Gene,FUN=sum)
          threshold <- min(plotSize,20)
          colnames(pie_table_all) <- c("Gene","count")
          
          if(length(pie_table_all$Gene) == 1){
            pie_table <- pie_table_all
            # print(pie_table)
            sliceColors <- viridis::plasma(length(pie_table$Gene),direction = 1)
            names(sliceColors) <- pie_table$Gene            
          } else {
            pie_table_all <- pie_table_all[order(pie_table_all$count,decreasing = T),]
            threshold <- min(threshold,length(pie_table_all$Gene))
            pie_table <- pie_table_all[1:threshold,]
            pie_table[(threshold+1),] <- list("Others",sum(pie_table_all$count[(threshold+1):length(pie_table_all$count)]))
            rm(pie_table_all);gc()
            sliceColors <- rep(NA,length(pie_table$Gene))
            idx <- which(pie_table$Gene == "Others")
            sliceColors[idx] <- "#c7c7c7"
            sliceColors[-idx] <- viridis::plasma(length(pie_table$Gene[-idx]),direction = 1)
            names(sliceColors) <- pie_table$Gene
            # print(pie_table)
          }
          
          output$plot <- renderPlot({
            ggPie <- ggplot(pie_table,aes(x="",y=count,fill=reorder(Gene,-count)))+geom_bar(stat="identity", width=1, color="white") + theme_void() + scale_fill_manual(values= sliceColors) + theme(legend.position="right",legend.text=element_text(family="serif",size=12),legend.key.size = unit(0.5, "lines")) + guides(fill = guide_legend(title = "Genes", title.position = "top", byrow = T, nrow = (threshold+1),title.theme = element_text(family="serif", face = "italic", angle = 0)))+ coord_polar(theta = "y",direction = -1)
            ggBar <- ggplot(data=DF[1:plotSize,],aes(x=reorder(mutsID,-counts),y=counts))+geom_col(fill="#ff5e19",color=NA)+ylab("Number of somatic mutations")+xlab(paste0("Mutations (n=",plotSize,")"))+theme_bar_plot+scale_y_continuous(limits = c(0,max(DF$counts[1:plotSize])),expand = c(0,0))
            
            plotObject1 <-  gridExtra::grid.arrange(ggBar,ggPie,ncol=2,nrow=1,widths = c(2, 2))
          })
        }
      }
})
    session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}