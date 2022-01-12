library(shiny)
library(data.table)
library(ggplot2)

# Or load from xz archive
# if(!file.exists("./data/Table.csv")){
#   system(command = "xz -kd ./data/Table.csv.xz",intern = F,wait = T)
# }
ColumnNames <- readLines("./data/ColumnNames.txt")

## -- END --

theme_bar_plot=theme(axis.line = element_line(colour = "black",
                                              size=0.5),
                     panel.border = element_blank(),
                     panel.background=element_blank(),
                     panel.grid.major=element_blank(),
                     axis.text.y= element_text(size = rel(1.5),
                                               color="black",
                                               margin=unit(c(0.3,0.3,0.3,0.3), 
                                                           "cm")),
                     legend.key= element_rect(fill=NA,
                                              colour = NA), 
                     axis.ticks.y =element_line(colour = "black"), 
                     axis.ticks.x = element_blank(),
                     axis.text.x = element_blank(), 
                     legend.position="none",
                     text=element_text(family="serif"),
                     axis.ticks.length =unit(0.2, "cm"),
                     axis.title.y = element_text(size=rel(1.5),
                                                 face="italic"),
                     axis.title.x=element_text(size=rel(1.5),
                                               face="italic"))


function(input, output,session) {
  RunAnalysis <- reactive({list(input$Search, input$size, input$tissue)})
    observeEvent(input$reset_input, {
      updateNumericInput(session, "size", value = 100)
      updateTextInput(session, "tissue", value = "all")
      shinyWidgets::updateSearchInput(session,"Search", value = "", trigger = T)
    })
    observeEvent(RunAnalysis(),{
      searchTerm <- toupper(input$Search)
      plotSize <- as.integer(input$size)
      targetTissue <- input$tissue
      if(searchTerm == ""){
        command <- ""
        DF <- data.table::fread(file = paste0("./data/tissue/",targetTissue,".csv"),
                                  sep = ";",
                                  header = F,
                                  nrows = plotSize,
                                  stringsAsFactors = F,
                                  showProgress = F)
          colnames(DF) = ColumnNames[1:ncol(DF)]
        if(targetTissue != "all"){
          output$table <- renderTable(DF[1:plotSize, c(1,2,3)])
        } else {
          output$table <- renderTable(DF[1:plotSize, c(1,2,3,4)])
        }
        plotSize <- min(plotSize,nrow(DF), na.rm = T)
         
        DF[,mutsID := paste0(Protein,Mutation,sep="_")]
        
        output$plot <- renderPlot({
        overall_percentage_bar <- readRDS(paste0("./data/bar_with_others/",targetTissue,".RDS"))
        ggBar <- ggplot(data=DF[1:plotSize,], aes(x=reorder(mutsID, -counts),
                                                  y=counts))+
          geom_col(fill="#ff5e19",
                   color=NA)+
          ylab("Number of somatic mutations")+
          xlab(paste0("Mutations (n=", plotSize, ")"))+
          theme_bar_plot+
          scale_y_continuous(limits = c(0, max(DF$counts[1:plotSize])), 
                             expand = c(0,0))
        
        plotObject1 <-  gridExtra::grid.arrange(ggBar,
                                                overall_percentage_bar,
                                                ncol=2,
                                                nrow=1,
                                                widths = c(2, 2))
        })
    } else {
        searchTerm <- gsub("[,;]+[[:space:]]+", "|", searchTerm, fixed = F)
        if(grepl(pattern = " ",x = searchTerm,fixed = T)){
          resultsFile <- paste0("./tmp/",format(Sys.time(),"%Y%m%d%H%M%s"),"_tmp.csv")
          searchTerm <- unlist(strsplit(x = searchTerm, 
                                        split = " ",
                                        fixed = T),
                               use.names = F)
          command <- paste0("egrep -i '",
                            searchTerm[1],
                            "' ./data/tissue/",
                            targetTissue,
                            ".csv | ",
                            paste0("egrep -i '",
                                   searchTerm[2:length(searchTerm)],
                                   "'",
                                   collapse = " | "),
                            " >| ",
                            resultsFile)
        } else {
          resultsFile <- paste0("./tmp/",format(Sys.time(),"%Y%m%d%H%M%s"),"_tmp.csv")
          if(searchTerm != "") command = paste0("egrep -i '", searchTerm, "' ./data/tissue/", targetTissue, ".csv >| ", resultsFile)
        }
        if(command != "" ){
          system(command = command, intern = F, wait=T)
          if(file.info(resultsFile)$size == 0){ # if file is empty
            file.remove(resultsFile) # delete the empty result file
            resultsFile <- ""
            DF <- data.table::fread(
              file = "./data/error.txt",
              sep = ";",
              header = F,
              nrows = plotSize,
              stringsAsFactors = F,
              showProgress = F
            )
            # colnames(DF) = ColumnNames[1:ncol(DF)]
            output$table <- renderTable(DF[, c(1,2,3,4)])
            output$plot <- renderPlot({
              ggplot(data = data.frame()) +
                geom_point() +
                xlim(0, 10) +
                ylim(0, 10) +
                theme_void() +
                annotate(
                  "segment",
                  x = 2.5,
                  xend = 7.5,
                  y = 2.5,
                  yend = 7.5,
                  colour = "red",
                  size= 1.5
                ) + annotate(
                  geom = "text",
                  x = 4,
                  y = 5.5,
                  size=5,
                  label = paste0("Searching: ", toupper(input$Search))
                ) + annotate(
                  geom = "text",
                  x = 5.5,
                  y = 4,
                  size=5,
                  label = "Returns an Empty Table."
                )
            })
            
          } else {
            DF <- data.table::fread(file = resultsFile,
                                    sep = ";",
                                    header = F,
                                    nrows = plotSize,
                                    stringsAsFactors = F,
                                    showProgress = F)
            colnames(DF) = ColumnNames[1:ncol(DF)]
            file.remove(resultsFile) # removes the result file after reading the data
            resultsFile <- ""
            plotSize <- min(c(plotSize,nrow(DF)),na.rm = T)
            if(targetTissue != "all"){
              output$table <- renderTable(DF[1:plotSize, c(1,2,3)])
            } else {
              output$table <- renderTable(DF[1:plotSize, c(1,2,3,4)])
            }
            
            DF[,mutsID := paste0(Protein,Mutation,sep="_")]
            pie_table_all <- DF[,.(count=sum(counts)), by = Protein]
            threshold <- min(plotSize, uniqueN(x = pie_table_all, by = "Protein"), 20)
            
            single_protein_flag <- F
            if(length(pie_table_all$Protein) == 1){
              # Just single gene in the search results
              single_protein_flag <- T
              
              tissueFrequency_tmp <-
                t(DF[, lapply(.SD, sum, na.rm = T), .SDcols = seq(5, ncol(DF) - 1)])
              
              tissueFrequency <- data.table(Tissue=dimnames(tissueFrequency_tmp)[[1]],
                                            tissueFrequency_tmp)
              rm(tissueFrequency_tmp)
              setnames(tissueFrequency, "V1", "count")
              tissueFrequency <- tissueFrequency[count != 0, ]
              data.table::setorder(tissueFrequency, -count)
              
              threshold2 <- min(dim(tissueFrequency)[1], 15)
              pie_table <- tissueFrequency[1:threshold2,]
              if (threshold2 < nrow(tissueFrequency)) {
                
                pie_table <-
                  data.table::rbindlist(l = list(pie_table, 
                                                 list("Others", 
                                                      sum(
                    tissueFrequency[(threshold2 + 1):nrow(tissueFrequency), 
                                    "count"], 
                    na.rm = T))))  
              } else {
                pie_table <-
                  data.table::rbindlist(l = list(pie_table, list("Others", 0 )))  
              }
              
              sliceColors <- rep(NA, length(pie_table$Tissue))
              idx <- which(pie_table$Tissue == "Others")
              sliceColors[idx] <- "#c7c7c7"
              sliceColors[-idx] <- viridis::plasma(length(pie_table$Tissue[-idx]),direction = 1)
              names(sliceColors) <- pie_table$Tissue
              
              ggPie_singleProtein <- ggplot(pie_table, aes(x="",
                                             y=count,
                                             fill=reorder(Tissue,-count)))+
                geom_bar(stat="identity", 
                         width=1, 
                         color="white") + 
                theme_void() + 
                scale_fill_manual(values= sliceColors) + 
                theme(legend.position="right",
                      legend.text=element_text(family="serif",
                                               size=12),
                      legend.key.size = unit(0.5, "lines")) + 
                guides(fill = guide_legend(title = paste0("Tissue (",
                                                          unique(pie_table_all$Protein),
                                                          ")"), 
                                           title.position = "top", 
                                           byrow = T, 
                                           nrow = (threshold2+1),
                                           title.theme = element_text(family="serif", 
                                                                      face = "italic", 
                                                                      angle = 0)))+ 
                coord_polar(theta = "y",
                            direction = -1)
            } else {
              setorder(pie_table_all, -count, Protein)
              pie_table <- pie_table_all[1:threshold, ]
              if(threshold < nrow(pie_table_all)){
                pie_table <-
                  data.table::rbindlist(l = list(pie_table, list("Others", sum(
                    pie_table_all[(threshold + 1):nrow(pie_table_all), "count"], na.rm = T
                  ))))  
              } else {
                pie_table <-
                  data.table::rbindlist(l = list(pie_table, list("Others", 0 )))  
              }
              
              rm(pie_table_all);gc()
              sliceColors <- rep(NA, length(pie_table$Protein))
              idx <- which(pie_table$Protein == "Others")
              sliceColors[idx] <- "#c7c7c7"
              sliceColors[-idx] <- viridis::plasma(length(pie_table$Protein[-idx]),direction = 1)
              names(sliceColors) <- pie_table$Protein
            }
            
            output$plot <- renderPlot({
              ggPie <- ggplot(pie_table, aes(x="",
                                             y=count,
                                             fill=reorder(Protein,-count)))+
                geom_bar(stat="identity", 
                         width=1, 
                         color="white") + 
                theme_void() + 
                scale_fill_manual(values= sliceColors) + 
                theme(legend.position="right",
                      legend.text=element_text(family="serif",
                                               size=12),
                      legend.key.size = unit(0.5, "lines")) + 
                guides(fill = guide_legend(title = "Proteins", 
                                           title.position = "top", 
                                           byrow = T, 
                                           nrow = (threshold+1),
                                           title.theme = element_text(family="serif", 
                                                                      face = "italic", 
                                                                      angle = 0)))+ 
                coord_polar(theta = "y",
                            direction = -1)
              
              ggBar <- ggplot(data=DF[1:plotSize,], aes(x=reorder(mutsID, -counts),
                                                        y=counts))+
                geom_col(fill="#ff5e19",
                         color=NA)+
                ylab("Number of somatic mutations")+
                xlab(paste0("Mutations (n=", plotSize, ")"))+
                theme_bar_plot+
                scale_y_continuous(limits = c(0, max(DF$counts[1:plotSize])), 
                                   expand = c(0, 0))
              
             if(single_protein_flag){
               gridExtra::grid.arrange(ggBar,
                                       ggPie_singleProtein,
                                       ncol=2,
                                       nrow=1,
                                       widths = c(2, 2))
               
             } else {
               gridExtra::grid.arrange(ggBar,
                                       ggPie,
                                       ncol=2,
                                       nrow=1,
                                       widths = c(2, 2))
               
             } 
            })
          }
        }
      }
  })
    session$onSessionEnded(function() {
        gc()
        stopApp()
      })
}