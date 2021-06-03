rm(list=ls())
# -- Create essential files (initial run)
DF <- readRDS("./data/20210603.FrequencyMutations.byTissue.RDS")
write(colnames(DF),file = "./data/ColumnNames.txt",ncolumns = 1)
write.table(x = DF,file = "./data/RAW_DATA.csv",col.names = F,row.names = F,sep = ";",quote = F)

tissues <- readLines("./data/ColumnNames.txt",warn = F)
'%nin%' <- Negate('%in%')
tissues <- tissues[tissues %nin% c("Gene","Mutation","counts","Frequency")]

dir.create("./data/tissue")
file.copy("./data/RAW_DATA.csv","./data/tissue/all.csv")
for(tissue in tissues){
  sub <- na.omit(DF[,c(1,2,which(colnames(DF) == tissue))])
  sub <- sub[order(sub[,3],decreasing = T),]
  write.table(x = sub,
              file = paste0("./data/tissue/",tissue,".csv"),
              col.names = F,
              row.names = F,
              sep = ";",
              quote = F)
}

message("Remember to create ./tmp directory")
message("Then, run sudo chown -R shiny:shiny tmp ")
