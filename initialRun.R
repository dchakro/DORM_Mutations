library(data.table)
rm(list=ls());gc()
# -- Create essential files (initial run)
DF <- readRDS("./data/20210709.FrequencyByMutation.RDS")
DF <- DF[, replace(.SD, .SD == 0, NA)]
write(colnames(DF),file = "./data/ColumnNames.txt",ncolumns = 1)
message("Writing RDS as CSV")
write.table(x = DF,file = "./data/RAW_DATA.csv",col.names = F,row.names = F,sep = ";",quote = F)

tissues <- readLines("./data/ColumnNames.txt",warn = F)
'%nin%' <- Negate('%in%')
tissues <- tissues[tissues %nin% c("Gene","Mutation","counts","Frequency")]

dir.create("./tmp")
dir.create("./data/tissue")
file.copy("./data/RAW_DATA.csv","./data/tissue/all.csv")

writeTissueCSV <- function(tissue){
  idx <- c(1,2, which(colnames(DF) == tissue))
  subDF <- data.table:::na.omit.data.table(DF[,..idx])
  setorderv(subDF, c(colnames(subDF)[3],"Gene","Mutation"), order = c(-1,1,1)) 
  outfile <- paste0("./data/tissue/",tissue,".csv")
  message(paste("Writing",outfile))
  write.table(x = subDF,
              file = outfile,
              col.names = F,
              row.names = F,
              sep = ";",
              quote = F)
  return(NULL)
}

invisible(lapply(X = tissues,FUN = function(x) writeTissueCSV(x)))

message("Done!")
message("Note: Run  sudo chown -R shiny:shiny tmp ")
