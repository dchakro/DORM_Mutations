rm(list=ls())

library(data.table)
plot_bar <- function(Tissue){
  # Tissue <- "lung"
  if(Tissue == "gastrointestinal tract (site indeterminate)"){
    Tissue_title <- "GI tract (site indeterminate)"
  }else if(Tissue == "haematopoietic and lymphoid tissue"){
    Tissue_title <- "haematopoietic / lymphoid"
  } else if(Tissue == "NS"){
    Tissue_title <- "Not specified"
  } else {
    Tissue_title <- Tissue
  }
  
  if(Tissue == "all"){
    subSampleDT <- data.table::as.data.table(aggregate(SampleDT$count ~ SampleDT$Gene, FUN = sum))
    colnames(subSampleDT) <- c("Gene.name","count")
    setorder(subSampleDT,-count)
    plot_title <- paste0(Tissue_title," (n = ",sum(sampleCount$count)," samples)")
  } else {
    subSampleDT <- SampleDT[tissue == Tissue ,]
    setorder(subSampleDT,-count)
    subSampleDT[,tissue:=NULL]
    plot_title <- paste0(Tissue_title," (n = ",sampleCount[tissue==Tissue,count]," samples)")
  }
  # if slice weight normalized to total number of mutations in tissue type
  # subSampleDT[, per_mutCount:=count/sum(subSampleDT$count)]
  
  threshold <- 25
  threshold <- min(threshold, uniqueN(subSampleDT))
  subSampleDT <- subSampleDT[1:threshold, ]
  '%nin%'=Negate('%in%')
  
  if(Tissue == "all"){
    tmp <- Stats 
  } else {
    tmp <- Stats[tissue == Tissue,]
  }
  
  samples_accounted_for <- tmp[Gene.name %in% unique(subSampleDT$Gene.name),.(Sample.name)]
  samples_accounted_for <- unique(samples_accounted_for$Sample.name)
  others <- uniqueN(tmp[!Sample.name %in% samples_accounted_for,.(Sample.name)])
  rm(tmp,samples_accounted_for);gc()
  
  subSampleDT <- data.table::rbindlist(l = list(subSampleDT,list("Others",others)))
  if(Tissue != "all"){
    subSampleDT[,per_sample:=count/sampleCount[tissue==Tissue,count]]  
  } else {
    subSampleDT[,per_sample:=count/sum(sampleCount$count)]
  }
  
  pie_table <- subSampleDT[,.(Gene.name,per_sample)]
  setnames(pie_table,"Gene.name","Gene")
  
  sliceColors <- rep(NA, uniqueN(pie_table))
  idx <- which(pie_table$Gene == "Others")
  sliceColors[idx] <- "#a3a3a3"
  sliceColors[-idx] <- viridis::plasma(uniqueN(pie_table)-1, direction = 1)
  names(sliceColors) <- pie_table$Gene
  pie_table$Gene <- factor(pie_table$Gene,levels = pie_table$Gene)
  print(paste(Tissue,round(sum(pie_table$per_sample),digits = 2)))
  
  library(ggplot2)
  bar <- ggplot(pie_table,aes(x=Gene,
                              y=per_sample,
                              fill=Gene))+
    geom_bar(stat="identity", 
             position ="dodge", 
             width=0.75,  
             color=NA) + 
    scale_fill_manual(values = sliceColors) + 
    scale_y_continuous(limits = c(0,1),
                       expand = c(0,0), 
                       labels = paste0(seq(0,100,by=25),"%")) +
    ylab("Mutated in samples (%)") + 
    xlab("Proteins") +
    theme(axis.line = element_line(colour = "black",
                                   size=0.5),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          legend.position="none",
          legend.text=element_text(family="serif",
                                   size = 6),
          title = element_text(family = "serif", 
                               size = rel(1.3), 
                               face = "bold.italic"),
          plot.title = element_text(hjust = 0),
          axis.text.x = element_text(family = "serif",
                                     face = "plain",
                                     angle = 45,
                                     hjust = 1,
                                     vjust = 1,
                                     size = rel(1.2),
                                     color = "black"),
          axis.text.y = element_text(family = "serif",
                                     face = "plain",
                                     size = rel(1.3),
                                     color = "black"),
          axis.title = element_text(family="serif", 
                                    size = rel(1.2), 
                                    face = "italic", 
                                    angle = 0),
          legend.key.size = unit(0.2, "lines")) + 
    ggtitle(plot_title)
  
  return(bar)
}

# ----> Set up environment <-------
setwd("/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/websites/eleniuslabtools.utu.fi/shiny-apps/COSMIC/HotspotMutations/src")
source("https://gist.githubusercontent.com/dchakro/8b1e97ba6853563dd0bb5b7be2317692/raw/parallelRDS.R")

Stats <- readRDS.gz("/Users/deepankar/OneDrive - O365 Turun yliopisto/Klaus lab/Manuscripts/Hotspot Explorer/Data/COSMIC_v94_R_DT/CountStatsRAW.RDS")
rm(loadRDS,readRDS.gz,writeRDS,saveRDS.gz)

sampleCount <- unique(Stats[,.(Sample.name,tissue)])[,.N,.(tissue)]
setnames(sampleCount,"N","count")

SampleDT <- Stats[,.N, .(Gene.name,tissue)]
setnames(SampleDT,c("N"),c("count"))

tissues <- unique(SampleDT$tissue)
# plot_bar("all")
# plot_bar("pancreas")
# plot_bar("thyroid")
# plot_bar("lung")

# obj <- plot_bar("all")
# saveRDS(object = obj,file = "/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/websites/eleniuslabtools.utu.fi/shiny-apps/COSMIC/HotspotMutations/data/bar_with_others/all.RDS")
# ggsave(plot = obj,filename = "/Users/deepankar/OneDrive - O365 Turun yliopisto/Git/GitHub/websites/eleniuslabtools.utu.fi/shiny-apps/COSMIC/HotspotMutations/test.svg",width = 4,height = 4)

var <- parallel::mclapply(
    X = as.list(c("all",sort(tissues))),
    FUN = function(X) saveRDS(object = plot_bar(Tissue = X),file = paste0("../data/bar_with_others/", gsub(" ","_",X), ".RDS")),
    mc.cores = parallel::detectCores()
)
