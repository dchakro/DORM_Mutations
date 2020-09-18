rm(list=ls())
DF <- readRDS("20200819.FrequencyByMutation.RDS")

Tissue <- sort(unique(gsub(pattern = "^[[:space:]]",
                           replacement = "",
                           x = unique(x = unlist(x = stringi::stri_split_fixed(
                             str = stringi::stri_replace_all_regex(
                               str = DF$Frequency,
                               pattern = ":[0-9]+",
                               replacement = "",
                               vectorize_all = T),
                             pattern = ","),
                             use.names = F)
                           )
)
)
)

DF$mutID <- paste0(DF$Gene,"_",DF$Mutation)

mat <- matrix(data = NA,nrow = nrow(DF),ncol = length(Tissue),dimnames = list(DF$mutID,Tissue))

pb <- progress::progress_bar$new(format = ":current mutation [:bar] :percent", 
                                 total = nrow(DF), 
                                 clear = FALSE, 
                                 width= 50)

for(i in seq_along(DF$Gene)){
  counts <- stringi::stri_split_fixed(str = gsub(pattern = "^[[:space:]]",
                                       replacement = "",
                                       x = unlist(stringi::stri_split_fixed(
                                         str = DF$Frequency[i],
                                         pattern = ","),
                                         use.names = F)),
                            pattern = ":",
                            simplify = T)
  mat[i,counts[,1]] <- as.integer(counts[,2])
  if(i %% 1000 == 0) pb$tick(1000)
}
rm(pb,i,counts)

DF <- cbind(DF,mat)
rm(mat)

saveRDS(DF,file = "/Users/deepankar/OneDrive - O365 Turun yliopisto/ExtraWorkSync/Klaus-Lab-Data/Big Data/COSMIC/v91/Full_Database/20200918.COSMIC_Mutations_byTissue.mat.RDS",compress = T)


