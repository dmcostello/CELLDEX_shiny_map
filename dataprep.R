library(shiny)

#Load in datasets
field <- read.csv("./data/field_clean.csv")
field$part.str <- paste(field$partnerid,field$stream)

k <- read.csv("./data/str_k.csv")
colnames(k)[2] <- "part.str"

str_k <- merge(k,field[,c('biome_short','part.str','latitude','longitude')],by="part.str")

saveRDS(str_k,file="./data/CELLDEX.rds")


#Terminal code for pruning

#git fetch -p
#git branch -d branchname