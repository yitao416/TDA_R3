rm(list=ls())
library(igraph)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)
library(Matrix)
library(plyr)
library(gdata)
library(TDA)
library(plyr)
library(stringr)
library(dplyr)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### test                                                                    ####

dtest <- read.csv("data/test.csv")

gtest <- graph.data.frame(dtest,directed = F)
plot(gtest)
E(gtest)$weight <- dtest[,3]
DD = distances(gtest, v = V(gtest), to = V(gtest), mode = c("all"), weights = NULL, algorithm = c("automatic"))
DD

gtest <- simplify(gtest,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))
plot(gtest)

DD = distances(gtest, v = V(gtest), to = V(gtest), mode = c("all"), weights = NULL, algorithm = c("automatic"))
DD


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### pre data                                                                ####
rm(list=ls())

modelName <- "kyber"

dat <- read.csv(paste0("data/",modelName,".csv"))
dat <- dat[1:2000,]
head(dat)
#sort(dat$value)[1:100]
#sort(dat$value,decreasing = T)[1:100]
A = 1e+15
B = 0.5e+23
a = 1;b=10

dat <- dat %>% subset(.,value>A) %>% subset(.,value<B)
dat$value2 <- lapply(dat$value, function(x){1+ (x-A)*(b-a)/(B-A)}) %>% as.numeric() %>% round(.,digits = 4)
head(dat)
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### main program                                                            ####

g2 <- graph.data.frame(dat[,2:3],directed = F)
#plot(g2,vertex.size=1,vertex.label=NA)
E(g2)$weight <- dat$value2
g2 <- simplify(g2,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))


DD = distances(g2, v = V(g2), to = V(g2), mode = c("all"), weights = NULL, algorithm = c("automatic"))
Ind <- DD ==Inf
DD[Ind] <- vcount(g2)
write.table(DD,file=paste0(modelName,"_dist.csv"),sep = ";",row.names = FALSE,col.names = FALSE,eol = ";\n",append = FALSE)
# print(table(DD))
max(dat$value2)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### plot                                                                    ####
plot(g2,vertex.size=1,vertex.label=NA)
qplot(DD[DD<11])


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### GUDHI Run                                                               ####

f <- "kyber_dist.csv"

batFile <- paste0("Model_",modelName,".bat")
if (file.exists(batFile)) {
  file.remove(batFile)
}

for (j in 2:6) {
  
  cmd = paste("rips_distance_matrix_persistence.exe -o",gsub(".csv",paste0("MD",j,".txt"),f))
  op = paste(" -r",j,"-d 4 -p 2 ")
  cmd = paste0(cmd,op,f)
  cat(cmd,sep="",file=batFile,append = T)
  cat("\npause\n",file = batFile,append = T )
}



