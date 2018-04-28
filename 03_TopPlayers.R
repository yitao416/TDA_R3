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
### pre data                                                                ####
rm(list=ls())

modelName <- "kyber"

dat <- read.csv(paste0("data/",modelName,".csv"))
#dat <- dat[1:2000,]
head(dat)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Find frequent data                                                      ####
topsize = 150
frq.to <- sort(table(dat$to),decreasing = T)[1:topsize]
frq.from <- sort(table(dat$from),decreasing = T)[1:topsize]

toplist <- union(names(frq.to),names(frq.from)) %>% unique()

dat <- subset(dat,(to %in% toplist) & (from %in% toplist))

head(dat)

boxplot(dat$value)

sort(dat$value)[1:200]
sort(dat$value,decreasing = T)[1:200]
A = 1e+15
B = 1e+23
a = 1;b=10

dat <- dat %>% subset(.,value>A) %>% subset(.,value<B)
boxplot(dat$value)

dat$value2 <- lapply(dat$value, function(x){1+ (x-A)*(b-a)/(B-A)}) %>% as.numeric() %>% round(.,digits = 4)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### main program                                                            ####

g2 <- graph.data.frame(dat[,2:3],directed = F)
#plot(g2,vertex.size=1,vertex.label=NA)
E(g2)$weight <- dat$value2
g2 <- simplify(g2,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))


DD = distances(g2, v = V(g2), to = V(g2), mode = c("all"), weights = NULL, algorithm = c("automatic"))
Ind <- DD ==Inf
DD[Ind] <- vcount(g2)
########## replace filename: top150  ############
write.table(DD,file=paste0(modelName,"_top150_dist.csv"),sep = ";",row.names = FALSE,col.names = FALSE,eol = ";\n",append = FALSE)
# print(table(DD))
max(dat$value2)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### plot                                                                    ####
plot(g2,vertex.size=1,vertex.label=NA)
qplot(DD[DD<11])


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### GUDHI Run                                                               ####

f <- "kyber_top150_dist.csv"

batFile <- paste0("Model_",modelName,"_top150.bat")
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



