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
topsize = 100
frq.to <- sort(table(dat$to),decreasing = T)[1:topsize]
frq.from <- sort(table(dat$from),decreasing = T)[1:topsize]
toplist <- union(names(frq.to),names(frq.from)) %>% unique()
dat <- subset(dat,(to %in% toplist) & (from %in% toplist))
head(dat)

boxplot(dat$value)
sort(dat$value)[1:20]
sort(dat$value,decreasing = T)[1:20]
A = 1e+18 ; B = 5e+23
a = 1 ; b=10
dat <- dat %>% subset(.,value>A) %>% subset(.,value<B)
boxplot(dat$value,log = "y")

# normalize value
distnorm <- function(x){
  d = 1+(x-A)*(b-a)/(B-A)
  return(1/d)
}

dat$value2 <- lapply(dat$value, FUN = distnorm) %>% as.numeric() %>% round(.,digits = 4)
boxplot(dat$value2)
qplot(dat$value2,xlim=c(0,1.1))
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### main program                                                            ####

g2 <- graph.data.frame(dat[,2:3],directed = F)
#plot(g2,vertex.size=1,vertex.label=NA)
E(g2)$weight <- dat$value2
g2 <- simplify(g2,remove.multiple = T,edge.attr.comb = list(weight="mean","ignore"))

plot(g2,vertex.size=1,vertex.label=NA,main = paste0("vertices #:",vcount(g2)))

# delete degree 1
g3 <- g2
# g3.deg <- degree(g3) == 1
# while(any(g3.deg == T)){
#   g3 <- delete.vertices(g3,V(g3)[g3.deg])
#   g3.deg <- degree(g3) == 1
# }

# plot(g3,vertex.size=1,vertex.label=NA, main = paste0("vertices #:",vcount(g3)))
# table(degree(g3))


DD = distances(g3, v = V(g3), to = V(g3), mode = c("all"), weights = NULL, algorithm = c("automatic"))
Ind <- DD ==Inf
DD[Ind] <- vcount(g3)
DD <- round(DD,digits = 5)
########## replace filename: top150  ############
write.table(DD,file=paste0(modelName,"_top100Rev2_dist.csv"),sep = ";",row.names = FALSE,col.names = FALSE,eol = ";\n",append = FALSE)
# print(table(DD))
max(dat$value2)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### plot                                                                    ####
plot(g3,vertex.size=1,vertex.label=NA)
qplot(DD[DD<max(DD)])
table(degree(g3))
### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### GUDHI Run                                                               ####

f <- "kyber_top100Rev2_dist.csv"

batFile <- paste0("Model_",modelName,"_top100Rev2.bat")
if (file.exists(batFile)) {
  file.remove(batFile)
}

for (j in 1:6) {
  cmd = paste("rips_distance_matrix_persistence.exe -o",gsub(".csv",paste0("MD",j,".txt"),f))
  op = paste(" -r",j,"-d 4 -p 2 ")
  cmd = paste0(cmd,op,f)
  cat(cmd,sep="",file=batFile,append = T)
  cat("\n",file = batFile,append = T)
}
