# for the shortest path distance
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

#put all GUDHI results in this folder
input_folder = "01_coreDist/RW/"
file_list <- list.files(input_folder,pattern = ".txt")

output_foler = gsub("W/","W_betti/",input_folder)
mainDir <- getwd()
dir.create(file.path(mainDir, output_foler), showWarnings = FALSE)

total_file <- paste0(output_foler,"Summary.txt")
if (file.exists(total_file)) {
  file.remove(total_file)
}


for (f in file_list) {
  tmp <- read.table(paste0(input_folder,f))
  tmp <- tmp[2:4]
  
  #find the unqiue interval, drop the Inf
  intervalSet <- c(as.vector(tmp[,2]),as.vector(tmp[,3])) %>%
    unique() %>% setdiff(.,c(Inf)) %>% sort()
  
  #elpsion from 0 to Inf
  steps <- length(intervalSet) + 1 
  
  betti <- matrix(0,steps,5)
  colnames(betti) <- c("Time",'B0','B1','B2','B3')
  betti[,1] <- c(intervalSet,Inf)
  
  #condense betti
  for (i in 1:nrow(tmp)) {
    targetBettiCol = tmp[i,1] + 2
    birthID = which(betti[,1] == tmp[i,2]) 
    deathID = which(betti[,1] == tmp[i,3])
    
    if (tmp[i,3]==Inf) {
      betti[birthID:deathID,targetBettiCol] <- betti[birthID:deathID,targetBettiCol] + 1
    }else{
      deathID = deathID -1 
      # interval add 1
      betti[birthID:deathID,targetBettiCol] <- betti[birthID:deathID,targetBettiCol] + 1
    }
  }
  
  #output betti
  f_out = gsub(".txt","_betti.txt",f)
  write.fwf(betti,file=paste0(output_foler,f_out),colnames = TRUE, sep = "\t")
  
  # total txt files
  cat(paste0(f,"\n"),file = total_file,append = TRUE)
  write.fwf(betti,file = total_file,colnames = TRUE,append = TRUE,sep = "\t")
  cat("\n",file = total_file,append = TRUE)
  
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### plot betti                                                              ####





################ bottleneck distance ################


##### Ref #####
folder <- "01_coreDist/network/"
network.f <- paste0(folder,"kyber_top100Rev_dist.txt")
network.betti <- read.table(network.f)[2:4] %>% as.matrix()

##### RW Sample ####

folder <- "01_coreDist/RW/"
fileList <- list.files(folder,".txt")

dat <- c()
pat <- "(St)\\w+"
typeList <- c()


for (f in fileList) {
  
  betti.f <- read.table(paste0(folder,f))[2:4] %>% as.matrix()
  
  bk0 <- bottleneck(network.betti, betti.f,dimension = 0)
  bk1 <- bottleneck(network.betti, betti.f,dimension = 1)
  bk2 <- bottleneck(network.betti, betti.f,dimension = 2)
  bk3 <- bottleneck(network.betti, betti.f,dimension = 3)
  bk.type <- str_extract(f,pat)
  typeList <- c(typeList,bk.type)
  dat <- rbind(dat,c(bk0,bk1,bk2,bk3,bk.type))
  
}



# typeList <- unique(typeList)
# typeList <- c(typeList[4:length(typeList)],typeList[1:3])
bk <- data.frame(dat)
names(bk) <- c("BK0","BK1","BK2","BK3","TYPE")
bk$BK0 <- as.double(as.character(bk$BK0))
bk$BK1 <- as.double(as.character(bk$BK1))
bk$BK2 <- as.double(as.character(bk$BK2))
bk$BK3 <- as.double(as.character(bk$BK3))

bk


typeList <- c("Step2","Step3","Step4","Step5")
bk$TYPE <- factor(bk$TYPE,levels=typeList)
bk.avg<-ddply(bk, .(TYPE), summarize, BK0avg=mean(BK0),BK1avg = mean(BK1),BK2avg = mean(BK2),BK3avg = mean(BK3))
bk.std<-ddply(bk, .(TYPE), summarize, BK0std=sd(BK0),BK1std = sd(BK1),BK2std = sd(BK2),BK3std = sd(BK3))

bk.m <- melt(bk, id.vars = "TYPE")
ggplot(bk.m)
ggplot(bk.m,aes(TYPE,value,colour = variable)) +geom_point() + ylim(0,2) + geom_text(aes(label = value),vjust =-0.5,hjust=-0.5, size = 4)

