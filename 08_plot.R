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


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### plot betti                                                              ####
input_folder = "01_coreDist/network/"
# file_list <- list.files(input_folder,pattern = ".txt")
f.name = "kyber_top100Rev_dist.txt"
network.f <- paste0(input_folder,f.name)
network.betti <- read.table(network.f)[2:4] %>% as.matrix()

#### barcode plot ######
par(mfrow=c(1,2))
plot.diagram(network.betti)
plot.diagram(network.betti,barcode = T)
title("top 100 coreDist")

#### betti plot #######
theme_update(plot.title = element_text(hjust = 0.5,size=20))
par(mfrow=c(1,1))
folder <- "01_coreDist/network_betti/"
f <- "kyber_top100Rev_dist_betti.txt"
result <- read.table(paste0(folder,f),header = T)
# For each type
CM <- melt(result,id.vars = "Time", measure.vars = c("B0","B1","B2","B3"))
ggplot(CM, aes(x=Time, y=value, color=variable)) +scale_color_manual(values=c("black", "red", "blue","grey")) +
  geom_point()+geom_line() +geom_text(aes(label = value),vjust =-0.5,hjust=-0.5, size = 3) +
  ggtitle("top 100")






################  check with the original network only b0 is diff #########
# ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
# ### plot betti                                                              ####
# input_folder = "02_wholeDist/network/"
# # file_list <- list.files(input_folder,pattern = ".txt")
# f.name = "kyber_top100Rev2_distMD5.txt"
# network.f <- paste0(input_folder,f.name)
# network.betti <- read.table(network.f)[2:4] %>% as.matrix()
# 
# #### barcode plot ######
# par(mfrow=c(1,2))
# plot.diagram(network.betti)
# plot.diagram(network.betti,barcode = T)
# title("top 100 Dist")
# 
# #### betti plot #######
# theme_update(plot.title = element_text(hjust = 0.5,size=20))
# par(mfrow=c(1,1))
# folder <- "02_wholeDist/network_betti/"
# f <- "kyber_top100Rev2_distMD5_betti.txt"
# result <- read.table(paste0(folder,f),header = T)
# # For each type
# CM <- melt(result,id.vars = "Time", measure.vars = c("B0","B1","B2","B3"))
# ggplot(CM, aes(x=Time, y=value, color=variable)) +scale_color_manual(values=c("black", "red", "blue","grey")) +
#   geom_point()+geom_line() +geom_text(aes(label = value),vjust =-0.5,hjust=-0.5, size = 3) +
#   ggtitle("top 100 no cut")

