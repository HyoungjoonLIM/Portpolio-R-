

#------------------------------------------------------------------------
#
#   Preprocessing & analysis of Long-type OD
# 
#   2019-08-22
#
#------------------------------------------------------------------------

library(NetworkToolbox)
library(sqldf)
library(dplyr)
library(plyr)
library(data.table)
library(igraph)


setwd("E:/유동인구 분석/OD/190820_서울세종/processed data & results/div")

od0418_00 <- read.csv("od0418_00.csv", header=F, fileEncoding = "utf-8")
od0418_00_o <- od0418_00

colnames(od0418_00) <- c("day", "time", "org", "dest", "gender", "age", "pop")
od0418_00 <- sqldf("SELECT * FROM od0418_00 WHERE org != dest")
od0418_00 <- od0418_00[,-c(5,6)]

od0418_00 <- ddply(od0418_00,~day+time+org+dest,summarise,pop=sum(pop))
od0418_00$day <- NULL
od0418_00$time <- NULL

g0418_00 <- graph_from_data_frame(od0418_00, directed=TRUE) # make a graph form
E(g0418_00)$weight <- od0418_00$pop

N_0418_00 <- length(V(g0418_00))
L_0418_00 <- length(E(g0418_00))

setwd("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")

# 1) node degree
node_degree_out <- count(od0418_00$org)
node_degree_in <- count(od0418_00$dest)

node_degree_0418_00 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                             WHERE node_degree_in.x = node_degree_out.x")
node_degree_0418_00$node_degree <- rowSums(node_degree_0418_00[,c(2,4)])
node_degree_0418_00 <- node_degree_0418_00[,c(1,5)]
write.csv(node_degree_0418_00, "node_degree_0418_00.csv", row.names = F)

# 2) node flux
od0418_00 <- data.table(od0418_00)
node_flux_out_0418_00 <- od0418_00[, sum(pop), by="org"]
node_flux_in_0418_00 <- od0418_00[, sum(pop), by="dest"]

colnames(node_flux_out_0418_00)[1] <- "areaIndex"
colnames(node_flux_in_0418_00)[1] <- "areaIndex"

node_flux_0418_00 <- sqldf("SELECT * FROM node_flux_in_0418_00, node_flux_out_0418_00
                           WHERE node_flux_in_0418_00.areaIndex = node_flux_out_0418_00.areaIndex")
node_flux_0418_00$node_flux <- rowSums(node_flux_0418_00[c(2,4)]) 
node_flux_0418_00 <- node_flux_0418_00[,-c(2:4)]
write.csv(node_flux_0418_00, "node_flux_0418_00.csv", row.names = F)


# 3) coefficient of variation of k, F, w
CV_F_0418_00 <- sd(node_flux_0418_00$node_flux) / mean(node_flux_0418_00$node_flux)
CV_w_0418_00 <- sd(od0418_00$pop) / mean(od0418_00$pop)

write.csv(c(N_0418_00, L_0418_00, CV_F_0418_00, CV_w_0418_00), "CV_0418_00.csv", row.names = F)


# 5) link weight
topN <- 100
link_0418_00 <- od0418_00[c(order(-od0418_00$pop)),]
link_0418_00 <- link_0418_00[c(1:topN), c(3:5)]
write.csv(link_0418_00, "link_0418_00_top100.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0418_00), transitivity(g0418_00, type="local"), transitivity(g0418_00)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "clustering_coeff_0418_00.csv")

rm(list=ls())

#############################################################





setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/div")

od0418 <- read.csv("od0418.csv", header=F, fileEncoding = "utf-8")
od0418_o <- od0418

colnames(od0418) <- c("day", "org", "dest", "gender", "age", "pop")
od0418 <- sqldf("SELECT * FROM od0418 WHERE org != dest")
od0418 <- od0418[,-c(4,5)]

od0418 <- ddply(od0418,~day+org+dest,summarise,pop=sum(pop))
mean(od0418$pop)
od0418$day <- NULL
g0418 <- graph_from_data_frame(od0418, directed=TRUE) # make a graph form
E(g0418)$weight <- od0418$pop

N_0418 <- length(V(g0418))
L_0418 <- length(E(g0418))

setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/stat")

# 1) node degree
node_degree_out <- count(od0418$org)
node_degree_in <- count(od0418$dest)

node_degree_0418 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                          WHERE node_degree_in.x = node_degree_out.x")
node_degree_0418$node_degree <- rowSums(node_degree_0418[,c(2,4)])
node_degree_0418 <- node_degree_0418[,c(1,5)]
write.csv(node_degree_0418, "node_degree_0418.csv", row.names = F)

# 2) node flux
od0418 <- data.table(od0418)
node_flux_out_0418 <- od0418[, sum(pop), by="org"]
node_flux_in_0418 <- od0418[, sum(pop), by="dest"]

colnames(node_flux_out_0418)[1] <- "areaIndex"
colnames(node_flux_in_0418)[1] <- "areaIndex"

node_flux_0418 <- sqldf("SELECT * FROM node_flux_in_0418, node_flux_out_0418
                        WHERE node_flux_in_0418.areaIndex = node_flux_out_0418.areaIndex")
node_flux_0418$node_flux <- rowSums(node_flux_0418[c(2,4)]) 
node_flux_0418 <- node_flux_0418[,-c(2:4)]
write.csv(node_flux_0418, "node_flux_0418.csv", row.names = F)


# 3) coefficient of variation of k, F, w
CV_F_0418 <- sd(node_flux_0418$node_flux) / mean(node_flux_0418$node_flux)
CV_w_0418 <- sd(od0418$pop) / mean(od0418$pop)

write.csv(c(N_0418, L_0418, CV_F_0418, CV_w_0418), "CV_0418.csv", row.names = F)


# 5) link weight
topN <- 100
link_0418 <- od0418[c(order(-od0418$pop)),]
link_0418 <- link_0418[c(1:topN), c(2:4)]
write.csv(link_0418, "link_0418_top100.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0418), transitivity(g0418, type="local"), transitivity(g0418)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "clustering_coeff_0418.csv")


rm(list=ls())





#############################################################
# igraph only for clustering coefficient
setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/div")

od0418_00 <- read.csv("od0418_00.csv", header=F, fileEncoding = "utf-8")
od0418_00_o <- od0418_00

colnames(od0418_00) <- c("day", "time", "org", "dest", "gender", "age", "pop")
od0418_00 <- sqldf("SELECT * FROM od0418_00 WHERE org != dest")
od0418_00 <- od0418_00[,-c(5,6)]

od0418_00 <- ddply(od0418_00,~day+time+org+dest,summarise,pop=sum(pop))
od0418_00$day <- NULL
od0418_00$time <- NULL
g0418_00 <- graph_from_data_frame(od0418_00, directed=TRUE) # make a graph form
E(g0418_00)$weight <- od0418_00$pop

N_0418_00 <- length(V(g0418_00))
L_0418_00 <- length(E(g0418_00))

setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/stat")

# 2) node flux
od0418_00 <- data.table(od0418_00)
node_flux_out_0418_00 <- od0418_00[, sum(pop), by="org"]
node_flux_in_0418_00 <- od0418_00[, sum(pop), by="dest"]

colnames(node_flux_out_0418_00)[1] <- "areaIndex"
colnames(node_flux_in_0418_00)[1] <- "areaIndex"

node_flux_0418_00 <- sqldf("SELECT * FROM node_flux_in_0418_00, node_flux_out_0418_00
                           WHERE node_flux_in_0418_00.areaIndex = node_flux_out_0418_00.areaIndex")
node_flux_0418_00$node_flux <- rowSums(node_flux_0418_00[c(2,4)]) 


# 3) coefficient of variation of k, F, w
CV_F_0418_00 <- sd(node_flux_0418_00$node_flux) / mean(node_flux_0418_00$node_flux)
CV_w_0418_00 <- sd(od0418_00$pop) / mean(od0418_00$pop)

write.csv(c(N_0418_00, L_0418_00, CV_F_0418_00, CV_w_0418_00), "CV_0418_00.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0418_00), transitivity(g0418_00, type="local"), transitivity(g0418_00)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "clustering_coeff_0418_00.csv")

rm(list=ls())

#############################################################





setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/div")

od0418 <- read.csv("od0418.csv", header=F, fileEncoding = "utf-8")
od0418_o <- od0418

colnames(od0418) <- c("day", "org", "dest", "gender", "age", "pop")
od0418 <- sqldf("SELECT * FROM od0418 WHERE org != dest")
od0418 <- od0418[,-c(4,5)]

od0418 <- ddply(od0418,~day+org+dest,summarise,pop=sum(pop))
od0418$day <- NULL
g0418 <- graph_from_data_frame(od0418, directed=TRUE) # make a graph form
E(g0418)$weight <- od0418$pop

N_0418 <- length(V(g0418))
L_0418 <- length(E(g0418))

setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/stat")

# 2) node flux
od0418 <- data.table(od0418)
node_flux_out_0418 <- od0418[, sum(pop), by="org"]
node_flux_in_0418 <- od0418[, sum(pop), by="dest"]

colnames(node_flux_out_0418)[1] <- "areaIndex"
colnames(node_flux_in_0418)[1] <- "areaIndex"

node_flux_0418 <- sqldf("SELECT * FROM node_flux_in_0418, node_flux_out_0418
                        WHERE node_flux_in_0418.areaIndex = node_flux_out_0418.areaIndex")
node_flux_0418$node_flux <- rowSums(node_flux_0418[c(2,4)]) 

# 3) coefficient of variation of k, F, w
CV_F_0418 <- sd(node_flux_0418$node_flux) / mean(node_flux_0418$node_flux)
CV_w_0418 <- sd(od0418$pop) / mean(od0418$pop)

write.csv(c(N_0418, L_0418, CV_F_0418, CV_w_0418), "CV_0418.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0418), transitivity(g0418, type="local"), transitivity(g0418)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "clustering_coeff_0418.csv")

rm(list=ls())

setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/div")

od0421 <- read.csv("od0421.csv", header=F, fileEncoding = "utf-8")
od0421_o <- od0421

colnames(od0421) <- c("day", "org", "dest", "gender", "age", "pop")
od0421 <- sqldf("SELECT * FROM od0421 WHERE org != dest")
od0421 <- od0421[,-c(4,5)]

od0421 <- ddply(od0421,~day+org+dest,summarise,pop=sum(pop))
od0418$day <- NULL

g0421 <- graph_from_data_frame(od0421, directed=TRUE) # make a graph form
E(g0421)$weight <- od0421$pop

N_0421 <- length(V(g0421))
L_0421 <- length(E(g0421))

setwd("E:/?쑀?룞?씤援? 遺꾩꽍/OD/190820_?꽌?슱?꽭醫?/processed data & results/stat")

# 2) node flux
od0421 <- data.table(od0421)
node_flux_out_0421 <- od0421[, sum(pop), by="org"]
node_flux_in_0421 <- od0421[, sum(pop), by="dest"]

colnames(node_flux_out_0421)[1] <- "areaIndex"
colnames(node_flux_in_0421)[1] <- "areaIndex"

node_flux_0421 <- sqldf("SELECT * FROM node_flux_in_0421, node_flux_out_0421
                        WHERE node_flux_in_0421.areaIndex = node_flux_out_0421.areaIndex")
node_flux_0421$node_flux <- rowSums(node_flux_0421[c(2,4)]) 

# 3) coefficient of variation of k, F, w
CV_F_0421 <- sd(node_flux_0421$node_flux) / mean(node_flux_0421$node_flux)
CV_w_0421 <- sd(od0421$pop) / mean(od0421$pop)

write.csv(c(N_0421, L_0421, CV_F_0421, CV_w_0421), "CV_0421.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0421), transitivity(g0421, type="local"), transitivity(g0421)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "clustering_coeff_0421.csv")

rm(list=ls())



################################################
# reading and make table
dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")
setwd(dir)
flist <- list.files(dir)

deglist <- flist[151:200]
fluxlist <- flist[201:250]

all_table <- data.frame()

clulist <- flist[1:50]
clulist <- clulist[-c(1,26)]
cvlist <- flist[51:100]
cvlist <- cvlist[-c(1,26)]

deglist <- flist[151:200]
deglist <- deglist[-c(1,26)]

fluxlist <- flist[201:250]
fluxlist <- fluxlist[-c(1,26)]


for (i in 1:length(clulist)){
  temp <- read.csv(clulist[i], header=T)
  name <- c(substr(clulist[i], 18, 24))
  
  clu_coeff <- mean(temp$local, na.rm = TRUE)
  global_coeff <- temp[1,3]
  all_table[i,1] <- name
  all_table[i,2] <- clu_coeff
  all_table[i,3] <- global_coeff
  
  temp2 <- read.csv(cvlist[i], header=T)
  all_table[i,4] <- temp2[1,1]
  all_table[i,5] <- temp2[2,1]
  all_table[i,6] <- temp2[3,1]
  all_table[i,7] <- temp2[4,1]
  
  temp3 <- read.csv(deglist[i], header=T)
  deg <- mean(temp3$node_degree, na.rm = TRUE)
  all_table[i,8] <- deg
  
  temp4 <- read.csv(fluxlist[i], header=T)
  flux <- mean(temp4$node_flux, na.rm = TRUE)
  all_table[i,9] <- flux
  
  print(i)
}

colnames(all_table) <- c("name", "local", "global", "N", "L"
                         , "CV(F)", "CV(w)", "average_node_degree", "average_node_flux")
all_table <- all_table[,c(1,4,5,8,9,6,7,2,3)]

dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/div")
setwd(dir)
flist <- list.files(dir)
flist <- flist[-c(1,26)]

for (i in 1:length(flist)){
  temp <- read.csv(flist[i], header=F, fileEncoding = "utf-8")
  flux <- mean(temp$V7, na.rm = TRUE)
  all_table[i,10] <- flux
  print(i)
}
colnames(all_table)[10] <- "average_link_weight"

setwd("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")

write.csv(all_table, "stat_hourly.csv", row.names = F)


###############hd#####################



setwd("E:/유동인구 분석/OD/190820_서울세종/rawdata/ODsample/Seoul")

od <- read.csv("c_Seoul_hd_OD.csv", fileEncoding = "cp949")
od0418 <- od[od$STD_YMD==20180418,]
od0421 <- od[od$STD_YMD==20180421,]
rm(od)

dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")
setwd(dir)

od0418_o <- od0418

od0418 <- od0418[,c(2,5,8:11)]
colnames(od0418) <- c("day", "org", "dest", "gender", "age", "pop")
od0418 <- sqldf("SELECT * FROM od0418 WHERE org != dest")
od0418 <- od0418[,-c(4,5)]

od0418 <- ddply(od0418,~day+org+dest,summarise,pop=sum(pop))
mean(od0418$pop)
od0418$day <- NULL
g0418 <- graph_from_data_frame(od0418, directed=TRUE) # make a graph form
E(g0418)$weight <- od0418$pop

N_0418 <- length(V(g0418))
L_0418 <- length(E(g0418))


# 1) node degree
node_degree_out <- count(od0418$org)
node_degree_in <- count(od0418$dest)

node_degree_0418 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                          WHERE node_degree_in.x = node_degree_out.x")
node_degree_0418$node_degree <- rowSums(node_degree_0418[,c(2,4)])
node_degree_0418 <- node_degree_0418[,c(1,5)]
write.csv(node_degree_0418, "hd_node_degree_0418.csv", row.names = F)

# 2) node flux
od0418 <- data.table(od0418)
node_flux_out_0418 <- od0418[, sum(pop), by="org"]
node_flux_in_0418 <- od0418[, sum(pop), by="dest"]

colnames(node_flux_out_0418)[1] <- "areaIndex"
colnames(node_flux_in_0418)[1] <- "areaIndex"

node_flux_0418 <- sqldf("SELECT * FROM node_flux_in_0418, node_flux_out_0418
                        WHERE node_flux_in_0418.areaIndex = node_flux_out_0418.areaIndex")
node_flux_0418$node_flux <- rowSums(node_flux_0418[c(2,4)]) 
node_flux_0418 <- node_flux_0418[,-c(2:4)]
write.csv(node_flux_0418, "hd_node_flux_0418.csv", row.names = F)


# 3) coefficient of variation of k, F, w
CV_F_0418 <- sd(node_flux_0418$node_flux) / mean(node_flux_0418$node_flux)
CV_w_0418 <- sd(od0418$pop) / mean(od0418$pop)

write.csv(c(N_0418, L_0418, CV_F_0418, CV_w_0418), "hd_CV_0418.csv", row.names = F)


# 5) link weight
topN <- 40
link_0418 <- od0418[c(order(-od0418$pop)),]
link_0418 <- link_0418[c(1:topN),]
write.csv(link_0418, "hd_link_0418_top40.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0418), transitivity(g0418, type="local"), transitivity(g0418)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "hd_clustering_coeff_0418.csv")




od0421_o <- od0421

od0421 <- od0421[,c(2,5,8:11)]
colnames(od0421) <- c("day", "org", "dest", "gender", "age", "pop")
od0421 <- sqldf("SELECT * FROM od0421 WHERE org != dest")
od0421 <- od0421[,-c(4,5)]

od0421 <- ddply(od0421,~day+org+dest,summarise,pop=sum(pop))
mean(od0421$pop)
od0421$day <- NULL
g0421 <- graph_from_data_frame(od0421, directed=TRUE) # make a graph form
E(g0421)$weight <- od0421$pop

N_0421 <- length(V(g0421))
L_0421 <- length(E(g0421))


# 1) node degree
node_degree_out <- count(od0421$org)
node_degree_in <- count(od0421$dest)

node_degree_0421 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                          WHERE node_degree_in.x = node_degree_out.x")
node_degree_0421$node_degree <- rowSums(node_degree_0421[,c(2,4)])
node_degree_0421 <- node_degree_0421[,c(1,5)]
write.csv(node_degree_0421, "hd_node_degree_0421.csv", row.names = F)

# 2) node flux
od0421 <- data.table(od0421)
node_flux_out_0421 <- od0421[, sum(pop), by="org"]
node_flux_in_0421 <- od0421[, sum(pop), by="dest"]

colnames(node_flux_out_0421)[1] <- "areaIndex"
colnames(node_flux_in_0421)[1] <- "areaIndex"

node_flux_0421 <- sqldf("SELECT * FROM node_flux_in_0421, node_flux_out_0421
                        WHERE node_flux_in_0421.areaIndex = node_flux_out_0421.areaIndex")
node_flux_0421$node_flux <- rowSums(node_flux_0421[c(2,4)]) 
node_flux_0421 <- node_flux_0421[,-c(2:4)]
write.csv(node_flux_0421, "hd_node_flux_0421.csv", row.names = F)


# 3) coefficient of variation of k, F, w
CV_F_0421 <- sd(node_flux_0421$node_flux) / mean(node_flux_0421$node_flux)
CV_w_0421 <- sd(od0421$pop) / mean(od0421$pop)

write.csv(c(N_0421, L_0421, CV_F_0421, CV_w_0421), "hd_CV_0421.csv", row.names = F)


# 5) link weight
topN <- 40
link_0421 <- od0421[c(order(-od0421$pop)),]
link_0421 <- link_0421[c(1:topN),]
write.csv(link_0421, "hd_link_0421_top40.csv", row.names = F)

## clustering coefficient

clustering_coeff <- data.frame(cbind(V(g0421), transitivity(g0421, type="local"), transitivity(g0421)))
colnames(clustering_coeff) <- c("no","local", "global")
clustering_coeff$no <- NULL
write.csv(clustering_coeff, "hd_clustering_coeff_0421.csv")






#############by-time(hd)############

setwd("E:/유동인구 분석/OD/190820_서울세종/rawdata/ODsample/Seoul")

od2 <- read.csv("c_Seoul_hd_time_OD.csv", fileEncoding = "cp949")

dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")
setwd(dir)

for (i in 0:23){
  temp <- od2[od2$STD_YMD==20180418 & od2$HH==i,]
  
  temp_o <- temp
  
  temp <- temp[,c(2,6,9:12)]
  colnames(temp) <- c("day", "org", "dest", "gender", "age", "pop")
  temp <- sqldf("SELECT * FROM temp WHERE org != dest")
  temp <- temp[,-c(4,5)]
  
  temp <- ddply(temp,~day+org+dest,summarise,pop=sum(pop))
  mean(temp$pop)
  temp$day <- NULL
  g0418 <- graph_from_data_frame(temp, directed=TRUE) # make a graph form
  E(g0418)$weight <- temp$pop
  
  N_0418 <- length(V(g0418))
  L_0418 <- length(E(g0418))
  
  
  # 1) node degree
  node_degree_out <- count(temp$org)
  node_degree_in <- count(temp$dest)
  
  node_degree_0418 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                            WHERE node_degree_in.x = node_degree_out.x")
  node_degree_0418$node_degree <- rowSums(node_degree_0418[,c(2,4)])
  node_degree_0418 <- node_degree_0418[,c(1,5)]
  write.csv(node_degree_0418, paste0("hd_node_degree_0418_",i,".csv"), row.names = F)
  
  # 2) node flux
  temp <- data.table(temp)
  node_flux_out_0418 <- temp[, sum(pop), by="org"]
  node_flux_in_0418 <- temp[, sum(pop), by="dest"]
  
  colnames(node_flux_out_0418)[1] <- "areaIndex"
  colnames(node_flux_in_0418)[1] <- "areaIndex"
  
  node_flux_0418 <- sqldf("SELECT * FROM node_flux_in_0418, node_flux_out_0418
                          WHERE node_flux_in_0418.areaIndex = node_flux_out_0418.areaIndex")
  node_flux_0418$node_flux <- rowSums(node_flux_0418[c(2,4)]) 
  node_flux_0418 <- node_flux_0418[,-c(2:4)]
  write.csv(node_flux_0418, paste0("hd_node_flux_0418_",i,".csv"), row.names = F)
  
  
  # 3) coefficient of variation of k, F, w
  CV_F_0418 <- sd(node_flux_0418$node_flux) / mean(node_flux_0418$node_flux)
  CV_w_0418 <- sd(temp$pop) / mean(temp$pop)
  
  write.csv(c(N_0418, L_0418, CV_F_0418, CV_w_0418), paste0("hd_CV_0418_",i,".csv"), row.names = F)
  
  
  # 5) link weight
  topN <- 40
  link_0418 <- temp[c(order(-temp$pop)),]
  link_0418 <- link_0418[c(1:topN),]
  write.csv(link_0418, paste0("hd_link_0418_top40_",i,".csv"), row.names = F)
  
  ## clustering coefficient
  
  clustering_coeff <- data.frame(cbind(V(g0418), transitivity(g0418, type="local"), transitivity(g0418)))
  colnames(clustering_coeff) <- c("no","local", "global")
  clustering_coeff$no <- NULL
  write.csv(clustering_coeff, paste0("hd_clustering_coeff_",i,".csv"))
  print(i)
}

for (i in 0:23){
  temp <- od2[od2$STD_YMD==20180421 & od2$HH==i,]
  
  temp_o <- temp
  
  temp <- temp[,c(2,6,9:12)]
  colnames(temp) <- c("day", "org", "dest", "gender", "age", "pop")
  temp <- sqldf("SELECT * FROM temp WHERE org != dest")
  temp <- temp[,-c(4,5)]
  
  temp <- ddply(temp,~day+org+dest,summarise,pop=sum(pop))
  mean(temp$pop)
  temp$day <- NULL
  g0418 <- graph_from_data_frame(temp, directed=TRUE) # make a graph form
  E(g0418)$weight <- temp$pop
  
  N_0418 <- length(V(g0418))
  L_0418 <- length(E(g0418))
  
  
  # 1) node degree
  node_degree_out <- count(temp$org)
  node_degree_in <- count(temp$dest)
  
  node_degree_0418 <- sqldf("SELECT * FROM node_degree_in, node_degree_out
                            WHERE node_degree_in.x = node_degree_out.x")
  node_degree_0418$node_degree <- rowSums(node_degree_0418[,c(2,4)])
  node_degree_0418 <- node_degree_0418[,c(1,5)]
  write.csv(node_degree_0418, paste0("hd_node_degree_0421_",i,".csv"), row.names = F)
  
  # 2) node flux
  temp <- data.table(temp)
  node_flux_out_0418 <- temp[, sum(pop), by="org"]
  node_flux_in_0418 <- temp[, sum(pop), by="dest"]
  
  colnames(node_flux_out_0418)[1] <- "areaIndex"
  colnames(node_flux_in_0418)[1] <- "areaIndex"
  
  node_flux_0418 <- sqldf("SELECT * FROM node_flux_in_0418, node_flux_out_0418
                          WHERE node_flux_in_0418.areaIndex = node_flux_out_0418.areaIndex")
  node_flux_0418$node_flux <- rowSums(node_flux_0418[c(2,4)]) 
  node_flux_0418 <- node_flux_0418[,-c(2:4)]
  write.csv(node_flux_0418, paste0("hd_node_flux_0421_",i,".csv"), row.names = F)
  
  
  # 3) coefficient of variation of k, F, w
  CV_F_0418 <- sd(node_flux_0418$node_flux) / mean(node_flux_0418$node_flux)
  CV_w_0418 <- sd(temp$pop) / mean(temp$pop)
  
  write.csv(c(N_0418, L_0418, CV_F_0418, CV_w_0418), paste0("hd_CV_0421_",i,".csv"), row.names = F)
  
  
  # 5) link weight
  topN <- 40
  link_0418 <- temp[c(order(-temp$pop)),]
  link_0418 <- link_0418[c(1:topN),]
  write.csv(link_0418, paste0("hd_link_0421_top40_",i,".csv"), row.names = F)
  
  ## clustering coefficient
  
  clustering_coeff <- data.frame(cbind(V(g0418), transitivity(g0418, type="local"), transitivity(g0418)))
  colnames(clustering_coeff) <- c("no","local", "global")
  clustering_coeff$no <- NULL
  write.csv(clustering_coeff, paste0("hd_clustering_coeff_0421_",i,".csv"))
  print(i)
}



############time graph##############

dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")
setwd(dir)
flist <- list.files(dir)

deglist <- flist[251:300]
fluxlist <- flist[301:350]

all_table <- data.frame()

clulist <- flist[101:150]
cluslist2 <- clulist[8:31]
clulist1 <- clulist[c(1:5,32:50)]

clulist <- c(clulist1, cluslist2)
cvlist <- flist[151:200]
cvlist <- cvlist[-c(1,26)]

deglist <- flist[151:200]
deglist <- deglist[-c(1,26)]

fluxlist <- flist[201:250]
fluxlist <- fluxlist[-c(1,26)]


for (i in 1:length(clulist)){
  temp <- read.csv(clulist[i], header=T)
  name <- c(substr(clulist[i], 21, 27))
  
  clu_coeff <- mean(temp$local, na.rm = TRUE)
  global_coeff <- temp[1,3]
  all_table[i,1] <- name
  all_table[i,2] <- clu_coeff
  all_table[i,3] <- global_coeff
  
  temp2 <- read.csv(cvlist[i], header=T)
  all_table[i,4] <- temp2[1,1]
  all_table[i,5] <- temp2[2,1]
  all_table[i,6] <- temp2[3,1]
  all_table[i,7] <- temp2[4,1]
  
  temp3 <- read.csv(deglist[i], header=T)
  deg <- mean(temp3$node_degree, na.rm = TRUE)
  all_table[i,8] <- deg
  
  temp4 <- read.csv(fluxlist[i], header=T)
  flux <- mean(temp4$node_flux, na.rm = TRUE)
  all_table[i,9] <- flux
  
  print(i)
}

colnames(all_table) <- c("name", "local", "global", "N", "L"
                         , "CV(F)", "CV(w)", "average_node_degree", "average_node_flux")
all_table <- all_table[,c(1,4,5,8,9,6,7,2,3)]

dir <- c("E:/유동인구 분석/OD/190820_서울세종/processed data & results/div")
setwd(dir)
flist <- list.files(dir)
flist <- flist[-c(1,26)]


flux <- data.frame()
for (i in 0:23){
  temp <- od2[od2$STD_YMD==20180418 & od2$HH==i,]
  
  temp_o <- temp
  
  temp <- temp[,c(2,6,9:12)]
  colnames(temp) <- c("day", "org", "dest", "gender", "age", "pop")
  temp <- sqldf("SELECT * FROM temp WHERE org != dest")
  temp <- temp[,-c(4,5)]
  
  temp <- ddply(temp,~day+org+dest,summarise,pop=sum(pop))
  
  flux <- rbind(flux, mean(temp$pop))
  print(i)
}
for (i in 0:23){
  temp <- od2[od2$STD_YMD==20180421 & od2$HH==i,]
  
  temp_o <- temp
  
  temp <- temp[,c(2,6,9:12)]
  colnames(temp) <- c("day", "org", "dest", "gender", "age", "pop")
  temp <- sqldf("SELECT * FROM temp WHERE org != dest")
  temp <- temp[,-c(4,5)]
  
  temp <- ddply(temp,~day+org+dest,summarise,pop=sum(pop))
  
  flux <- rbind(flux, mean(temp$pop))
  print(i)
}

all_table$average_link_weight <- flux
colnames(all_table)[10] <- "average_link_weight"


setwd("E:/유동인구 분석/OD/190820_서울세종/processed data & results/stat")

write.csv(all_table, "dong_stat_hourly.csv", row.names = F)









