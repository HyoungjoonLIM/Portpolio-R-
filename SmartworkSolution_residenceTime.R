#----------------------------------------------------------------------------------------------------------------------------
# Dec 2020, Hyoungjoon Lim
# Using the log data of the smart work solution, the residence time for each menu is calculated. 
# Knowing how long employees stay when using which menus, and from which menus they stop using and exit.
#----------------------------------------------------------------------------------------------------------------------------


install.packages('dplyr') 
library(dplyr) 
library(sqldf)
install.packages("dtw")
library(dtw)
install.packages("cluster")
library(cluster)

setwd("D:/Desktop/rawdata")

data_o <- read.csv("raw_20201207.csv")

date <- c("19-12","20-01","20-02","20-03","20-10","20-11",
          "20-04","20-05","20-06","20-07","20-08","20-09")

data <- subset(data_o, substr(data_o$활동일자,1,7)==paste0("20",date[k]))
data <- subset(data, data$활동유형!="기타")

list <- unique(data$활동등록자사원번호)

data$label <- 2 # reply
data[data$activity=="change status of job card",]$label <- 4
data[data$활동유형=="add job card",]$label <- 3
data[data$활동유형=="login",]$label <- 1
data[data$활동유형=="instruct",]$label <- 5

#summary(as.factor(data$label))

new_list <- list

DIST <- data.frame()

for (i in 1:(length(new_list)-1)){
  print(i)
  id_q <- new_list[i]
  seq_q <- subset(data, data$ID == id_q) # extract query index
  traj_q <- array(seq_q$label)
  
  for (j in (i+1):length(new_list)){
    id_r <- new_list[j]
    seq_r <- subset(data, data$ID == id_r) # extract query index
    
    traj_r <- array(seq_r$label)
    
    dtw_temp <- dtw(traj_q, traj_r, keep = TRUE) # evaluating distance by DTW
    DIST <- rbind(DIST, c(id_q, id_r, dtw_temp$distance))
    
    rm(id_r, seq_r, traj_r, dtw_temp)
  }
}

colnames(DIST) <- c("id_q", "id_r", "dist")

ls1 <- sqldf("SELECT DISTINCT(id_q) FROM DIST")
ls2 <- sqldf("SELECT DISTINCT(id_r) FROM DIST")

distmat <- matrix(nrow=nrow(ls2), ncol=nrow(ls2))
colnames(distmat) <- array(ls1$id_q)
rownames(distmat) <- array(ls2$id_r)
DIST_o <- DIST

for(r in 1:nrow(distmat)){
  for(c in 1:ncol(distmat)){
    if(r<c){
      distmat[r,c] = 0
    }
  }
}

for(i in nrow(distmat):2){
  c <- -(i-nrow(distmat)-1)
  temp <- DIST[1:i,]
  DIST <- DIST[-c(1:i),]
  distmat[c:nrow(distmat),c] <- array(temp$dist)
  # print(i)
}
distmat[nrow(distmat),nrow(distmat)] <- DIST[1,3]
setwd("D:/Desktop/modified/dtw")

write.csv(distmat, paste0("new_distmat_20",date[k],".csv"))

data <- data_o

new_list <- unique(data$ID)

data$resTime <- 0

for (i in 1:(length(new_list)-1)){
  print(i)
  id_q <- new_list[i]
  seq_q <- subset(data, data$ID == id_q) # extract query index
  
  for(j in 1:(nrow(seq_q)-1)){
    if(seq_q$Date[j] == seq_q$Date[j+1] & seq_q$activity!="login"){
      seq_q$resTime[j] <- as.POSIXct(seq_q$time[j+1], format = "%H:%M:%S")-as.POSIXct(seq_q$time[j], format = "%H:%M:%S")
    } else next 
  }
}  
