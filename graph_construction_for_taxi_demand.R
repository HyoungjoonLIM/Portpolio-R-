
#----------------------------------------------#
#                                              #
#   Graph construction using taxicab data      #
#                                              #
#   2019-09-11                                 #
#                                              #
#----------------------------------------------#


# 1. Extract origin and destination

library(dplyr)
library(sqldf)
library(stringr)

path <- "D:/processed_taxi/"
flist <- list.files(path)[59:102]

temp <- paste0(path, "unzip")
setwd(temp)

for (i in 3:11){
  
  unzip(paste0(path,flist[i]), exdir=temp)
  fflist <- list.files(temp)
  
  for (j in 1:length(fflist)){
    setwd(temp)
    
    test <- read.table( fflist[j], header = F, sep = ",")
    test <- test[test$V7!=0,]
    test <- test[test$V7!=3,]
    
    link_test <- data.frame()
    list <- NULL
    
    for (k in 2:nrow(test)) {
      if (test$V7[k-1] == 2 && test$V7[k] == 2) {
        list <- c(list, k)
      }
    }
    
    test_o <- test
    test <- test[-list,]
    
    setwd(paste0(path,"link"))
  
    write.csv(test, paste0("link_", substr(fflist[j], 12, 19), ".csv"))
    print(j)
    
  }
  setwd(temp)
  file.remove(fflist)
  print(i)
  
}

##############################################
# 2. K-means clustering(extract only origin from 8 dates)
path <- "E:/석사4학기/졸업연구/Graph construction/k-means"
setwd(path)
flist <- list.files(path)
for (i in 1:length(flist)){
  test <- read.csv(flist[i])
  test2 <- test[test$V7==1,]
  write.csv(test2, paste0("ori_", substr(flist[i], 6, 13), ".csv"))
  print(i)
}

coord <- data.frame()
for (i in 11:length(flist)){
  test <- read.csv(flist[i])
  test2 <- test[,12:13]
  coord <- rbind(coord, test2)
  print(i)
}
write.csv(coord, "all_coord.csv", row.names = F)

coord <- read.csv("inSeoul.csv")
coord.kmeans <- kmeans(coord, centers = 300, iter.max = 10000)
coord$cluster <- as.factor(coord.kmeans$cluster)

write.csv(coord, "k-means_result.csv", row.names = F)


# 2-1. preprocessing for spatial join
path <- "D:/processed_taxi/"

temp <- paste0(path, "link")
temp2 <- paste0(path, "short_link")

setwd(temp)
flist <- list.files(temp)

for (i in 1:length(flist)){
  setwd(temp)
  test <- read.csv(flist[i])
  test2 <- test[,c(1,8,11,12)]
  setwd(temp2)
  write.csv(test2, paste0(substr(flist[i], 1, 13), ".csv"))
  print(i)
}

##############################################
# 3. Preparing input data - 1) matrix form and 2) adjacency list form
library(sqldf)

path <- "E:/석사4학기/졸업연구/processed_taxi/"
link_dir <- paste0(path, "4. poly_link")

flist <- list.files(link_dir)

graph_dir <- paste0(path, "graph")
mat_dir <- paste0(path, "mat")


for (i in 1:length(flist)){
  setwd(link_dir)
  
  test2 <- read.csv(flist[i])
  test2 <- test2[order(test2$field_1),]
  test2 <- na.omit(test2)
  
  list2 <- NULL
  
  for (j in 2:nrow(test2)) {
    if (test2$V7[j-1] == 2 && test2$V7[j] == 2) {
      list2 <- c(list2, j)
    } else if (test2$V7[j-1] == 1 && test2$V7[j] == 1) {
      list2 <- c(list2, j)
    }
    print(j)
  }
  test2_o <- test2
  test2 <- test2[-list2,]
  
  org <- test2[test2$V7==1,13]
  dest <- test2[test2$V7==2,13]
  org <- data.frame(org)
  dest <- data.frame(dest)
  graph <- cbind(org, dest)
  
  graph_o <- graph
  graph <- sqldf("SELECT org, dest, count(*)
                  FROM graph
                  GROUP BY org, dest")
  colnames(graph)[3] <- "weight"
  
  mat <- matrix(0, nrow = 300, ncol = 300)
  
  for (k in 1:nrow(graph)) {
    org_m <- graph$org[k]
    dest_m <- graph$dest[k]
    mat[org_m, dest_m] <- graph$weight[k]
    
    print(k)
  }
  
  rownames(mat) <- 1:300
  colnames(mat) <- 1:300
  
  setwd(graph_dir)
  write.csv(graph, paste0("graph_", substr(flist[i], 11, 18), ".csv"), row.names = F)
  
  setwd(mat_dir)
  write.csv(mat, paste0("mat_", substr(flist[i], 11, 18), ".csv"))
  
}

##############################################
# 4. Neighborhood matrix
path <- "E:/석사4학기/졸업연구/processed_taxi/"
spatial_dir <- paste0(path, "6. Spatial dependency modeling")

setwd(spatial_dir)
nei <- read.csv("neighborhood.csv")
nei <- nei[ order(nei$cluster), ]

nei_mat <- matrix(0, nrow = 300, ncol = 300)
nei$NEIGHBORS <- as.character(nei$NEIGHBORS)

for (i in 1:nrow(nei)){
  ref_idx <- nei$cluster[i]
  temp <- unlist(strsplit(nei$NEIGHBORS[i], ","))
  for (j in 1:length(temp)){
    nei_mat[ref_idx, as.integer(temp[j])] <- 1
  }
  print(i)
}

rownames(nei_mat) <- 1:300
colnames(nei_mat) <- 1:300

write.csv(nei_mat, paste0("neiborhood_matrix.csv"))

# 4-1. Connectivity matrix
conn <- read.csv("joined_node.csv", fileEncoding = "UTF-8")
conn_o <- conn
conn <- cbind(conn$NODE_ID, conn$cluster, conn$LINK_ID)
colnames(conn) <- c("node", "cluster", "link")
conn <- data.frame(conn)

conn <- na.omit(conn)
conn_o <- conn

temp <- data.frame()
for (i in 1:length(list_link)) {
  
}

















