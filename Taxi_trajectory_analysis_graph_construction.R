
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

path <- "E:/processed_taxi/hourly3/"
temp <- paste0(path, "2. link")
setwd(temp)
flist <- list.files(temp)

time <- c("00","01","02","03","04","05","06","07",
          "08","09","10","11","12","13","14","15",
          "16","17","18","19","20","21","22","23")
  
for (j in 1:length(flist)){
  
  setwd(paste0(path, "2. link"))
    
  test <- read.table( flist[j], header = T, sep = ",")

  for (k in 1:8) {
      
    temp <- data.frame()
      
    temp <- test[substr(as.character(test$V2),9,10)==time[3*k-2]
                 | substr(as.character(test$V2),9,10)==time[3*k-1]
                 | substr(as.character(test$V2),9,10)==time[3*k],]
    
    temp <- temp [,c(1,8,11,12)]
    setwd(paste0(path,"2. div_link"))
      
    write.csv(temp, paste0("time3_link_", paste0(substr(flist[j], 6, 13),"_",time[3*k-2],time[3*k]), ".csv"), row.names = F)
        
  }
  print(j)
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

path <- "E:/processed_taxi/hourly3/"
link_dir <- paste0(path, "4. poly_link")

flist <- list.files(link_dir)

graph_dir <- paste0(path, "5. graph")
mat_dir <- paste0(path, "5-1. mat")


for (i in 1:length(flist)){
  setwd(link_dir)
  
  test2 <- read.csv(flist[i])
  test2 <- test2[order(test2$X),]

  list2 <- NULL
  
  for (j in 2:nrow(test2)) {
    if (test2$V7[j-1] == 2 && test2$V7[j] == 2) {
      list2 <- c(list2, j)
    } else if (test2$V7[j-1] == 1 && test2$V7[j] == 1) {
      list2 <- c(list2, j)
    }
    #print(j)
  }
  test2_o <- test2
  test2 <- test2[-list2,]
  
  org <- test2[test2$V7==1,7]
  dest <- test2[test2$V7==2,7]
  org <- data.frame(org)
  dest <- data.frame(dest)
  
  if (test2$V7[1] == 2) {
    test2 <- test2[-c(1,nrow(test2)),]
    org <- test2[test2$V7==1,7]
    dest <- test2[test2$V7==2,7]
    org <- data.frame(org)
    dest <- data.frame(dest)
  } 
  
  if (nrow(org)<nrow(dest)) {
    dest <- dest[-nrow(dest),]
  } else if (nrow(org)>nrow(dest)) { 
    org <- org[-nrow(org),]
  }
  
  org <- data.frame(org)
  dest <- data.frame(dest)
  graph <- cbind(org, dest)
  
  graph_o <- graph
  graph <- sqldf("SELECT org, dest, count(*)
                 FROM graph
                 GROUP BY org, dest")
  colnames(graph)[3] <- "weight"
  graph <-  na.omit(graph)
  
  mat <- matrix(0, nrow = 300, ncol = 300)
  
  for (k in 1:nrow(graph)) {
    org_m <- graph$org[k]
    dest_m <- graph$dest[k]
    mat[org_m, dest_m] <- graph$weight[k]
    
    #print(k)
  }
  
  rownames(mat) <- 1:300
  colnames(mat) <- 1:300
  
  setwd(graph_dir)
  write.csv(graph, paste0("time3_graph_", substr(flist[i], 6, 18), ".csv"), row.names = F)
  
  setwd(mat_dir)
  write.csv(mat, paste0("time3_mat_", substr(flist[i],  6, 18), ".csv"))
  print(i)
}

# 3. Preparing input data - label(node flux)
path <- "E:/processed_taxi/hourly3/"
mat_dir <- paste0(path, "5-1. mat")

flist <- list.files(mat_dir)
label_dir <- paste0(path, "5-2. label")

for (i in 1:length(flist)){
  setwd(mat_dir)
  
  test2 <- read.csv(flist[i])
  test2 <- test2[,-1]
  
  label <- data.frame()
  
  for (j in 1:ncol(test2)){
    all <- sum(test2[j,]) + sum(test2[,j]) - test2[j,j]
    demand <- sum(test2[j,])
    
    label <- rbind(label, c(j, all, demand))
  }
  
  colnames(label) <- c("idx", "all", "demand")
  
  setwd(label_dir)
  write.csv(label, paste0("time3_label_", substr(flist[i], 11, 23), ".csv"), row.names = FALSE)
  print(i)
}

##############################################
# 4. Neighborhood matrix
path <- "E:/processed_taxi/"
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

nei <- read.csv("neiborhood_matrix.csv")
nei <- nei[,-1]

geo_list <- data.frame()

geo_list <- rbind(geo_list, c(41,266))

for (i in 1:nrow(geo_list)){
  nei[geo_list[i,1],geo_list[i,2]] <- 0.5
  nei[geo_list[i,2],geo_list[i,1]] <- 0.5
  print(i)
}
write.csv(nei, paste0("adj_neiborhood_matrix.csv"))


# 4-1. Connectivity matrix
conn <- read.csv("grid_joined_node.csv", fileEncoding = "UTF-8")
conn_o <- conn
link <- read.csv("link.csv", fileEncoding = "UTF-8")
link <- link[,1:3]
link <- sqldf("SELECT link.*, conn.id FROM link, conn WHERE link.F_NODE = conn.NODE_ID")
link <- sqldf("SELECT link.*, conn.id FROM link, conn WHERE link.T_NODE = conn.NODE_ID")
link <- link[link$id != link$id..5,]

conn_mat <- matrix(0, nrow = 300, ncol = 300)

for (i in 1:nrow(link)) {
  conn_mat[link$id[i], link$id..5[i]] <- conn_mat[link$id[i], link$id..5[i]] + 1
  print(i)
}

rownames(conn_mat) <- 1:300
colnames(conn_mat) <- 1:300

write.csv(conn_mat, paste0("grid_conn_matrix.csv"))

# 4-1-1. Normalized
conn <- read.csv("conn_mat.csv")
conn <- conn[,-1]

area <- read.csv("area_matrix.csv", header=F)
norm_conn <- conn/area


input_conn <- NULL

for (i in 1:744){
  
  input_conn <- rbind(input_conn, conn)
  
  print(i)
}

setwd("E:/processed_taxi/only_month")
write.csv(input_conn, "time_input_conn.csv", row.names = FALSE)

# create real input data
path <- "E:/processed_taxi/hourly3/"
mat_dir <- paste0(path, "5-1. mat")

flist <- list.files(mat_dir)

input <- data.frame()

for (i in 1:length(flist)){
  setwd(mat_dir)
  
  test2 <- read.csv(flist[i])
  test2 <- test2[,-1]
  
  input <- rbind(input, test2)
  
  print(i)
}

setwd(path)
write.csv(input, "time3_input.csv", row.names = FALSE)



label_dir <- paste0(path, "5-2. label")

flist <- list.files(label_dir)

input_label <- data.frame()

for (i in 1:length(flist)){
  setwd(label_dir)
  
  test2 <- read.csv(flist[i])
  test2 <- test2[,-2]
  
  input_label <- rbind(input_label, test2)
  
  print(i)
}

setwd(path)
write.csv(input_label, "time3_input_demand.csv", row.names = FALSE)



path <- "E:/processed_taxi/"
spatial_dir <- paste0(path, "6. Spatial dependency modeling")

input_conn <- data.frame()

for (i in 1:717){
  setwd(spatial_dir)
  
  test2 <- read.csv("grid_conn_matrix.csv", header=F)
  
  input_conn <- rbind(input_conn, test2)
  
  print(i)
}
setwd(path)
write.csv(input_conn, "grid_input_conn.csv", row.names = FALSE)

nei <- data.frame(c(1:14))
temp_o <- data.frame(c(1:14))

for (i in 2:20){
  temp <- temp_o + 14*(i-1)
  nei <- cbind(nei, temp)
}

input_nei <- NULL

for (i in 1:744){

  input_nei <- rbind(input_nei, nei)
  
  print(i)
}

write.csv(input_nei, "adj_input_nei.csv", row.names = F)

for (i in 1:14){
  for (j in 1:20){
    
    nei_list <- data.frame()
    nei_list <- na.omit(unlist(c(nei[i-1,j],nei[i,j-1], nei[i+1,j],nei[i,j+1])))
    
    temp <- nei_list[1]
    
    for (k in 2:length(nei_list)){
      temp <- paste(temp, nei_list[k], sep = ",")
    }
    
    #temp2 <- data.frame()
    #temp2 <- c(nei[i,j], temp)
    input_nei <- rbind(input_nei, temp)  

  }
}

input_nei <- data.frame(input_nei)
rownames(input_nei) <- 1:300

path <- "D:/processed_taxi/"
spatial_dir <- paste0(path, "6. Spatial dependency modeling")



nei_mat <- matrix(0, nrow = 300, ncol = 300)
input_nei$input_nei <- as.character(input_nei$input_nei)

for (i in 1:nrow(input_nei)){
  ref_idx <- i
  temp <- unlist(strsplit(input_nei[i,], ","))
  for (j in 1:length(temp)){
    nei_mat[ref_idx, as.integer(temp[j])] <- 1
  }
  print(i)
}

rownames(nei_mat) <- 1:300
colnames(nei_mat) <- 1:300

write.csv(nei_mat, "grid_neiborhood_matrix.csv", row.names = F)

input_nei <- data.frame()

for (i in 1:717){
  setwd(spatial_dir)
  
  test2 <- read.csv("grid_neiborhood_matrix.csv", header=F)
  
  input_nei <- rbind(input_nei, test2)
  
  print(i)
}
setwd(path)
write.csv(input_nei, "grid_input_conn.csv", row.names = FALSE)


##########################################################
# 1. only_month
path <- "C:/Users/u/Desktop/processed_taxi/only_month/2. link"
flist <- list.files(path)
path2 <- "C:/Users/u/Desktop/processed_taxi/only_month/2. div_link"
time <- as.character(c(0:23))

for (k in 1:length(time)){
  if (nchar(time[k])==1) {time[k] = paste0("0", time[k])}
}

for (i in 1:60){
  setwd(path)
  temp <- read.csv(flist[i])
  temp$V2 <- as.character(temp$V2)
  for (j in 1:length(time)){
    temp2 <- temp[substr(temp$V2,9,10)==time[j],]
    temp2 <- temp2[,c(1,8,11,12)]
    setwd(path2)
    write.csv(temp2, paste0("link_",substr(flist[i],6,13),"_",time[j],".csv"), row.names = F)
  }
  print(i)
}



































