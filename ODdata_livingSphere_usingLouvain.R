

#------------------------------------------------------------------------
#
#   community detection with Louvain algorithm
# 
#   2019-03-11
#
#------------------------------------------------------------------------

setwd("E:/유동인구 분석/OD/서울/processed data & result")

library(readxl)
library(dplyr)
library(NetworkToolbox)
library(sqldf)


# data import (3496-by-3496) & basic stat

odseoul <- read.csv("louvain_input.csv")
odseoul <- odseoul[, -1]
diag(odseoul) <- 0
N_seoul <- nrow(odseoul) # number of nodes
L_seoul <- sum(colSums(odseoul != 0)) # number of non-zero links
del_seoul <- 2*L_seoul/N_seoul^2 # network connectivity

odtotal <- read.csv("od_total.csv")
odtotal <- odtotal[, -1]
diag(odtotal) <- 0
N_total <- nrow(odtotal) # number of nodes
L_total <- sum(colSums(odtotal != 0)) # number of links
del_total <- 2*L_total/N_total^2 # network connectivity


N_gw <- nrow(odgw) # number of nodes
L_gw <- sum(colSums(odgw != 0)) # number of non-zero links
del_gw <- 2*L_gw/N_gw^2 # network connectivity


od_guro <- odseoul[c(261:275), c(261:275)]
od_yd <- odseoul[c(286:303), c(286:303)]



############ node-based tool ############ 
setwd("E:/유동인구 분석/OD/서울/processed data & result/stat")
# 1) node degree
cnt <- 0
node_degree_seoul <- data.frame()

for (areaIndex in 1:N_seoul){
  
  i <- areaIndex
  for (j in 1:ncol(odseoul)){
    if (odseoul[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  j <- areaIndex
  for (i in 1:nrow(odseoul)){
    if (odseoul[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  node_degree_seoul <- rbind(node_degree_seoul, c(areaIndex, cnt))
  print(areaIndex)
  cnt <- 0
}  

colnames(node_degree_seoul) <- c("areaIndex","node_degree")
write.csv(node_degree_seoul, "node_degree_seoul.csv", row.names = F)

cnt <- 0
node_degree_gw <- data.frame()

for (areaIndex in 1:N_gw){
  
  i <- areaIndex
  for (j in 1:ncol(odgw)){
    if (odgw[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  j <- areaIndex
  for (i in 1:nrow(odgw)){
    if (odgw[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  node_degree_gw <- rbind(node_degree_gw, c(areaIndex, cnt))
  print(areaIndex)
  cnt <- 0
}  

colnames(node_degree_gw) <- c("areaIndex","node_degree")
write.csv(node_degree_gw, "node_degree_gw.csv", row.names = F)

cnt <- 0
node_degree_total <- data.frame()

for (areaIndex in 1:N_total){
  
  i <- areaIndex
  for (j in 1:ncol(odtotal)){
    if (odtotal[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  j <- areaIndex
  for (i in 1:nrow(odtotal)){
    if (odtotal[i,j] != 0){
      cnt <- cnt + 1
    }
  }
  node_degree_total <- rbind(node_degree_total, c(areaIndex, cnt))
  print(areaIndex)
  cnt <- 0
}  

colnames(node_degree_total) <- c("areaIndex","node_degree")
write.csv(node_degree_total, "node_degree_total.csv", row.names = F)


# 2) node flux
flux_in <- colSums(odseoul)
flux_out <- rowSums(odseoul)
node_flux_seoul <- data.frame()

for (areaIndex in 1:N_seoul){
  node_flux_seoul <- rbind(node_flux_seoul, c(areaIndex, flux_in[areaIndex], flux_out[areaIndex], 
                                              flux_in[areaIndex]+flux_out[areaIndex]))
}  

colnames(node_flux_seoul) <- c("areaIndex","flux_in", "flux_out", "node_flux")
write.csv(node_flux_seoul, "node_flux_seoul.csv", row.names = F)



flux_in <- colSums(odgw)
flux_out <- rowSums(odgw)
node_flux_gw <- data.frame()

for (areaIndex in 1:N_gw){
  node_flux_gw <- rbind(node_flux_gw, c(areaIndex, flux_in[areaIndex], flux_out[areaIndex], 
                                              flux_in[areaIndex]+flux_out[areaIndex]))
}  

colnames(node_flux_gw) <- c("areaIndex","flux_in", "flux_out", "node_flux")
write.csv(node_flux_gw, "node_flux_gw.csv", row.names = F)



flux_in <- colSums(odtotal)
flux_out <- rowSums(odtotal)
node_flux_total <- data.frame()

for (areaIndex in 1:N_total){
  node_flux_total <- rbind(node_flux_total, c(areaIndex, flux_in[areaIndex], flux_out[areaIndex], 
                                              flux_in[areaIndex]+flux_out[areaIndex]))
}  

colnames(node_flux_total) <- c("areaIndex","flux_in", "flux_out", "node_flux")
write.csv(node_flux_total, "node_flux_total.csv", row.names = F)


# 3) coefficient of variation of k, F
CV_k_seoul <- sd(node_degree_seoul$node_degree) / mean(node_degree_seoul$node_degree)
CV_k_total <- sd(node_degree_total$node_degree) / mean(node_degree_total$node_degree)
CV_k_busan <- sd(node_degree_busan$node_degree) / mean(node_degree_busan$node_degree)
CV_k_gw <- sd(node_degree_gw$node_degree) / mean(node_degree_gw$node_degree)

CV_F_seoul <- sd(node_flux_seoul$node_flux) / mean(node_flux_seoul$node_flux)
CV_F_total <- sd(node_flux_total$node_flux) / mean(node_flux_total$node_flux)
CV_F_busan <- sd(node_flux_busan$node_flux) / mean(node_flux_busan$node_flux)
CV_F_gw <- sd(node_flux_gw$node_flux) / mean(node_flux_gw$node_flux)

# 4) clustering coefficient
# local clustering coefficient
local_clu_coeff_seoul <- data.frame()
tau0 <- (N_seoul-1)*(N_seoul-2)

for (areaIndex in 1:N_seoul){
  cnt <- 0
  listin <- which(odseoul[,areaIndex]!=0, arr.ind = TRUE)
  listout <- which(odseoul[areaIndex,]!=0, arr.ind = TRUE)[,2]
  list <- unique(c(listin, listout))
  for (i in 1:(length(list)-1)){
    pt1 <- list[i]
    for (j in (i+1):length(list)){
      pt2 <- list[j]
      if (odseoul[pt1, pt2] | odseoul[pt2, pt1]) {cnt <- cnt + 1}
      else {next}
    }
  }
  clu_coeff <- cnt/tau0
  local_clu_coeff_seoul <- rbind(local_clu_coeff_seoul, c(areaIndex, clu_coeff))
  print(areaIndex)
}

colnames(local_clu_coeff_seoul) <- c("areaIndex","local_clu_coeff_seoul")
write.csv(local_clu_coeff_seoul, "local_clu_coeff_seoul.csv", row.names = F)
mean_clu_coeff_seoul <- mean(local_clu_coeff_seoul$`local_clu_coeff_seoul`)

# weighted local clustering coefficient
weighted_local_clu_coeff_seoul <- data.frame()
tau0 <- sum(odseoul)

for (areaIndex in 1:N_seoul){
  cnt <- 0
  listin <- which(odseoul[,areaIndex]!=0, arr.ind = TRUE)
  listout <- which(odseoul[areaIndex,]!=0, arr.ind = TRUE)[,2]
  list <- unique(c(listin, listout))
  for (i in 1:(length(list)-1)){
    pt1 <- list[i]
    for (j in (i+1):length(list)){
      pt2 <- list[j]
      cnt <- cnt + odseoul[pt1, pt2] + odseoul[pt2, pt1]
    }
  }
  tau <- tau0 - (colSums(odseoul)[areaIndex]+rowSums(odseoul)[areaIndex])
  clu_coeff <- cnt/tau
  weighted_local_clu_coeff_seoul <- rbind(weighted_local_clu_coeff_seoul, c(areaIndex, clu_coeff))
  print(areaIndex)
}

colnames(weighted_local_clu_coeff_seoul) <- c("areaIndex","weighted_local_clu_coeff")
write.csv(weighted_local_clu_coeff_seoul, "weighted_local_clu_coeff_seoul.csv", row.names = F)
mean_wei_clu_coeff_seoul <- mean(weighted_local_clu_coeff_seoul$`weighted_local_clu_coeff`)


local_clu_coeff_gw <- data.frame()
tau0 <- (N_gw-1)*(N_gw-2)

for (areaIndex in 1:N_gw){
  cnt <- 0
  listin <- which(odgw[,areaIndex]!=0, arr.ind = TRUE)
  listout <- which(odgw[areaIndex,]!=0, arr.ind = TRUE)[,2]
  list <- unique(c(listin, listout))
  for (i in 1:(length(list)-1)){
    pt1 <- list[i]
    for (j in (i+1):length(list)){
      pt2 <- list[j]
      if (odgw[pt1, pt2] & odgw[pt2, pt1]) {cnt <- cnt + 2}
      else if (odgw[pt1, pt2] | odgw[pt2, pt1]) {cnt <- cnt + 1}
      else {next}
    }
  }
  clu_coeff <- cnt*2/tau0
  local_clu_coeff_gw <- rbind(local_clu_coeff_gw, c(areaIndex, clu_coeff))
  print(areaIndex)
}

colnames(local_clu_coeff_gw) <- c("areaIndex","local_clu_coeff_gw")
write.csv(local_clu_coeff_gw, "local_clu_coeff_gw.csv", row.names = F)
mean_clu_coeff_gw <- mean(local_clu_coeff_gw$`local_clu_coeff_gw`)

# weighted local clustering coefficient
weighted_local_clu_coeff_gw <- data.frame()
tau0 <- sum(odgw)

for (areaIndex in 1:N_gw){
  cnt <- 0
  listin <- which(odgw[,areaIndex]!=0, arr.ind = TRUE)
  listout <- which(odgw[areaIndex,]!=0, arr.ind = TRUE)[,2]
  list <- unique(c(listin, listout))
  for (i in 1:(length(list)-1)){
    pt1 <- list[i]
    for (j in (i+1):length(list)){
      pt2 <- list[j]
      cnt <- cnt + odgw[pt1, pt2] + odgw[pt2, pt1]
    }
  }
  tau <- tau0 - (colSums(odgw)[areaIndex]+rowSums(odgw)[areaIndex])
  clu_coeff <- cnt/tau
  weighted_local_clu_coeff_gw <- rbind(weighted_local_clu_coeff_gw, c(areaIndex, clu_coeff))
  print(areaIndex)
}

colnames(weighted_local_clu_coeff_gw) <- c("areaIndex","weighted_local_clu_coeff")
write.csv(weighted_local_clu_coeff_gw, "weighted_local_clu_coeff_gw.csv", row.names = F)
mean_wei_clu_coeff_gw <- mean(weighted_local_clu_coeff_gw$`weighted_local_clu_coeff`)


all_node <- data.frame(cbind(node_degree_seoul$areaIndex, node_degree_seoul$node_degree, node_flux_seoul$node_flux, local_clu_coeff_seoul$`local clustering coefficient`, CV_F_seoul))
colnames(all_node) <- c("OD_code", "node_degree", "node_flux", "local_clustering_coefficient", "CV_F")
write.csv(all_node, "Node-based_analysis_output.csv", row.names = F)

############ link-based tool ############ 
# 1) link weight
odseoul_l <- odseoul
link_list_seoul <- data.frame()
for (i in 1:20){
  link_list_seoul <- rbind(link_list_seoul, 
                           c(i, which(odseoul_l==max(odseoul_l), arr.ind = TRUE), 
                           max(odseoul_l)))
  odseoul_l[which(odseoul_l==max(odseoul_l), arr.ind = TRUE)] <- 0
}
colnames(link_list_seoul) <- c("number", "origin", "destination", "link weight")
write.csv(link_list_seoul, "link_list_seoul.csv", row.names = F)

odgw_l <- odgw
link_list_gw <- data.frame()
for (i in 1:20){
  link_list_gw <- rbind(link_list_gw, 
                           c(i, which(odgw_l==max(odgw_l), arr.ind = TRUE), 
                             max(odgw_l)))
  odgw_l[which(odgw_l==max(odgw_l), arr.ind = TRUE)] <- 0
}
colnames(link_list_gw) <- c("number", "origin", "destination", "link weight")
write.csv(link_list_gw, "link_list_gw.csv", row.names = F)

odtotal_l <- odtotal

link_list_total <- data.frame()
for (i in 1:20){
  link_list_total <- rbind(link_list_total, 
                           c(i, which(odtotal_l==max(odtotal_l), arr.ind = TRUE), 
                             max(odtotal_l)))
  odtotal_l[which(odtotal_l==max(odtotal_l), arr.ind = TRUE)] <- 0
}
colnames(link_list_total) <- c("number", "origin", "destination", "link weight")
write.csv(link_list_total, "link_list_total.csv", row.names = F)

  
# 2) coefficient of variation of w
long_odseoul <- data.frame()
for (i in 1:N_seoul){
  for (j in 1:N_seoul){
    long_odseoul <- rbind(long_odseoul, c(i,j,odseoul[i,j]))
  }
  print(i)
}
colnames(long_odseoul) <- c("origin","destination","link_weight")
long_odseoul <- long_odseoul[-long_odseoul$link_weight!=0,]
CV_w_seoul <- sd(long_odseoul$link_weight) / mean(long_odseoul$link_weight)

long_odbusan <- data.frame()
for (i in 1:N_busan){
  for (j in 1:N_busan){
    long_odbusan <- rbind(long_odbusan, c(i,j,odbusan[i,j]))
  }
  print(i)
}
colnames(long_odbusan) <- c("origin","destination","link_weight")
long_odbusan <- long_odbusan[-long_odbusan$link_weight!=0,]
CV_w_busan <- sd(long_odbusan$link_weight) / mean(long_odbusan$link_weight)

long_odgw <- data.frame()
for (i in 1:N_gw){
  for (j in 1:N_gw){
    long_odgw <- rbind(long_odgw, c(i,j,odgw[i,j]))
  }
  print(i)
}
colnames(long_odgw) <- c("origin","destination","link_weight")
long_odgw <- long_odgw[-long_odgw$link_weight!=0,]
CV_w_gw <- sd(long_odgw$link_weight) / mean(long_odgw$link_weight)

long_odtotal <- data.frame()
for (i in 1:N_total){
  for (j in 1:N_total){
    long_odtotal <- rbind(long_odtotal, c(i,j,odtotal[i,j]))
  }
  print(i)
}
colnames(long_odtotal) <- c("origin","destination","link_weight")
long_odtotal <- long_odtotal[-long_odtotal$link_weight!=0,]
CV_w_total <- sd(long_odtotal$link_weight) / mean(long_odtotal$link_weight)


# 3) Global clustering coefficient
perm <- function(n,k){choose(n,k) * factorial(k)}
tau <- perm(N_seoul,3)

cnt <- 0
for (i in 1:(N_seoul-2)){
  for(j in (i+1):(N_seoul-1)){
    for(k in (j+1):N_seoul){
      if((odseoul[i,j]!=0 | odseoul[j,i]!=0) & (odseoul[j,k]!=0 | odseoul[k,j]!=0) & (odseoul[k,i]!=0 | odseoul[i,k]!=0)){
        cnt <- cnt + 1
      }
    }
  }
  print(i)
}

global_clu_coeff_seoul <- cnt*6 / tau

perm <- function(n,k){choose(n,k) * factorial(k)}
tau <- perm(N_gw,3)

cnt <- 0
for (i in 1:(N_gw-2)){
  for(j in (i+1):(N_gw-1)){
    for(k in (j+1):N_gw){
      if((odgw[i,j]!=0 | odgw[j,i]!=0) & (odgw[j,k]!=0 | odgw[k,j]!=0) & (odgw[k,i]!=0 | odgw[i,k]!=0)){
        cnt <- cnt + 1
      }
    }
  }
  print(i)
}

global_clu_coeff_gw <- cnt*6 / tau

perm <- function(n,k){choose(n,k) * factorial(k)}
tau <- perm(N_busan,3)

cnt <- 0
for (i in 1:(N_busan-2)){
  for(j in (i+1):(N_busan-1)){
    for(k in (j+1):N_busan){
      if((odbusan[i,j]!=0 | odbusan[j,i]!=0) & (odbusan[j,k]!=0 | odbusan[k,j]!=0) & (odbusan[k,i]!=0 | odbusan[i,k]!=0)){
        cnt <- cnt + 1
      }
    }
  }
  print(i)
}

global_clu_coeff_busan <- cnt*6 / tau

all_link <- rbind(link_list_seoul, c("CV(w)", CV_w_seoul, "Global clustering coefficient", global_clu_coeff_seoul))
write.csv(all_link, "Link-based_analysis_output.csv")


############ louvain - NetworkToolbox ############ 


odsample <- function(k){
  odsample <- od_yd[1:k, 1:k]
  return(odsample)
  }

odsample(100) %>% dim()

# simple result
k <-15
A <- odsample(k)
gamma <- 1
louvain(A, gamma)


# function


modularity <- function(k, gamma){
  start_time <- Sys.time()
  A <- odsample(k)
  modularity <- louvain(A, gamma)
  end_time <- Sys.time()
  list(k=k, gamma=gamma, Community=modularity$community, Q=modularity$Q, time = end_time - start_time)
}


# results

result <- modularity(k=18, gamma=1.5)
sort(unique(result$Community))
write.csv(result$Community, 'res_1.5_yd.csv')
#------------------------------------------------------------------------
# package2 - igraph

#install.packages("igraph")
library(igraph)

# This is so simple that we will have only one level
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
g
cluster_louvain(g)

######################################################################
#songdo
# NetworkToolbox

odsample <- function(k){
  odsample <- odtotal[1:k, 1:k]
  return(odsample)
}


# function

modularity <- function(k, gamma){
  start_time <- Sys.time()
  A <- odsample(k)
  modularity <- louvain(A, gamma)
  end_time <- Sys.time()
  list(k=k, gamma=gamma, Community=modularity$community, Q=modularity$Q, time = end_time - start_time)
}

odtotal <- read.csv("180412.csv")
odtotal <- odtotal[, -1]

# results

result <- modularity(k=267, gamma=0.5)
sort(unique(result$Community))
write.csv(result$Community, 'res_0.5_100m.csv')

result <- modularity(k=267, gamma=1)
sort(unique(result$Community))
write.csv(result$Community, 'res_1_100m.csv')

result <- modularity(k=267, gamma=1.5)
sort(unique(result$Community))
write.csv(result$Community, 'res_1.5_100m.csv')

result <- modularity(k=267, gamma=2)
sort(unique(result$Community))
write.csv(result$Community, 'res_2_100m.csv')



odtotal <- read.csv("180426.csv")
odtotal <- odtotal[, -1]

# results

result <- modularity(k=150, gamma=1)
sort(unique(result$Community))
write.csv(result$Community, 'res_180426.csv')

odtotal <- read.csv("180427.csv")
odtotal <- odtotal[, -1]

# results

result <- modularity(k=150, gamma=1)
sort(unique(result$Community))
write.csv(result$Community, 'res_180427.csv')

# modularity
setwd("E:/유동인구 분석/OD/서울/processed data & result")
A <- read.csv("louvain_input.csv")
A <- A[,-1]

M0 <- read.csv("res_seoul_0.55_5.csv")
M0 <- M0[,2]
gamma <- 0.55
diag(A) <- 0
n <- ncol(A)
s <- sum(A)

Mb <- M0
M <- M0
mat <- matrix(0, nrow = n, ncol = n)
for (i in 1:n){
  for (j in 1:n) {
  mat[i, j] <- (colSums(A)[i] * rowSums(A)[j])/s
  }
  print(i)
}
B <- A - (gamma * (mat))
B <- (B + t(B))/2
Hnm <- matrix(0, nrow = nrow(A), ncol = (length(unique(Mb))))
for (m in 1:max(Mb)) {
  if (!is.null(nrow(B[, which(Mb == m)]))) {
    Hnm[, m] <- rowSums(B[, which(Mb == m)])
  }
  else {
    Hnm[, m] <- B[, which(Mb == m)]
  }
}
H <- colSums(Hnm)
Hm <- rowSums(Hnm)
Q0 <- (-Inf)
bsxfun <- matrix(0, nrow = n, ncol = n)
diag(bsxfun) <- 1
Q <- sum(diag(as.matrix(B)*bsxfun))/s
n <- max(Mb)
B1 <- matrix(0, nrow = n, ncol = n)
for (u in 1:n) for (v in u:n) {
  bm <- sum(sum(B[Mb == u, Mb == v]))
  B1[u, v] <- bm
  B1[v, u] <- bm
}
B <- B1
Mb <- 1:n
Hnm <- B
H <- colSums(B)
Q0 <- Q
Q <- sum(diag(B))/s











































