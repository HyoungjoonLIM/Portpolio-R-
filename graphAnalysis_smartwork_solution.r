#------------------------------------------------------------------------
#
#   community detection 
# 
#   2019-03-11
#
#------------------------------------------------------------------------
install.packages(c('tidygraph','ggraph'))
install.packages("gtools")
install.packages("netrankr")
library(gtools)
library(readxl)
library(dplyr)
library(NetworkToolbox)
library(tidygraph)
library(ggraph)
library(netrankr)

setwd("D:/Desktop/rawdata")
data_o <- read.csv("task_by_employeeID.csv")

colnames(data_o) <- c('taskdate','taskID','jobdate','jobID',
                      'b_lev','c_lev','d_lev','rank','duty','ID','name')

# masking
substr(data_o$name,2,2) <- "*"
data_o$key <- paste0(data_o$ID, data_o$name)

# filtering
data <- subset(data_o, substr(data_o$taskdate,1,4) == "2021" &
                 data_o$d_lev == "Digital Strategy Dept." &
                 data_o$duty != "Part Leader" &
                 data_o$duty != "Senior manager")

data <- data[,c(2,12)]
data <- unique(data)
list <- unique(data$taskID)

fromto <- data.frame()

for (i in 1:length(list)){
  if(nrow(subset(data, data$taskID==list[i]))<2){next}
  else{
    temp <- subset(data, data$taskID==list[i])
    perm <- permutations(nrow(temp), 2, temp$key)
    fromto <- rbind(fromto, perm)
  }
  print(i)
}

colnames(fromto) <- c('from','to')
#fromto <- unique(fromto)

fromto %>% as_tbl_graph() %>%
  with_graph(graph_mean_dist())

fromto %>%
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(eigen = centrality_eigen(),
         group = group_louvain()) %>%
  ggraph(layout='kk') +
  geom_edge_link(color='gray50', alpha=.5) +
  geom_node_point(aes(color=factor(group),size=eigen)) +
  geom_node_text(aes(label=substr(name,8,10)), size=5, repel=TRUE) +
  theme_graph()

fromto %>% 
  as_tbl_graph() %>% 
  mutate(bet= centrality_betweenness()) %>%
  as_tibble %>%
  arrange(desc(bet))

fromto %>% 
  as_tbl_graph() %>% 
  mutate(bet= centrality_degree()) %>%
  as_tibble %>%
  arrange(desc(bet))

fromto %>% 
  as_tbl_graph() %>% 
  mutate(bet= centrality_eigen()) %>%
  as_tibble %>%
  arrange(desc(bet))


setwd("D:/Desktop/rawdata/graph")
















