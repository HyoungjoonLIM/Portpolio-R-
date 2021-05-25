#----------------------------------------------------------------------------------------------------------------------------
# Feb 2021, Hyoungjoon Lim
# Using the Association rule, activities that are highly related to each other among the activities left by employees 
# in the smart work solution were derived (descriptive analysis).
#----------------------------------------------------------------------------------------------------------------------------
install.packages("arulesViz")
library(arulesViz)
install.packages("arules")
library(arules)
install.packages('readxl')
library(readxl)
library(sqldf)
install.packages('dplyr')
library(dplyr)
library(datasets)
rm(list=ls())

setwd("D:/Desktop/마이워크 분석/rawdata/210125")

# Data preparation
act <- read.csv("액티비티raw_210304.csv")
upmu <- read.csv("업무등록raw_210304.csv")
weekly <- read.csv("주간보고raw_210304.csv")
alrm <- read.csv("알림수신raw_210304.csv")

mob <- read.csv("모바일raw_210304.csv")
sebu_inst <- read.csv("세부업무지시raw_210304.csv")
upmu_inst <- read.csv("업무지시raw_210304.csv")

summary(as.factor(act$액티비티유형명))

temp <- weekdays(as.Date(sebu$date,"%Y-%m-%d"))

# 1. alarm

#alrm$label <- paste0("알람수신(",alrm$알림구분코드명,")")
alrm$label <- "알람수신"
alrm <- alrm[,-10]
colnames(alrm) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 2. sebu
sebu_inst$label <- "세부업무지시"
colnames(sebu_inst) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 3. upmu
upmu$label <- "업무등록"
colnames(upmu) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")
upmu_inst$label <- "업무지시"
colnames(upmu_inst) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 4. weekly
weekly$label <- "주간보고제출"
colnames(weekly) <- c("date","time","name","ID","label")

# 5. activity
act$label <- act$액티비티유형명
act[act$액티비티유형명=="업무일정",]$label <- "일정등록"
act[act$액티비티유형명=="시스템"&
    grepl("업무를 추가", act$액티비티내용),]$label <- "세부업무추가"
act[act$액티비티유형명=="시스템"&
    grepl("'ToDo' 상태에서 'Doing'", act$액티비티내용),]$label <- "상태변경ToDoDoing"
act[act$액티비티유형명=="시스템"&
    grepl("'Doing' 상태에서 'Done'", act$액티비티내용),]$label <- "상태변경DoingDone"
act[act$액티비티유형명=="시스템"&
    grepl("'ToDo' 상태에서 'Done'", act$액티비티내용),]$label <- "상태변경ToDoDone"
act[act$액티비티유형명=="시스템"&
    grepl("'Done' 상태에서 'Doing'", act$액티비티내용),]$label <- "상태변경DoneDoing"
act[act$액티비티유형명=="시스템"&
    grepl("'Done' 상태에서 'ToDo'", act$액티비티내용),]$label <- "상태변경DoneToDo"
act[act$액티비티유형명=="시스템"&
    grepl("'Doing' 상태에서 'ToDo'", act$액티비티내용),]$label <- "상태변경DoingToDo"
act[act$액티비티유형명=="시스템"&
    grepl("업무를 지시", act$액티비티내용),]$label <- "세부업무지시"
act <- subset(act, act$label != "세부업무지시")
act <- subset(act, act$label != "시스템")

summary(as.factor(act$label))

act <- act[,-c(10,11)]

colnames(act) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 6. mobile
mob$label <- "모바일"
colnames(mob) <- c("date","time","name","ID",
                   "b_lev","c_lev","d_lev",
                   "rank","duty","label")

all_data <- rbind(act, alrm)
all_data <- rbind(all_data, sebu_inst)
all_data <- rbind(all_data, upmu)
all_data <- rbind(all_data, upmu_inst)
all_data <- rbind(all_data, mob)


humint <- unique(all_data[,c(4:9)])

temp <- merge(x=weekly, y=humint, by='ID')
temp <- temp[,c(2:4,1,6:10,5)]

all_data <- rbind(all_data, temp)

all_data <- all_data[order(all_data$time),]

all_data <- all_data[-c(1:5),]

write.csv(all_data, "all_data_210304.csv", row.names = FALSE)

summary(as.factor(all_data$label))

## association rule analysis : apriori()

all_data <- read.csv("all_data.csv")
set_data <- all_data

set_trans <- split(set_data$label, set_data$ID)
#set_trans <- as(set_trans, "transactions")

rule <- apriori(data = set_trans,
                parameter = list(support = 0.2,
                                 confidence = 0.5,
                                 minlen = 2,
                                 maxlen = 2))

#inspect(sort(rule, by = "lift")[1:20])

write(rule, 
      file = "rule2.csv", 
      sep = ",", 
      quote = TRUE,
      row.names = FALSE)
    
# scatter plot of association rules
plot(rule)

plot(sort(rule, by = "support")[1:20], method = 'graph', control = list(type = "itemsets"))

plot(sort(rule, by = "lift")[1:50], method = "grouped")
