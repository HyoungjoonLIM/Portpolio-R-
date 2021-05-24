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

setwd("D:/Desktop/���̿�ũ �м�/rawdata/210125")

# Data preparation
act <- read.csv("��Ƽ��Ƽraw_210304.csv")
upmu <- read.csv("�������raw_210304.csv")
weekly <- read.csv("�ְ�����raw_210304.csv")
alrm <- read.csv("�˸�����raw_210304.csv")

mob <- read.csv("�����raw_210304.csv")
sebu_inst <- read.csv("���ξ�������raw_210304.csv")
upmu_inst <- read.csv("��������raw_210304.csv")

summary(as.factor(act$��Ƽ��Ƽ������))

temp <- weekdays(as.Date(sebu$date,"%Y-%m-%d"))

# 1. alarm

#alrm$label <- paste0("�˶�����(",alrm$�˸������ڵ��,")")
alrm$label <- "�˶�����"
alrm <- alrm[,-10]
colnames(alrm) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 2. sebu
sebu_inst$label <- "���ξ�������"
colnames(sebu_inst) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 3. upmu
upmu$label <- "�������"
colnames(upmu) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")
upmu_inst$label <- "��������"
colnames(upmu_inst) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 4. weekly
weekly$label <- "�ְ���������"
colnames(weekly) <- c("date","time","name","ID","label")

# 5. activity
act$label <- act$��Ƽ��Ƽ������
act[act$��Ƽ��Ƽ������=="��������",]$label <- "�������"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("������ �߰�", act$��Ƽ��Ƽ����),]$label <- "���ξ����߰�"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'ToDo' ���¿��� 'Doing'", act$��Ƽ��Ƽ����),]$label <- "���º���ToDoDoing"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'Doing' ���¿��� 'Done'", act$��Ƽ��Ƽ����),]$label <- "���º���DoingDone"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'ToDo' ���¿��� 'Done'", act$��Ƽ��Ƽ����),]$label <- "���º���ToDoDone"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'Done' ���¿��� 'Doing'", act$��Ƽ��Ƽ����),]$label <- "���º���DoneDoing"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'Done' ���¿��� 'ToDo'", act$��Ƽ��Ƽ����),]$label <- "���º���DoneToDo"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("'Doing' ���¿��� 'ToDo'", act$��Ƽ��Ƽ����),]$label <- "���º���DoingToDo"
act[act$��Ƽ��Ƽ������=="�ý���"&
    grepl("������ ����", act$��Ƽ��Ƽ����),]$label <- "���ξ�������"
act <- subset(act, act$label != "���ξ�������")
act <- subset(act, act$label != "�ý���")

summary(as.factor(act$label))

act <- act[,-c(10,11)]

colnames(act) <- c("date","time","name","ID",
                    "b_lev","c_lev","d_lev",
                    "rank","duty","label")

# 6. mobile
mob$label <- "�����"
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
