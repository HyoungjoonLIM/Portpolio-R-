#----------------------------------------------------------------------------------------------------------------------------
# Nov 2020, Hyoungjoon Lim
# An EDA was attempted using the texts left by employees on the smart work solution. Emoticons using frequently used words were created and applied.
#----------------------------------------------------------------------------------------------------------------------------

install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
install.packages("wordcloud")
library(KoNLP)
library(wordcloud)
install.packages('tm')
library(tm)

setwd("D:/Desktop/rawdata")

data <- read.csv("raw_20200929_forTM.csv")
data_o <- subset(data, activity_type=="add reply")

data$label <- 0
data[data$code=="B000032",]$label <- 1 
data[data$code=="B000028",]$label <- 2 
data[data$code=="B000025",]$label <- 3 
data[data$code=="B000030",]$label <- 4 
data[data$code=="B000020",]$label <- 5 
data[data$code=="B000017",]$label <- 6 
data[data$code=="B000014" | data$code=="C000043" | data$code=="C000067",]$label <- 7 
data[data$code=="C000066",]$label <- 8
data[data$code=="C000076"| data$code=="C000086",]$label <- 9 
data[data$code=="B000018",]$label <- 10 
data[data$code=="B000027",]$label <- 11 

setwd("D:/Desktop/modified")

data_o <- data

useSejongDic()

for(i in 1:max(range(data$label))){
  
  data <- data_o[data_o$label==i,]
  data <- data$활동내용
  
  nouns <- extractNoun(data) # 명사 추출
  
  wordCount <- table(unlist(nouns))
  df_word <- as.data.frame(wordCount, stringAsFactors = F) 
  df_word$word <- as.character(df_word$Var1)
  df_word$count <- nchar(df_word$word)
  df_word <- subset(df_word, df_word$count >= 2)
  
  
  pal <- brewer.pal(8, "Dark2")   # Dark2 
  set.seed(1234)  # 난수 고정
  png(paste0('wordCloud_',i,'.png'),width = 2000, height = 1500)
  wordcloud(words = df_word$word, freq = df_word$Freq, min.freq = 2, max.words = 75, 
            random.order = F, rot.per = .1, scale = c(4, 0.3), colors = pal)
  
  write.csv(df_word, paste0('df_word_',i,'.csv'), row.names = F)
  print(i)
  rm(data)
  
}











