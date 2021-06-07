#------------------------------------------------------------------------
#
#   Textmining with Web114 & Miwork data
# 
#   2021-04-12
#
#   Hyoungjoon Lim in my labtop 
#
# All employees must write their work in the form of a keyword list on the web114. 
# Web114 has been updated for smooth communication between departments in May, 
# and I would like to compare this with actual task (registered in Miwork).
#
#------------------------------------------------------------------------


install.packages("multilinguer")
install_jdk()
install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
install.packages("wordcloud")
install.packages('tm')
install.packages("proxy") 
library(multilinguer)
library(KoNLP)
library(wordcloud)
library(tm)
library(proxy)

setwd("D:/HJ/KB손해보험/3. 분산근무_분석백업/아카이브/data")

# 1. Data preparation
# '20.11 & '21.05 웹114 주요업무/상세업무 데이터 및 마이워크 데이터 
web2011 <- read.csv('WEB114_201112.csv')
web2105 <- read.csv('WEB114_210524.csv')# 웹114 주요업무/상세업무 데이터 업로드
txt <- read.csv('업무_세부업무명.csv')# 마이워크 데이터 업로드 
txt <- subset(txt, txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO' | txt$등록당시부서명=='OOO' | 
                txt$등록당시부서명=='OOO') # 등록당시 부서명으로 LoB 13개 파트 필터링(파트명은 마스킹) 
partList <- as.character(unique(txt$등록당시부서명)) # 분석 대상 부서 리스트로 관리 

tmp <- subset(web2105, web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO' | web2105$D레벨명=='OOO' | 
                web2105$D레벨명=='OOO') # '20.11 부서명에 오류, align을 위한 레퍼런스 리스트 생성 

codeList <- as.data.frame(unique(cbind(as.character(tmp$D레벨코드), as.character(tmp$D레벨명))))

codeList$V1 <- as.character(codeList$V1)
codeList$V2 <- as.character(codeList$V2)
web2011$부서코드 <- as.character(web2011$부서코드)
web2011$부서명 <- as.character(web2011$부서명)

for (i in 1:nrow(codeList)){ # align
  web2011[web2011$부서코드==codeList$V1[i],]$부서명 <- codeList$V2[i]
}

clean <- function(txt){ # 텍스트 전처리하는 함수 설정 
  txt <- tolower(txt)
  txt <- gsub('[[:punct:]]',' ',txt)
  txt <- removePunctuation(txt)
  txt <- removeNumbers(txt)
  txt <- stripWhitespace(txt)
  
  return(txt)
}

useSejongDic()

for(i in 1:length(partList)){ # 빈출단어 및 워드클라우드 생성하기
  print(partList[i])
  tmp_web2011 <- subset(web2011, web2011$부서명 == partList[i])
  tmp_web2105 <- subset(web2105, web2105$D레벨명 == partList[i])
  tmp_txt <- subset(txt, txt$등록당시부서명 == partList[i])# 각 파트별 웹114, 마이워크 데이터 추출
  
  web2011_byd <- c(as.character(tmp_web2011$담당업무), as.character(tmp_web2011$상세업무))
  web2105_byd <- c(as.character(tmp_web2105$주요업무), as.character(tmp_web2105$상세업무))
  
  tmp_txt1 <- tmp_txt[,-11]
  tmp_txt1 <- unique(tmp_txt1) # 업무명 중복 제거
  
  txt_byd <- c(as.character(tmp_txt1$업무명), as.character(tmp_txt$세부업무명)) # 중복 제거 후 세부ㅇ무명과 merge
  
  web2011_byd_N <- table(unlist(extractNoun(clean(web2011_byd))))
  web2105_byd_N <- table(unlist(extractNoun(clean(web2105_byd))))
  txt_byd_N <- table(unlist(extractNoun(clean(txt_byd)))) # 명사 추출 후 도수분포표 구성 
  
  web2011_byd_N <- as.data.frame(sort(web2011_byd_N, decreasing = T, by='Freq'))
  web2105_byd_N <- as.data.frame(sort(web2105_byd_N, decreasing = T, by='Freq'))
  txt_byd_N <- as.data.frame(sort(txt_byd_N, decreasing = T, by='Freq')) # 많은 순서대로 정렬 
  
  web2011_byd_N <- subset(web2011_byd_N, nchar(as.character(web2011_byd_N$Var1))>1)
  web2105_byd_N <- subset(web2105_byd_N, nchar(as.character(web2105_byd_N$Var1))>1)
  txt_byd_N <- subset(txt_byd_N, nchar(as.character(txt_byd_N$Var1))>1) # 한글자인 단어 제외 
  
  setwd("D:/HJ/KB손해보험/3. 분산근무_분석백업/아카이브/data/result/tm_2105")
  
  write.csv(web2011_byd_N, paste0('web2011_',partList[i],'.csv'), row.names = F)
  write.csv(web2105_byd_N, paste0('web2105_',partList[i],'.csv'), row.names = F)
  write.csv(txt_byd_N, paste0('txt_',partList[i],'.csv'), row.names = F) # 단어리스트 저장 
  
  png(filename=paste0('wc_web2011_',partList[i],'.png'),width=450,height=450,unit="px",bg="transparent")
  wordcloud(words = web2011_byd_N$Var1, freq = web2011_byd_N$Freq, min.freq = 2, max.words = 100, 
            random.order = F, rot.per = .1, scale = c(4, 0.3), colors = brewer.pal(5,"Set1"))
  dev.off()
  
  png(filename=paste0('wc_web2105_',partList[i],'.png'),width=450,height=450,unit="px",bg="transparent")
  wordcloud(words = web2105_byd_N$Var1, freq = web2105_byd_N$Freq, min.freq = 2, max.words = 100, 
            random.order = F, rot.per = .1, scale = c(4, 0.3), colors = brewer.pal(5,"Set1"))
  dev.off()
  
  png(filename=paste0('wc_txt_',partList[i],'.png'),width=450,height=450,unit="px",bg="transparent")
  wordcloud(words = txt_byd_N$Var1, freq = txt_byd_N$Freq, min.freq = 2, max.words = 100, 
            random.order = F, rot.per = .1, scale = c(4, 0.3), colors = brewer.pal(5,"Set1"))
  dev.off() # 워드클라우드 생성 후 png로 저장
}

web_byI <- merge(x=web2011, y=web2105, by.x='사번', by.y='이름')
IDlist <- unique(web_byI$사번)

RESULT <- data.frame()

for(i in 1:length(IDlist)){ # 직원별 코사인유사도 계산 
  print(i)
  tmp_web2011 <- subset(web2011, web2011$사번 == IDlist[i])
  tmp_web2105 <- subset(web2105, web2105$이름 == IDlist[i])
  tmp_txt <- subset(txt, txt$담당자사원번호 == IDlist[i])# 각 직원별 웹114, 마이워크 데이터 추출
  
  web2011_byd <- c(as.character(tmp_web2011$담당업무), as.character(tmp_web2011$상세업무))
  web2105_byd <- c(as.character(tmp_web2105$주요업무), as.character(tmp_web2105$상세업무))
  
  tmp_txt1 <- tmp_txt[,-11]
  tmp_txt1 <- unique(tmp_txt1) # 업무명 중복 제거
  
  # txt_byd <- as.character(tmp_txt$세부업무명)
  txt_byd <- c(as.character(tmp_txt1$업무명), as.character(tmp_txt$세부업무명)) # 중복 제거 후 세부ㅇ무명과 merge
  
  web2011_byd <- clean(paste(list(web2011_byd)))
  web2105_byd <- clean(paste(list(web2105_byd)))
  txt_byd <- clean(paste(list(txt_byd)))
  
  all_txt <- c(web2011_byd, web2105_byd, txt_byd)
  
  corpus <- Corpus(VectorSource(all_txt))
  
  tdm <- as.matrix(TermDocumentMatrix(corpus))
  #tdm[tdm>1] <- 1
  
  cosine_dist_mat <- as.matrix(dist(t(tdm), method = "cosine"))
  
  tmp_result <- t(as.data.frame(c(IDlist[i], 
                                  as.character(tmp_txt[tmp_txt$담당자사원번호==IDlist[i],]$담당자D레벨조직명[1]),
                                  as.character(tmp_txt[tmp_txt$담당자사원번호==IDlist[i],]$담당자직위명[1]),
                                  cosine_dist_mat[1,3], cosine_dist_mat[2,3])))
  colnames(tmp_result) <- c('ID','D Level', 'Rank', '2011-miwork', '2105-miwork')
  RESULT <- rbind(RESULT, tmp_result)
}

write.csv(RESULT, 'cosine_result_직원별.csv', row.names = F)

tmp_IDlist <- unique(web_byI$사번)

RESULT2 <- data.frame()

tmp_ID <- 1006432

for(i in 1:length(partList)){ # 부서별 빈출단어 Top10 추출 후, 직원별 빈출단어에서 걸러내고, 고유 단어들과 웹114 비교
  print(partList[i])
  tmp_txt <- subset(txt, txt$등록당시부서명 == partList[i])# 각 파트별 웹114, 마이워크 데이터 추출
  tmp_txt1 <- tmp_txt[,-11]
  tmp_txt1 <- unique(tmp_txt1) # 업무명 중복 제거
  txt_byd <- c(as.character(tmp_txt1$업무명), as.character(tmp_txt$세부업무명)) # 중복 제거 후 세부ㅇ무명과 merge
  
  txt_byd_N <- table(unlist(extractNoun(clean(txt_byd)))) # 명사 추출 후 도수분포표 구성 
  txt_byd_N <- as.data.frame(sort(txt_byd_N, decreasing = T, by='Freq')) # 많은 순서대로 정렬 
  txt_byd_N <- subset(txt_byd_N, nchar(as.character(txt_byd_N$Var1))>1) # 한글자인 단어 제외 
  
  top20 <- txt_byd_N[1:20,]
  
  tmp_IDlist <- subset(web_byI, as.character(web_byI$D레벨명)==partList[i])
  
  for(j in 1:length(tmp_IDlist)){
    # ttmp_txt <- subset(tmp_txt, tmp_txt$담당자사원번호 == tmp_IDlist$사번[j])
    ttmp_txt <- subset(tmp_txt, tmp_txt$담당자사원번호 == tmp_ID)
    
    if(nrow(ttmp_txt)==0){next}
    ttmp_txt1 <- ttmp_txt[,-11]
    ttmp_txt1 <- unique(ttmp_txt1) # 업무명 중복 제거
    txt_byi <- c(as.character(ttmp_txt1$업무명), as.character(ttmp_txt$세부업무명)) # 중복 제거 후 세부ㅇ무명과 merge
    
    txt_byi_N <- table(unlist(extractNoun(clean(txt_byi)))) # 명사 추출 후 도수분포표 구성 
    txt_byi_N <- as.data.frame(sort(txt_byi_N, decreasing = T, by='Freq')) # 많은 순서대로 정렬 
    txt_byi_N <- subset(txt_byi_N, nchar(as.character(txt_byi_N$Var1))>1) # 한글자인 단어 제외 
    
    txt_byi_N$common <- ifelse(as.character(txt_byi_N$Var1) %in% as.character(top20$Var1),
                               txt_byi_N$common <- 1, txt_byi_N$common <- 0) # 부서에서 흔히 쓰는 단어를 common이란 변수에 표시
    txt_byi_N <- subset(txt_byi_N, txt_byi_N$common==0) # 고유한 단어만 추출
    
    # ttmp_web2105 <- subset(web2105, web2105$이름 == tmp_IDlist$사번[j])
    ttmp_web2105 <- subset(web2105, web2105$이름 == tmp_ID)
    
    web2105_byi <- c(as.character(ttmp_web2105$주요업무), as.character(ttmp_web2105$상세업무))
    web2105_byi_N <- table(unlist(extractNoun(clean(web2105_byi))))
    web2105_byi_N <- as.data.frame(sort(web2105_byi_N, decreasing = T, by='Freq'))
    web2105_byi_N <- subset(web2105_byi_N, nchar(as.character(web2105_byi_N$Var1))>1)
    
    txt_byi_N$common2 <- ifelse(as.character(txt_byi_N$Var1) %in% as.character(web2105_byi_N$Var1),
                                txt_byi_N$common2 <- 1, txt_byi_N$common2 <- 0) # 고유업무를 대변하는 단어들이 web2105에 있는지 표시
    result <- t(as.data.frame(c(tmp_IDlist$사번[j], 
                                as.character(ttmp_txt[ttmp_txt$담당자사원번호==tmp_IDlist$사번[j],]$담당자D레벨조직명[1]),
                                as.character(ttmp_txt[ttmp_txt$담당자사원번호==tmp_IDlist$사번[j],]$담당자직위명[1]),
                                sum(txt_byi_N$common2))))
    RESULT2 <- rbind(RESULT2, result)
  }
}

write.csv(RESULT2, '웹114에 포함된 고유단어 개수_직원별.csv')