##### Code2 - 신도시별 업종별매출 #####

# Code1과 연계됨.

## "업종별매출_yymm.csv" 파일명 제작
make <- function(y) {
  filename <- NULL
  for(i in 1:12) {
    if(i %in% 1:9) {
      filename <- c(filename, paste0("업종별매출_", y, 0, i, ".csv"))
    } else {
      filename <- c(filename, paste0("업종별매출_", y, i, ".csv"))
    }
  }
  return(filename)
}

# 2015년 업종별매출 csv file 이름
file15 <- make(15)
file15

# 2016년 업종별매출 csv file 이름
file16 <- make(16)
file16


#################################################################################################################################

### 참조 ###
# 용량 큰 파일 읽기 : data.table package의 fread() 함수
# fread()의 옵션 : read.table(), read.csv()와 동일함.

##### 1기 신도시 - 업종별매출 #####
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

## 1. 분당신도시 - 업종별매출 추출
findsale_b <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_bundang))
  }
  bundang15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  bundang16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_b()

# 결과 확인
str(bundang15)
bundang15

str(bundang16)
bundang16



## 2. 일산신도시 - 업종별매출 추출
findsale_i <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_ilsan))
  }
  ilsan15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  ilsan16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_i()

# 결과 확인
str(ilsan15)
ilsan15

str(ilsan16)
ilsan16



## 3. 평촌신도시 - 업종별매출 추출
findsale_pc <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_pyeongchon))
  }
  pyeongchon15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  pyeongchon16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_pc()

# 결과 확인
str(pyeongchon15)
pyeongchon15

str(pyeongchon16)
pyeongchon16



## 4. 산본신도시 - 업종별매출 추출
findsale_s <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_sanbon))
  }
  sanbon15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  sanbon16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_s()

# 결과 확인
str(sanbon15)
sanbon15

str(sanbon16)
sanbon16



## 5. 중동신도시 - 업종별매출 추출
findsale_j <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_jungdong))
  }
  jungdong15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  jungdong16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_j()

# 결과 확인
str(jungdong15)
jungdong15

str(jungdong16)
jungdong16

#################################################################################################################################

##### 2기 신도시 - 업종별매출 #####

## 1. 판교신도시 - 업종별매출 추출
findsale_p <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_pangyo))
  }
  pangyo15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  pangyo16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_p()

# 결과 확인
str(pangyo15)
pangyo15

str(pangyo16)
pangyo16



## 2. 한강신도시 - 업종별매출 추출
findsale_h <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_hangang))
  }
  hangang15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  hangang16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_h()

# 결과 확인
str(hangang15)
hangang15

str(hangang16)
hangang16



## 3. 운정신도시 - 업종별매출 추출
findsale_u <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_unjeong))
  }
  unjeong15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  unjeong16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_u()

# 결과 확인
str(unjeong15)
unjeong15

str(unjeong16)
unjeong17



## 4. 광교신도시 - 업종별매출 추출
findsale_g <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_gwanggyo))
  }
  gwanggyo15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  gwanggyo16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_g()

# 결과 확인
str(gwanggyo15)
gwanggyo15

str(gwanggyo16)
gwanggyo16



## 5. 양주신도시 - 업종별매출 추출
findsale_y <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_yangju))
  }
  yangju15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  yangju16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_y()

# 결과 확인
str(yangju15)
yangju15

str(yangju16)
yangju16



## 6. 동탄신도시 - 업종별매출 추출
findsale_d <- function(x = file15, y = file16) {
  ksale <- NULL
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F, encoding = "UTF-8")
    ksale <- rbind(ksale, subset(a, a$BLOCK_CD %in% cd_dongtan))
  }
  dongtan15 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2015")
  dongtan16 <<- subset(ksale, substr(ksale$STD_YM, 1, 4) == "2016")
}

findsale_d()

# 결과 확인
str(dongtan15)
dongtan15

str(dongtan16)
dongtan16

#################################################################################################################################

##### 교육 매출, 의료 매출 추출 #####

### 1기 신도시 ###

## 1. 분당신도시
extractsale_b <- function(x = bundang15, y = bundang16) {
  edu.bundang <<- NULL
  medic.bundang <<- NULL
  
  edu.bundang <<- rbind(edu.bundang, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.bundang <<- rbind(medic.bundang, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_b()

# 결과 확인
edu.bundang
medic.bundang



## 2. 일산신도시
extractsale_i <- function(x = ilsan15, y = ilsan16) {
  edu.ilsan <<- NULL
  medic.ilsan <<- NULL
  
  edu.ilsan <<- rbind(edu.ilsan, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.ilsan <<- rbind(medic.ilsan, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_i()

# 결과 확인
edu.ilsan
medic.ilsan



## 3. 평촌신도시
extractsale_pc <- function(x = pyeongchon15, y = pyeongchon16) {
  edu.pyeongchon <<- NULL
  medic.pyeongchon <<- NULL
  
  edu.pyeongchon <<- rbind(edu.pyeongchon, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.pyeongchon <<- rbind(medic.pyeongchon, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_pc()

# 결과 확인
edu.pyeongchon
medic.pyeongchon



## 4. 산본신도시
extractsale_s <- function(x = sanbon15, y = sanbon16) {
  edu.sanbon <<- NULL
  medic.sanbon <<- NULL
  
  edu.sanbon <<- rbind(edu.sanbon, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.sanbon <<- rbind(medic.sanbon, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_s()

# 결과 확인
edu.sanbon
medic.sanbon



## 5. 중동신도시
extractsale_j <- function(x = jungdong15, y = jungdong16) {
  edu.jungdong <<- NULL
  medic.jungdong <<- NULL
  
  edu.jungdong <<- rbind(edu.jungdong, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.jungdong <<- rbind(medic.jungdong, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_j()

# 결과 확인
edu.jungdong
medic.jungdong



### 2기 신도시 ###

## 1. 판교신도시
extractsale_p <- function(x = pangyo15, y = pangyo16) {
  edu.pangyo <<- NULL
  medic.pangyo <<- NULL
  
  edu.pangyo <<- rbind(edu.pangyo, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.pangyo <<- rbind(medic.pangyo, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_p()

# 결과 확인
edu.pangyo
medic.pangyo



## 2. 한강신도시
extractsale_h <- function(x = hangang15, y = hangang16) {
  edu.hangang <<- NULL
  medic.hangang <<- NULL
  
  edu.hangang <<- rbind(edu.hangang, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.hangang <<- rbind(medic.hangang, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_h()

# 결과 확인
edu.hangang
medic.hangang



## 3. 운정신도시
extractsale_u <- function(x = unjeong15, y = unjeong16) {
  edu.unjeong <<- NULL
  medic.unjeong <<- NULL
  
  edu.unjeong <<- rbind(edu.unjeong, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.unjeong <<- rbind(medic.unjeong, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_u()

# 결과 확인
edu.unjeong
medic.unjeong



## 4. 광교신도시
extractsale_g <- function(x = gwanggyo15, y = gwanggyo16) {
  edu.gwanggyo <<- NULL
  medic.gwanggyo <<- NULL
  
  edu.gwanggyo <<- rbind(edu.gwanggyo, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.gwanggyo <<- rbind(medic.gwanggyo, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_g()

# 결과 확인
edu.gwanggyo
medic.gwanggyo



## 5. 양주신도시
extractsale_y <- function(x = yangju15, y = yangju16) {
  edu.yangju <<- NULL
  medic.yangju <<- NULL
  
  edu.yangju <<- rbind(edu.yangju, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.yangju <<- rbind(medic.yangju, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_y()

# 결과 확인
edu.yangju
medic.yangju



## 6. 동탄신도시
extractsale_d <- function(x = dongtan15, y = dongtan16) {
  edu.dongtan <<- NULL
  medic.dongtan <<- NULL
  
  edu.dongtan <<- rbind(edu.dongtan, subset(x, x$LCLS_NM == "학문/교육"), subset(y, y$LCLS_NM == "학문/교육"))
  medic.dongtan <<- rbind(medic.dongtan, subset(x, x$LCLS_NM == "의료"), subset(y, y$LCLS_NM == "의료"))
}

extractsale_d()

# 결과 확인
edu.dongtan
medic.dongtan

#################################################################################################################################

##### 교육 매출, 의료 매출 시각화 #####
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

### 1기 신도시 ###

## 1. 교육 매출 막대그래프
edu.newtown1 <- data.frame(matrix(data = NA, nrow = 5, ncol = 2))
edu.newtown1[, 1] <- c(mean(edu.bundang$SALE_AMT), mean(edu.ilsan$SALE_AMT), mean(edu.pyeongchon$SALE_AMT),
                       mean(edu.sanbon$SALE_AMT), mean(edu.jungdong$SALE_AMT))
edu.newtown1[, 2] <- factor(c("bundang_edu", "ilsan_edu", "pyeongchon_edu", "sanbon_edu", "jungdong_edu"))
colnames(edu.newtown1) <- c("sale_mean", "level")
edu.newtown1

edu.newtown1.plot <- ggplot(edu.newtown1, aes(x = level, y = sale_mean, fill = level)) + geom_bar(stat = "identity", width = 0.5) +
  ggtitle("1기 신도시 교육 업종 평균매출")
edu.newtown1.plot



## 2. 의료 매출 막대그래프
medic.newtown1 <- data.frame(matrix(data = NA, nrow = 5, ncol = 2))
medic.newtown1[, 1] <- c(mean(medic.bundang$SALE_AMT), mean(medic.ilsan$SALE_AMT), mean(medic.pyeongchon$SALE_AMT),
                       mean(medic.sanbon$SALE_AMT), mean(medic.jungdong$SALE_AMT))
medic.newtown1[, 2] <- factor(c("bundang_medic", "ilsan_medic", "pyeongchon_medic", "sanbon_medic", "jungdong_medic"))
colnames(medic.newtown1) <- c("sale_mean", "level")
medic.newtown1

medic.newtown1.plot <- ggplot(medic.newtown1, aes(x = level, y = sale_mean, fill = level)) + geom_bar(stat = "identity", width = 0.5) +
  ggtitle("1기 신도시 의료 업종 평균매출")
medic.newtown1.plot



### 2기 신도시 ###

## 1. 교육 매출 막대그래프
edu.newtown2 <- data.frame(matrix(data = NA, nrow = 6, ncol = 2))
edu.newtown2[, 1] <- c(mean(edu.pangyo$SALE_AMT), mean(edu.hangang$SALE_AMT), mean(edu.unjeong$SALE_AMT),
                       mean(edu.gwanggyo$SALE_AMT), mean(edu.yangju$SALE_AMT), mean(edu.dongtan$SALE_AMT))
edu.newtown2[, 2] <- factor(c("pangyo_edu", "hangang_edu", "unjeong_edu", "gwanggyo_edu", "yangju_edu", "dongtan_edu"))
colnames(edu.newtown2) <- c("sale_mean", "level")
edu.newtown2

edu.newtown2.plot <- ggplot(edu.newtown2, aes(x = level, y = sale_mean, fill = level)) + geom_bar(stat = "identity", width = 0.5) +
  ggtitle("2기 신도시 교육 업종 평균매출")
edu.newtown2.plot



## 2. 의료 매출 막대그래프
medic.newtown2 <- data.frame(matrix(data = NA, nrow = 6, ncol = 2))
medic.newtown2[, 1] <- c(mean(medic.pangyo$SALE_AMT), mean(medic.hangang$SALE_AMT), mean(medic.unjeong$SALE_AMT),
                         mean(medic.gwanggyo$SALE_AMT), mean(medic.yangju$SALE_AMT), mean(medic.dongtan$SALE_AMT))
medic.newtown2[, 2] <- factor(c("pangyo_medic", "hangang_medic", "unjeong_medic", "gwanggyo_medic", "yangju_medic", "dongtan_medic"))
colnames(medic.newtown2) <- c("sale_mean", "level")
medic.newtown2

medic.newtown2.plot <- ggplot(medic.newtown2, aes(x = level, y = sale_mean, fill = level)) + geom_bar(stat = "identity", width = 0.5) +
  ggtitle("2기 신도시 의료 업종 평균매출")
medic.newtown2.plot

#################################################################################################################################

##### 교육 매출, 의료 매출 중분류 빈도표 #####

### 1기 신도시 ###

## 1. 교육 매출
table(edu.bundang$MCLS_NM)
table(edu.ilsan$MCLS_NM)
table(edu.pyeongchon$MCLS_NM)
table(edu.sanbon$MCLS_NM)
table(edu.jungdong$MCLS_NM)



## 2. 의료 매출
table(medic.bundang$MCLS_NM)
table(medic.ilsan$MCLS_NM)
table(medic.pyeongchon$MCLS_NM)
table(medic.sanbon$MCLS_NM)
table(medic.jungdong$MCLS_NM)



### 2기 신도시 ###

## 1. 교육 매출
table(edu.pangyo$MCLS_NM)
table(edu.hangang$MCLS_NM)
table(edu.unjeong$MCLS_NM)
table(edu.gwanggyo$MCLS_NM)
table(edu.yangju$MCLS_NM)
table(edu.dongtan$MCLS_NM)



## 2. 의료 매출
table(medic.pangyo$MCLS_NM)
table(medic.hangang$MCLS_NM)
table(medic.unjeong$MCLS_NM)
table(medic.gwanggyo$MCLS_NM)
table(medic.yangju$MCLS_NM)
table(medic.dongtan$MCLS_NM)
