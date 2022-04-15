##### Code4 - 신도시별 연령별 유동인구 비율 #####

# 성별을 고려하지 않고 연령별만 고려함.
# Code1과 연계됨.

## "성연령별유동인구_yymm.csv" 파일명 제작
makepop <- function(y) {
  filename <- NULL
  for(i in 1:12) {
    if(i %in% 1:9) {
      filename <- c(filename, paste0("성연령별유동인구_", y, 0, i, ".csv"))
    } else {
      filename <- c(filename, paste0("성연령별유동인구_", y, i, ".csv"))
    }
  }
  return(filename)
}

# 2015년 성연령별유동인구 csv file 이름
filepop15 <- makepop(15)
filepop15

# 2016년 성연령별유동인구 csv file 이름
filepop16 <- makepop(16)
filepop16


#################################################################################################################################

##### 1기 신도시 - 연령별 유동인구 #####
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

## 1. 분당신도시 - 연령별 유동인구 추출
findpop_b2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_bundang))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  bundang.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  bundang.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  bundang.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  bundang.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(bundang.fp2[, 2])
  colnames(bundang.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_b2()

# 결과 확인
str(bundang.fp2)
bundang.fp2

# 그림 확인
bundang.fp.plot2 <- ggplot(bundang.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "분당신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
bundang.fp.plot2



## 2. 일산신도시 - 연령별 유동인구 추출
findpop_i2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_ilsan))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  ilsan.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  ilsan.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  ilsan.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  ilsan.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(ilsan.fp2[, 2])
  colnames(ilsan.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_i2()

# 결과 확인
str(ilsan.fp2)
ilsan.fp2

# 그림 확인
ilsan.fp.plot2 <- ggplot(ilsan.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "일산신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
ilsan.fp.plot2



## 3. 평촌신도시 - 연령별 유동인구 추출
findpop_pc2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_pyeongchon))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  pyeongchon.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  pyeongchon.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  pyeongchon.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  pyeongchon.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(pyeongchon.fp2[, 2])
  colnames(pyeongchon.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_pc2()

# 결과 확인
str(pyeongchon.fp2)
pyeongchon.fp2

# 그림 확인
pyeongchon.fp.plot2 <- ggplot(pyeongchon.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "평촌신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
pyeongchon.fp.plot2



## 4. 산본신도시 - 연령별 유동인구 추출
findpop_s2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_sanbon))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  sanbon.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  sanbon.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  sanbon.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  sanbon.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(sanbon.fp2[, 2])
  colnames(sanbon.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_s2()

# 결과 확인
str(sanbon.fp2)
sanbon.fp2

# 그림 확인
sanbon.fp.plot2 <- ggplot(sanbon.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "산본신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
sanbon.fp.plot2



## 5. 중동신도시 - 연령별 유동인구 추출
findpop_j2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_jungdong))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  jungdong.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  jungdong.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  jungdong.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  jungdong.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(jungdong.fp2[, 2])
  colnames(jungdong.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_j2()

# 결과 확인
str(jungdong.fp2)
jungdong.fp2

# 그림 확인
jungdong.fp.plot2 <- ggplot(jungdong.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "중동신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
jungdong.fp.plot2

#################################################################################################################################

##### 2기 신도시 - 연령별 유동인구 #####

## 1. 판교신도시 - 연령별 유동인구 추출
findpop_p2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_pangyo))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  pangyo.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  pangyo.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  pangyo.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  pangyo.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(pangyo.fp2[, 2])
  colnames(pangyo.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_p2()

# 결과 확인
str(pangyo.fp2)
pangyo.fp2

# 그림 확인
pangyo.fp.plot2 <- ggplot(pangyo.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "판교신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
pangyo.fp.plot2



## 2. 한강신도시 - 연령별 유동인구 추출
findpop_h2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_hangang))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  hangang.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  hangang.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  hangang.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  hangang.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(hangang.fp2[, 2])
  colnames(hangang.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_h2()

# 결과 확인
str(hangang.fp2)
hangang.fp2

# 그림 확인
hangang.fp.plot2 <- ggplot(hangang.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "한강신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
hangang.fp.plot2



## 3. 운정신도시 - 연령별 유동인구 추출
findpop_u2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_unjeong))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  unjeong.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  unjeong.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  unjeong.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  unjeong.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(unjeong.fp2[, 2])
  colnames(unjeong.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_u2()

# 결과 확인
str(unjeong.fp2)
unjeong.fp2

# 그림 확인
unjeong.fp.plot2 <- ggplot(unjeong.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "운정신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
unjeong.fp.plot2



## 4. 광교신도시 - 연령별 유동인구 추출
findpop_g2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_gwanggyo))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  gwanggyo.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  gwanggyo.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  gwanggyo.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  gwanggyo.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(gwanggyo.fp2[, 2])
  colnames(gwanggyo.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_g2()

# 결과 확인
str(gwanggyo.fp2)
gwanggyo.fp2

# 그림 확인
gwanggyo.fp.plot2 <- ggplot(gwanggyo.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "광교신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
gwanggyo.fp.plot2



## 5. 양주신도시 - 연령별 유동인구 추출
findpop_y2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_yangju))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  yangju.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  yangju.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  yangju.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  yangju.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(yangju.fp2[, 2])
  colnames(yangju.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_y2()

# 결과 확인
str(yangju.fp2)
yangju.fp2

# 그림 확인
yangju.fp.plot2 <- ggplot(yangju.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "양주신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
yangju.fp.plot2



## 6. 동탄신도시 - 연령별 유동인구 추출
findpop_d2 <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_dongtan))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$age10 <- kdata$MAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_10G
  kdata$age20 <- kdata$MAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_20G
  kdata$age30 <- kdata$MAN_FLOW_POP_CNT_30G + kdata$WMAN_FLOW_POP_CNT_30G
  kdata$age40 <- kdata$MAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_40G
  kdata$age50 <- kdata$MAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_50G
  kdata$age60 <- kdata$MAN_FLOW_POP_CNT_60GU + kdata$WMAN_FLOW_POP_CNT_60GU
  
  age10.fp <- 0; age20.fp <- 0; age30.fp <- 0
  age40.fp <- 0; age50.fp <- 0; age60.fp <- 0
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      age10.fp <- age10.fp + sum(subdata$age10); age20.fp <- age20.fp + sum(subdata$age20)
      age30.fp <- age30.fp + sum(subdata$age30); age40.fp <- age40.fp + sum(subdata$age40)
      age50.fp <- age50.fp + sum(subdata$age50); age60.fp <- age60.fp + sum(subdata$age60)
    }
  }
  
  dongtan.fp2 <<- data.frame(matrix(data = NA, nrow = 6, ncol = 3))
  dongtan.fp2[, 1] <<- factor(c("age10_pop", "age20_pop", "age30_pop", "age40_pop", "age50_pop", "age60_pop"))
  dongtan.fp2[, 2] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)
  dongtan.fp2[, 3] <<- c(age10.fp, age20.fp, age30.fp, age40.fp, age50.fp, age60.fp)/sum(dongtan.fp2[, 2])
  colnames(dongtan.fp2) <<- c("age_group", "age_population", "age_group_ratio")
}

findpop_d2()

# 결과 확인
str(dongtan.fp2)
dongtan.fp2

# 그림 확인
dongtan.fp.plot2 <- ggplot(dongtan.fp2, aes(x = "", y = age_population, fill = age_group)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(age_group_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "동탄신도시 연령별 유동인구 비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
dongtan.fp.plot2
