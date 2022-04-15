##### Code3 - 신도시별 유동인구 총합 추이 및 남녀 비율 #####

# 총합 추이 : 연령대, 성별을 고려하지 않고 합침. 전반적인 추이 파악이 주목적임.
# 남녀 비율 : 연령대를 고려하지 않고 성별만 고려함.
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

## 날짜 입력 함수 제작
makedate <- function(x = 15, y = 16) {
  mydate <<- NULL
  for(yy in c(x, y)) {
    for(m in 1:12) {
      if(m %in% 1:9) {
        mydate <<- c(mydate, as.numeric(paste0(yy, 0, m)))
      } else {
        mydate <<- c(mydate, as.numeric(paste0(yy, m)))
      }
    }
  }
  mydate <<- as.factor(mydate)
}

# 날짜 입력 함수 결과(밑에 성별 유동인구 구분 시 사용할 예정)
makedate()
mydate


#################################################################################################################################

##### 1기 신도시 #####
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

## 1. 분당신도시
findpop_b <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_bundang))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  bundang.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  bundang.fp[, 1] <<- mydate
  for(i in 1:24) {
    bundang.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(bundang.fp) <<- c("date_ym", "pop_total")
  
  bundang.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  bundang.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  bundang.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  bundang.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(bundang.fp.rat[, 2])
  colnames(bundang.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_b()

# 결과 확인
# (1) 유동인구 총합
str(bundang.fp)
bundang.fp

# (2) 남녀비율
str(bundang.fp.rat)
bundang.fp.rat



## 2. 일산신도시
findpop_i <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_ilsan))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  ilsan.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  ilsan.fp[, 1] <<- mydate
  for(i in 1:24) {
    ilsan.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(ilsan.fp) <<- c("date_ym", "pop_total")
  
  ilsan.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  ilsan.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  ilsan.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  ilsan.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(ilsan.fp.rat[, 2])
  colnames(ilsan.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_i()

# 결과 확인
# (1) 유동인구 총합
str(ilsan.fp)
ilsan.fp

# (2) 남녀비율
str(ilsan.fp.rat)
ilsan.fp.rat



## 3. 평촌신도시
findpop_pc <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_pyeongchon))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  pyeongchon.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  pyeongchon.fp[, 1] <<- mydate
  for(i in 1:24) {
    pyeongchon.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(pyeongchon.fp) <<- c("date_ym", "pop_total")
  
  pyeongchon.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  pyeongchon.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  pyeongchon.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  pyeongchon.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(pyeongchon.fp.rat[, 2])
  colnames(pyeongchon.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_pc()

# 결과 확인
# (1) 유동인구 총합
str(pyeongchon.fp)
pyeongchon.fp

# (2) 남녀비율
str(pyeongchon.fp.rat)
pyeongchon.fp.rat



## 4. 산본신도시
findpop_s <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_sanbon))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  sanbon.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  sanbon.fp[, 1] <<- mydate
  for(i in 1:24) {
    sanbon.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(sanbon.fp) <<- c("date_ym", "pop_total")
  
  sanbon.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  sanbon.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  sanbon.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  sanbon.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(sanbon.fp.rat[, 2])
  colnames(sanbon.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_s()

# 결과 확인
# (1) 유동인구 총합
str(sanbon.fp)
sanbon.fp

# (2) 남녀비율
str(sanbon.fp.rat)
sanbon.fp.rat



## 5. 중동신도시
findpop_j <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_jungdong))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  jungdong.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  jungdong.fp[, 1] <<- mydate
  for(i in 1:24) {
    jungdong.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(jungdong.fp) <<- c("date_ym", "pop_total")
  
  jungdong.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  jungdong.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  jungdong.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  jungdong.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(jungdong.fp.rat[, 2])
  colnames(jungdong.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_j()

# 결과 확인
# (1) 유동인구 총합
str(jungdong.fp)
jungdong.fp

# (2) 남녀비율
str(jungdong.fp.rat)
jungdong.fp.rat

#################################################################################################################################

##### 2기 신도시 #####

## 1. 판교신도시
findpop_p <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_pangyo))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  pangyo.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  pangyo.fp[, 1] <<- mydate
  for(i in 1:24) {
    pangyo.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(pangyo.fp) <<- c("date_ym", "pop_total")
  
  pangyo.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  pangyo.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  pangyo.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  pangyo.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(pangyo.fp.rat[, 2])
  colnames(pangyo.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_p()

# 결과 확인
# (1) 유동인구 총합
str(pangyo.fp)
pangyo.fp

# (2) 남녀비율
str(pangyo.fp.rat)
pangyo.fp.rat



## 2. 한강신도시
findpop_h <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_hangang))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  hangang.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  hangang.fp[, 1] <<- mydate
  for(i in 1:24) {
    hangang.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(hangang.fp) <<- c("date_ym", "pop_total")
  
  hangang.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  hangang.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  hangang.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  hangang.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(hangang.fp.rat[, 2])
  colnames(hangang.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_h()

# 결과 확인
# (1) 유동인구 총합
str(hangang.fp)
hangang.fp

# (2) 남녀비율
str(hangang.fp.rat)
hangang.fp.rat



## 3. 운정신도시
findpop_u <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_unjeong))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  unjeong.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  unjeong.fp[, 1] <<- mydate
  for(i in 1:24) {
    unjeong.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(unjeong.fp) <<- c("date_ym", "pop_total")
  
  unjeong.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  unjeong.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  unjeong.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  unjeong.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(unjeong.fp.rat[, 2])
  colnames(unjeong.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_u()

# 결과 확인
# (1) 유동인구 총합
str(unjeong.fp)
unjeong.fp

# (2) 남녀비율
str(unjeong.fp.rat)
unjeong.fp.rat



## 4. 광교신도시
findpop_g <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_gwanggyo))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  gwanggyo.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  gwanggyo.fp[, 1] <<- mydate
  for(i in 1:24) {
    gwanggyo.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(gwanggyo.fp) <<- c("date_ym", "pop_total")
  
  gwanggyo.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  gwanggyo.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  gwanggyo.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  gwanggyo.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(gwanggyo.fp.rat[, 2])
  colnames(gwanggyo.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_g()

# 결과 확인
# (1) 유동인구 총합
str(gwanggyo.fp)
gwanggyo.fp

# (2) 남녀비율
str(gwanggyo.fp.rat)
gwanggyo.fp.rat



## 5. 양주신도시
findpop_y <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_yangju))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  yangju.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  yangju.fp[, 1] <<- mydate
  for(i in 1:24) {
    yangju.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(yangju.fp) <<- c("date_ym", "pop_total")
  
  yangju.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  yangju.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  yangju.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  yangju.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(yangju.fp.rat[, 2])
  colnames(yangju.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_y()

# 결과 확인
# (1) 유동인구 총합
str(yangju.fp)
yangju.fp

# (2) 남녀비율
str(yangju.fp.rat)
yangju.fp.rat



## 6. 동탄신도시
findpop_d <- function(x = filepop15, y = filepop16) {
  kdata <- NULL
  
  for(fx in c(x, y)) {
    a <- fread(fx, stringsAsFactors = F)
    kdata <- rbind(kdata, subset(a, a$BLOCK_CD %in% cd_dongtan))
  }
  
  kdata$year <- as.numeric(substr(kdata$STD_YM, 1, 4))
  kdata$month <- as.numeric(substr(kdata$STD_YM, 5, 6))
  kdata$man <- kdata$MAN_FLOW_POP_CNT_10G + kdata$MAN_FLOW_POP_CNT_20G + kdata$MAN_FLOW_POP_CNT_30G +
    kdata$MAN_FLOW_POP_CNT_40G + kdata$MAN_FLOW_POP_CNT_50G + kdata$MAN_FLOW_POP_CNT_60GU
  kdata$woman <- kdata$WMAN_FLOW_POP_CNT_10G + kdata$WMAN_FLOW_POP_CNT_20G + kdata$WMAN_FLOW_POP_CNT_30G +
    kdata$WMAN_FLOW_POP_CNT_40G + kdata$WMAN_FLOW_POP_CNT_50G + kdata$WMAN_FLOW_POP_CNT_60GU
  
  man.fp <- NULL
  woman.fp <- NULL
  
  for(y in 2015:2016) {
    for(m in 1:12) {
      subdata <- subset(kdata, kdata$year == y & kdata$month == m)
      man.fp <- c(man.fp, sum(subdata$man))
      woman.fp <- c(woman.fp, sum(subdata$woman))
    }
  }
  
  dongtan.fp <<- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  dongtan.fp[, 1] <<- mydate
  for(i in 1:24) {
    dongtan.fp[i, 2] <<- sum(c(man.fp[i], woman.fp[i]))
  }
  colnames(dongtan.fp) <<- c("date_ym", "pop_total")
  
  dongtan.fp.rat <<- data.frame(matrix(data = NA, nrow = 2, ncol = 3))
  dongtan.fp.rat[, 1] <<- factor(c("man_pop", "woman_pop"))
  dongtan.fp.rat[, 2] <<- c(sum(man.fp), sum(woman.fp))
  dongtan.fp.rat[, 3] <<- c(sum(man.fp), sum(woman.fp))/sum(dongtan.fp.rat[, 2])
  colnames(dongtan.fp.rat) <<- c("gender", "gender_pop", "gender_ratio")
}

findpop_d()

# 결과 확인
# (1) 유동인구 총합
str(dongtan.fp)
dongtan.fp

# (2) 남녀비율
str(dongtan.fp.rat)
dongtan.fp.rat

#################################################################################################################################

##### 그림 - 유동인구 총합 추이 #####

## 1. 1기 신도시 유동인구 총합 추이
newtown1 <- rbind(bundang.fp, ilsan.fp, pyeongchon.fp, sanbon.fp, jungdong.fp)
newtown1$level <- rep(factor(c("bundang_pop", "ilsan_pop", "pyeongchon_pop", "sanbon_pop", "jungdong_pop")), each = 24)
newtown1

newtown1.plot <- ggplot(data = newtown1, aes(x = date_ym, y = pop_total, color = level, group = level)) +
  geom_line() + geom_point() + ggtitle("1기 신도시 유동인구 총합 추이")
newtown1.plot



## 2. 2기 신도시 유동인구 총합 추이
newtown2 <- rbind(pangyo.fp, hangang.fp, unjeong.fp, gwanggyo.fp, yangju.fp, dongtan.fp)
newtown2$level <- rep(factor(c("pangyo_pop", "hangang_pop", "unjeong_pop", "gwanggyo_pop", "yangju_pop", "dongtan_pop")), each = 24)
newtown2

newtown2.plot <- ggplot(data = newtown2, aes(x = date_ym, y = pop_total, color = level, group = level)) +
  geom_line() + geom_point() + ggtitle("2기 신도시 유동인구 총합 추이")
newtown2.plot



#################################################################################################################################

##### 그림 - 유동인구 남녀비율 #####

### 1기 신도시 ###

## 1. 분당신도시
bundang.fp.plot <- ggplot(bundang.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "분당신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
bundang.fp.plot



## 2. 일산신도시
ilsan.fp.plot <- ggplot(ilsan.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "일산신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
ilsan.fp.plot



## 3. 평촌신도시
pyeongchon.fp.plot <- ggplot(pyeongchon.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "평촌신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
pyeongchon.fp.plot



## 4. 산본신도시
sanbon.fp.plot <- ggplot(sanbon.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "산본신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
sanbon.fp.plot



## 5. 중동신도시
jungdong.fp.plot <- ggplot(jungdong.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "중동신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
jungdong.fp.plot



### 2기 신도시 ###

## 1. 판교신도시
pangyo.fp.plot <- ggplot(pangyo.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = " 판교신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
pangyo.fp.plot



## 2. 한강신도시
hangang.fp.plot <- ggplot(hangang.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "한강신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
hangang.fp.plot



## 3. 운정신도시
unjeong.fp.plot <- ggplot(unjeong.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "운정신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
unjeong.fp.plot



## 4. 광교신도시
gwanggyo.fp.plot <- ggplot(gwanggyo.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "광교신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
gwanggyo.fp.plot



## 5. 양주신도시
yangju.fp.plot <- ggplot(yangju.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "양주신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
yangju.fp.plot



## 6. 동탄신도시
dongtan.fp.plot <- ggplot(dongtan.fp.rat, aes(x = "", y = gender_pop, fill = gender)) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + geom_text(aes(label = paste0(round(gender_ratio*100, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "동탄신도시 유동인구 남녀비율") +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
dongtan.fp.plot
