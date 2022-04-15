##### Code6 - 기업등록부 상용근로자 추출 #####

## Set and Get working directory
setwd("D:/최종욱(cjw0107)/TAKE_OUT_FILES/data analysis")
getwd()

## 기업등록부 원데이터 호출
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

if(!require(bit64)) {
  install.packages("bit64")
}
library(bit64)

company15 <- fread("기업등록부_사업자_2015.csv", stringsAsFactors = F)
company16 <- fread("기업등록부_사업자_2016.csv", stringsAsFactors = F)

#########################################################################################

##### BR사업체 상용근로자 합계 및 비율 추출 #####

### 1기 신도시 ###

## 1. 분당신도시
am_bundang <- c(3102362, 3102364, 3102363, 3102360, 3102361, 3102358, 3102359, 3102351, 3102353,
                3102354, 3102352, 3102378, 3102377, 3102355, 3102356, 3102371, 3102372)
bundang.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_bundang))
bundang.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_bundang))
bundang.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_bundang))
bundang.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_bundang))

bundang.workq15 <- bundang.worker15 / bundang.work.tot15
bundang.workq16 <- bundang.worker16 / bundang.work.tot16

# 결과 확인
# (1) 상용근로자 합계
bundang.worker15
bundang.worker16

# (2) 상용근로자 비율
bundang.workq15
bundang.workq16



## 2. 일산신도시
am_ilsan <- c(3110359, 3110356, 3110357, 3110355, 3110361, 3110353, 3110451, 3110452, 3110453, 3110455, 3110456, 3110457)
ilsan.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_ilsan))
ilsan.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_ilsan))
ilsan.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_ilsan))
ilsan.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_ilsan))

ilsan.workq15 <- ilsan.worker15 / ilsan.work.tot15
ilsan.workq16 <- ilsan.worker16 / ilsan.work.tot16

# 결과 확인
# (1) 상용근로자 합계
ilsan.worker15
ilsan.worker16

# (2) 상용근로자 비율
ilsan.workq15
ilsan.workq16



## 3. 평촌신도시
am_pyeongchon <- c(3104267, 3104261, 3104265, 3104258, 3104254, 3104266, 3104260, 3104259)
pyeongchon.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_pyeongchon))
pyeongchon.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_pyeongchon))
pyeongchon.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_pyeongchon))
pyeongchon.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_pyeongchon))

pyeongchon.workq15 <- pyeongchon.worker15 / pyeongchon.work.tot15
pyeongchon.workq16 <- pyeongchon.worker16 / pyeongchon.work.tot16

# 결과 확인
# (1) 상용근로자 합계
pyeongchon.worker15
pyeongchon.worker16

# (2) 상용근로자 비율
pyeongchon.workq15
pyeongchon.workq16



## 4. 산본신도시
am_sanbon <- c(3116054, 3116055, 3116057, 3116058, 3116059, 3116060, 3116061)
sanbon.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_sanbon))
sanbon.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_sanbon))
sanbon.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_sanbon))
sanbon.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_sanbon))

sanbon.workq15 <- sanbon.worker15 / sanbon.work.tot15
sanbon.workq16 <- sanbon.worker16 / sanbon.work.tot16

# 결과 확인
# (1) 상용근로자 합계
sanbon.worker15
sanbon.worker16

# (2) 상용근로자 비율
sanbon.workq15
sanbon.workq16



## 5. 중동신도시
am_jungdong15 <- c(3105162, 3105163, 3105164, 3105165, 3105166, 3105153, 3105161, 3105259, 3105260, 3105357)
am_jungdong16 <- c(3105061, 3105066, 3105067, 3105068, 3105064, 3105053, 3105065, 3105073, 3105074, 3105086)
jungdong.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_jungdong15))
jungdong.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_jungdong16))
jungdong.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_jungdong15))
jungdong.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_jungdong16))

jungdong.workq15 <- jungdong.worker15 / jungdong.work.tot15
jungdong.workq16 <- jungdong.worker16 / jungdong.work.tot16

# 결과 확인
# (1) 상용근로자 합계
jungdong.worker15
jungdong.worker16

# (2) 상용근로자 비율
jungdong.workq15
jungdong.workq16



### 2기 신도시 ###

## 1. 판교신도시
am_pangyo <- c(3102374, 3102376, 3102375, 3102368)
pangyo.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_pangyo))
pangyo.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_pangyo))
pangyo.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_pangyo))
pangyo.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_pangyo))

pangyo.workq15 <- pangyo.worker15 / pangyo.work.tot15
pangyo.workq16 <- pangyo.worker16 / pangyo.work.tot16

# 결과 확인
# (1) 상용근로자 합계
pangyo.worker15
pangyo.worker16

# (2) 상용근로자 비율
pangyo.workq15
pangyo.workq16



## 2. 한강신도시
am_hangang <- c(3123059, 3123056, 3123060, 3123058)
hangang.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_hangang))
hangang.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_hangang))
hangang.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_hangang))
hangang.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_hangang))

hangang.workq15 <- hangang.worker15 / hangang.work.tot15
hangang.workq16 <- hangang.worker16 / hangang.work.tot16

# 결과 확인
# (1) 상용근로자 합계
hangang.worker15
hangang.worker16

# (2) 상용근로자 비율
hangang.workq15
hangang.workq16



## 3. 운정신도시
am_unjeong <- c(3120056, 3120057, 3120058)
unjeong.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_unjeong))
unjeong.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_unjeong))
unjeong.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_unjeong))
unjeong.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_unjeong))

unjeong.workq15 <- unjeong.worker15 / unjeong.work.tot15
unjeong.workq16 <- unjeong.worker16 / unjeong.work.tot16

# 결과 확인
# (1) 상용근로자 합계
unjeong.worker15
unjeong.worker16

# (2) 상용근로자 비율
unjeong.workq15
unjeong.workq16



## 4. 광교신도시
am_gwanggyo <- c(3101460, 3101462, 3101463, 3119357, 3119358)
gwanggyo.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_gwanggyo))
gwanggyo.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_gwanggyo))
gwanggyo.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_gwanggyo))
gwanggyo.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_gwanggyo))

gwanggyo.workq15 <- gwanggyo.worker15 / gwanggyo.work.tot15
gwanggyo.workq16 <- gwanggyo.worker16 / gwanggyo.work.tot16

# 결과 확인
# (1) 상용근로자 합계
gwanggyo.worker15
gwanggyo.worker16

# (2) 상용근로자 비율
gwanggyo.workq15
gwanggyo.workq16



## 5. 양주신도시
am_yangju <- c(3126054, 3126056)
yangju.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_yangju))
yangju.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_yangju))
yangju.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_yangju))
yangju.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_yangju))

yangju.workq15 <- yangju.worker15 / yangju.work.tot15
yangju.workq16 <- yangju.worker16 / yangju.work.tot16

# 결과 확인
# (1) 상용근로자 합계
yangju.worker15
yangju.worker16

# (2) 상용근로자 비율
yangju.workq15
yangju.workq16



## 6. 동탄신도시
am_dongtan <- c(3124061, 3124060, 3124062, 3124063)
dongtan.worker15 <- sum(subset(company15$BR_EMP_T, company15$AD_CLS_CD %in% am_dongtan))
dongtan.worker16 <- sum(subset(company16$BR_EMP_T, company16$AD_CLS_CD %in% am_dongtan))
dongtan.work.tot15 <- sum(subset(company15$BR_EMP_MF_T, company15$AD_CLS_CD %in% am_dongtan))
dongtan.work.tot16 <- sum(subset(company16$BR_EMP_MF_T, company16$AD_CLS_CD %in% am_dongtan))

dongtan.workq15 <- dongtan.worker15 / dongtan.work.tot15
dongtan.workq16 <- dongtan.worker16 / dongtan.work.tot16

# 결과 확인
# (1) 상용근로자 합계
dongtan.worker15
dongtan.worker16

# (2) 상용근로자 비율
dongtan.workq15
dongtan.workq16
