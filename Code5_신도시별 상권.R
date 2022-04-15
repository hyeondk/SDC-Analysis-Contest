##### Code6 - 신도시별 상권 #####

# 신도시별 상권을 추출하고자 함.
# Code1과 연계됨.

##### 작업 1 : 신도시별 상권코드 추출 #####

### 1기 신도시 ###

## 1. 분당신도시 - 상권
bz_bundang <- unique(bundang$BZ_CD)
bz_bundang <- subset(bz_bundang, substr(bz_bundang, 2, 2) == "_")
bz_bundang
length(bz_bundang)

## 2. 일산신도시 - 상권
bz_ilsan <- unique(ilsan$BZ_CD)
bz_ilsan <- subset(bz_ilsan, substr(bz_ilsan, 2, 2) == "_")
bz_ilsan
length(bz_ilsan)

## 3. 평촌신도시 - 상권
bz_pyeongchon <- unique(pyeongchon$BZ_CD)
bz_pyeongchon <- subset(bz_pyeongchon, substr(bz_pyeongchon, 2, 2) == "_")
bz_pyeongchon
length(bz_pyeongchon)

## 4. 산본신도시 - 상권
bz_sanbon <- unique(sanbon$BZ_CD)
bz_sanbon <- subset(bz_sanbon, substr(bz_sanbon, 2, 2) == "_")
bz_sanbon
length(bz_sanbon)

## 5. 중동신도시 - 상권
bz_jungdong <- unique(jungdong$BZ_CD)
bz_jungdong <- subset(bz_jungdong, substr(bz_jungdong, 2, 2) == "_")
bz_jungdong
length(bz_jungdong)



### 2기 신도시 ###

## 1. 판교신도시 - 상권
bz_pangyo <- unique(pangyo$BZ_CD)
bz_pangyo <- subset(bz_pangyo, substr(bz_pangyo, 2, 2) == "_")
bz_pangyo
length(bz_pangyo)

## 2. 한강신도시 - 상권
bz_hangang <- unique(hangang$BZ_CD)
bz_hangang <- subset(bz_hangang, substr(bz_hangang, 2, 2) == "_")
bz_hangang
length(bz_hangang)

## 3. 운정신도시 - 상권
bz_unjeong <- unique(unjeong$BZ_CD)
bz_unjeong <- subset(bz_unjeong, substr(bz_unjeong, 2, 2) == "_")
bz_unjeong
length(bz_unjeong)

## 4. 광교신도시 - 상권
bz_gwanggyo <- unique(gwanggyo$BZ_CD)
bz_gwanggyo <- subset(bz_gwanggyo, substr(bz_gwanggyo, 2, 2) == "_")
bz_gwanggyo
length(bz_gwanggyo)

## 5. 양주신도시 - 상권
bz_yangju <- unique(yangju$BZ_CD)
bz_yangju <- subset(bz_yangju, substr(bz_yangju, 2, 2) == "_")
bz_yangju
length(bz_yangju)

## 6. 동탄신도시 - 상권
bz_dongtan <- unique(dongtan$BZ_CD)
bz_dongtan <- subset(bz_dongtan, substr(bz_dongtan, 2, 2) == "_")
bz_dongtan
length(bz_dongtan)


#########################################################################################

##### 각 신도시별 상권명 확인 #####
# 위에서 추출한 각 신도시별 상권 코드 이용.

## 1000대 상권 원데이터 호출
if(!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

saledata <- fread("1000대 상권.csv", stringsAsFactors = F)
str(saledata)



### 1기 신도시 ###
findsale_nt1 <- function(x = saledata) {
  bundang.sale <<- NULL; ilsan.sale <<- NULL; pyeongchon.sale <<- NULL
  sanbon.sale <<- NULL; jungdong.sale <<- NULL
  
  bundang.sale <<- rbind(bundang.sale, subset(x, x$BZ_CD %in% bz_bundang))
  ilsan.sale <<- rbind(ilsan.sale, subset(x, x$BZ_CD %in% bz_ilsan))
  pyeongchon.sale <<- rbind(pyeongchon.sale, subset(x, x$BZ_CD %in% bz_pyeongchon))
  sanbon.sale <<- rbind(sanbon.sale, subset(x, x$BZ_CD %in% bz_sanbon))
  jungdong.sale <<- rbind(jungdong.sale, subset(x, x$BZ_CD %in% bz_jungdong))
}

findsale_nt1()

# 결과 확인
bundang.sale
ilsan.sale
pyeongchon.sale
sanbon.sale
jungdong.sale



### 2기 신도시 ###
findsale_nt2 <- function(x = saledata) {
  pangyo.sale <<- NULL; hangang.sale <<- NULL; unjeong.sale <<- NULL
  gwanggyo.sale <<- NULL; yangju.sale <<- NULL; dongtan.sale <<- NULL
  
  pangyo.sale <<- rbind(pangyo.sale, subset(x, x$BZ_CD %in% bz_pangyo))
  hangang.sale <<- rbind(hangang.sale, subset(x, x$BZ_CD %in% bz_hangang))
  unjeong.sale <<- rbind(unjeong.sale, subset(x, x$BZ_CD %in% bz_unjeong))
  gwanggyo.sale <<- rbind(gwanggyo.sale, subset(x, x$BZ_CD %in% bz_gwanggyo))
  yangju.sale <<- rbind(yangju.sale, subset(x, x$BZ_CD %in% bz_yangju))
  dongtan.sale <<- rbind(dongtan.sale, subset(x, x$BZ_CD %in% bz_dongtan))
}

findsale_nt2()

# 결과 확인
pangyo.sale
hangang.sale
unjeong.sale
gwanggyo.sale
yangju.sale
dongtan.sale