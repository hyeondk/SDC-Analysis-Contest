### 2. 정류장 정보 ###
# 1. bus station crwaling과 연계

## fread 사용을 위한 패키지
if (!require(data.table)){
  install.packages('data.table')
}  
library(data.table)

# 정류장 정보 csv파일 로드
stationIF <- fread('stationIF.csv')

## 함수생성
station <- function(Allstation){
  citystation <- subset(stationIF, stationIF$STATION_NM %in% Allstation)
  temp <- unique(Allstation) %in% unique(citystation$STATION_NM)
  if(FALSE %in% temp){
    cat('미포함정류장:', unique(Allstation)[!temp])
  }
  return(citystation)
}

##### 2기 신도시 #####

### 판교신도시
pangyo_station <- station(Allpangyo_st)
str(pangyo_station)
unique(pangyo_station$STATION_NM)

### 광교신도시
gwanggyo_station <- station(Allgwanggyo_st)
str(pangyo_station)
unique(gwanggyo_station$STATION_NM)


### 동탄신도시
dongtan_station <- station(Alldongtan_st)
str(dongtan_station)
unique(dongtan_station$STATION_NM)

### 위례신도시
wirye_station <- station(Allwirye_st)
str(wirye_station)
unique(wirye_station$STATION_NM)

### 운정신도시
unjeong_station <- station(Allunjeong_st)
str(unjeong_station)
unique(unjeong_station$STATION_NM)

### 한강신도시
hangang_station <- station(Allhangang_st)
str(hangang_station)
unique(hangang_station$STATION_NM)

### 양주신도시
yangju_station <- station(Allyangju_st)
str(yangju_station)
unique(yangju_station$STATION_NM)

##### 1기 신도시 #####

### 분당신도시
bundang_station <- station(Allbundang_st)
str(bundang_station)
unique(bundang_station$STATION_NM)

### 일산신도시
ilsan_station <- station(Allilsan_st)
str(ilsan_station)
unique(ilsan_station$STATION_NM)

### 평촌신도시
pyeongchon_station <- station(Allpyeongchon_st)
str(pyeongchon_station)
unique(pyeongchon_station$STATION_NM)

### 산본신도시
sanbon_station <- station(Allsanbon_st)
str(sanbon_station)
unique(sanbon_station$STATION_NM)

### 중동신도시
jungdong_station <- station(Alljungdong_st)
str(jungdong_station)
unique(jungdong_station$STATION_NM)
