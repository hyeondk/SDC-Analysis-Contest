### 3. 경유 버스 및 노선번호 ###
# 2.Station Information과 연계

## 필요 패키지 설치
if (!require(data.table)){
  install.packages('data.table')
}  
library(data.table)

if(!require(dplyr)){
  install.packages('dplyr')
}
library(dplyr)

# 버스루트 데이터
RT_1 <- fread('RT_1.csv')
RT_2 <- fread('RT_2.csv')
RT_3 <- fread('RT_3.csv')

# 버스루트 리스트 
RT_list <- list()
RT_list[[1]] <- RT_1 
RT_list[[2]] <- RT_2
RT_list[[3]] <- RT_3



## 함수생성 (코드 안이쁨 수정요망)
# bus(버스번호), route(노선번호) 하위 리스트에 저장
# 같은 버스번호에 노선번호가 두개(이상) 할당된 경우 뽑아냄
Bus_Route <- function(station){
  tp <- NULL
  for(i in 1:length(RT_list)){
    list <- RT_list[[i]]
    temp <- subset(list, list$STATION_NM %in% station$STATION_NM)
    tp <- bind_rows(tp, temp)
  }
  bs <- unique(tp$ROUTE_NM)
  for(i in 1:length(bs)){
    a <- subset(tp, tp$ROUTE_NM == bs[i])
    if(length(unique(a$ROUTE_ID)) != 1)
      print(bs[i])
  }
  result <- list(bus = unique(tp$ROUTE_NM), route = unique(tp$ROUTE_ID))
  return(result)
}

# 노선번호를 통한 RT 데이터중 신도시 데이터 추출 
## 함수생성
Allcity_RT <- function(city){
  temp <- city$route
  result <- NULL
  for(i in 1:length(temp)){
    temp1 <- subset(RT_1, RT_1$ROUTE_ID == temp[i])
    temp2 <- subset(RT_2, RT_2$ROUTE_ID == temp[i])
    temp3 <- subset(RT_3, RT_3$ROUTE_ID == temp[i])
    temp4 <- bind_rows(temp1, temp2, temp3)
    result <- bind_rows(result, temp4)
  }
  return(result)
}

##### 2기 신도시 #####
# 각 신도시의 정류장을 지나는 버스와 노선번호
pangyo <- Bus_Route(pangyo_station)
gwanggyo <- Bus_Route(gwanggyo_station)
dongtan <- Bus_Route(dongtan_station)
hangang <- Bus_Route(hangang_station)
unjeong <- Bus_Route(unjeong_station)
wirye <- Bus_Route(wirye_station)
yangju <- Bus_Route(yangju_station)

# 각 신도시를 지나는 버스의 루트
RT_pangyo <- Allcity_RT(pangyo)
RT_gwanggyo <- Allcity_RT(gwanggyo)
RT_dongtan <- Allcity_RT(dongtan)
RT_hangang <- Allcity_RT(hangang)
RT_unjeong <- Allcity_RT(unjeong)
RT_wirye <- Allcity_RT(wirye)
RT_yangju <- Allcity_RT(yangju)


##### 1기 신도시 #####
# 각 신도시의 정류장을 지나는 버스와 노선번호
bundang <- Bus_Route(bundang_station)
ilsan <- Bus_Route(ilsan_station)
pyeongchon <- Bus_Route(pyeongchon_station)
sanbon <- Bus_Route(sanbon_station)
jungdong <- Bus_Route(jungdong_station)

# 각 신도시를 지나는 버스의 루트
RT_bundang <- Allcity_RT(bundang)
RT_ilsan <- Allcity_RT(ilsan)
RT_pyeongchon <- Allcity_RT(pyeongchon)
RT_sanbon <- Allcity_RT(sanbon)
RT_jungdong <- Allcity_RT(jungdong)
