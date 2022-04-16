### 4. 각도시버스노선의 정류장 중 서울소재 정류장 추출 ###
# 3. bus route와 연계

seoul_station <- fread('seoul bus station.csv', encoding = 'UTF-8')
str(seoul_station)

##### 2기 신도시 #####
# 서울 정류장 중 신도시를 지나는 버스가 경유하는 정류장 인덱스
temp1 <- seoul_station$'정류소명' %in% RT_pangyo$STATION_NM
temp2 <- seoul_station$'정류소명' %in% RT_gwanggyo$STATION_NM
temp3 <- seoul_station$'정류소명' %in% RT_dongtan$STATION_NM
temp4 <- seoul_station$'정류소명' %in% RT_hangang$STATION_NM
temp5 <- seoul_station$'정류소명' %in% RT_unjeong$STATION_NM
temp6 <- seoul_station$'정류소명' %in% RT_wirye$STATION_NM
temp7 <- seoul_station$'정류소명' %in% RT_yangju$STATION_NM

# 서울 정류장 중 신도시를 지나는 버스가 경유하는 정류장
pangyo_seoul <- seoul_station[temp1]
gwanggyo_seoul <- seoul_station[temp2]
dongtan_seoul <- seoul_station[temp3]
hangang_seoul <- seoul_station[temp4]
unjeong_seoul <- seoul_station[temp5]
wirye_seoul <- seoul_station[temp6]
yangju_seoul <- seoul_station[temp7]

# 갯수 확인
str(pangyo_seoul) # 360개
str(gwanggyo_seoul) # 326개
str(dongtan_seoul) # 166개
str(hangang_seoul) # 239개
str(unjeong_seoul) # 264개
str(yangju_seoul) # 76개

str(wirye_seoul) # 453개

##### 1기 신도시 #####
# 서울 정류장 중 신도시를 지나는 버스가 경유하는 정류장 인덱스
temp8 <- seoul_station$'정류소명' %in% RT_bundang$STATION_NM
temp9 <- seoul_station$'정류소명' %in% RT_ilsan$STATION_NM
temp10 <- seoul_station$'정류소명' %in% RT_pyeongchon$STATION_NM
temp11 <- seoul_station$'정류소명' %in% RT_sanbon$STATION_NM
temp12 <- seoul_station$'정류소명' %in% RT_jungdong$STATION_NM

# 서울 정류장 중 신도시를 지나는 버스가 경유하는 정류장
bundang_seoul <- seoul_station[temp8]
ilsan_seoul <- seoul_station[temp9]
pyeongchon_seoul <- seoul_station[temp10]
sanbon_seoul <- seoul_station[temp11]
jungdong_seoul <- seoul_station[temp12]

# 갯수 확인
str(bundang_seoul)
str(ilsan_seoul)
str(pyeongchon_seoul)
str(sanbon_seoul)
str(jungdong_seoul)
