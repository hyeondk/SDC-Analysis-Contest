##### Code1 - 신도시별 소지역 코드 분류 #####

## Set and Get working directory
setwd("D:/최종욱(cjw0107)/TAKE_OUT_FILES/data analysis")
getwd()

## 소지역 코드 원데이터 호출
rawdata <- read.csv("소지역 코드.csv", stringsAsFactors = F)
rawdata$BLOCK_CD <- as.numeric(gsub(",", "", rawdata$BLOCK_CD)) # ","를 ""로 모두 일괄적 처리
head(rawdata)

## 소지역 코드 - 경기도만 추출
rawdata_g <- subset(rawdata, rawdata$SIDO_NM == "경기도")
head(rawdata_g)


#################################################################################################################################

##### 1기 신도시 - 소지역 코드 분류 #####

## 1. 소지역 코드 - 분당신도시
# 행정동 : (성남시 분당구) *금곡동, 구미1동
# 법정동 : (성남시 분당구) 야탑동, 이매동, 서현동, 분당동, 수내동, *정자동

# 겹침 주의!!!
# 정자동 : 경기 수원시 장안구 정자동
# 금곡동 : 경기 수원시 권선구 금곡동, 경기 화성시 금곡동

# (1) 성남시 분당구만 우선 추출
rawdata_g_bundang <- subset(rawdata_g, rawdata_g$SGNG_NM == "성남시 분당구")

# (2) 해당 행정동과 법정동만 추출
bundang <- subset(rawdata_g_bundang, rawdata_g_bundang$ADONG_NM %in% c("금곡동", "구미1동"))
bundang <- rbind(bundang, subset(rawdata_g_bundang, rawdata_g_bundang$LDONG_NM %in% c("야탑동", "이매동", "서현동", "분당동", "수내동", "정자동")))

# 분당신도시 BLOCK_CD 추출
cd_bundang <- unique(bundang$BLOCK_CD)
head(cd_bundang)
str(cd_bundang)



## 2. 소지역 코드 - 일산신도시
# 행정동 : (고양시 일산동구) 장항2동, 백석1동, 백석2동, 정발산동, (고양시 일산서구) 대화동
# 법정동 : (고양시 일산동구) 마두동, (고양시 일산서구) 일산동, 주엽동

# 겹치는 행정동이나 법정동은 없음.

ilsan <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("장항2동", "백석1동", "백석2동", "정발산동", "대화동"))
ilsan <- rbind(ilsan, subset(rawdata_g, rawdata_g$LDONG_NM %in% c("마두동", "일산동", "주엽동")))

# 일산신도시 BLOCK_CD 추출
cd_ilsan <- unique(ilsan$BLOCK_CD)
head(cd_ilsan)
str(cd_ilsan)



## 3. 소지역 코드 - 평촌신도시
# 행정동 : (안양시 동안구) *갈산동, 귀인동, 범계동, *부림동, 부흥동, *신촌동, 평안동, 평촌동

# 겹침 주의 !!
# 갈산동 : 경기 이천시 갈산동
# 부림동 : 경기 과천시 부림동
# 신촌동 : 경기 성남시 수정구 신촌동, 경기 파주시 신촌동

# (1) 안양시 동안구만 우선 추출
rawdata_g_dongan <- subset(rawdata_g, rawdata_g$SGNG_NM == "안양시 동안구")

# (2) 해당 행정동만 추출
pyeongchon <- subset(rawdata_g_dongan, rawdata_g_dongan$ADONG_NM %in% c("갈산동", "귀인동", "범계동", "부림동", "부흥동", "신촌동", "평안동", "평촌동"))

# 평촌신도시 BLOCK_CD 추출
cd_pyeongchon <- unique(pyeongchon$BLOCK_CD)
head(cd_pyeongchon)
str(cd_pyeongchon)



## 4. 소지역 코드 - 산본신도시
# 행정동 : (군포시) 산본1동, 산본2동, 재궁동, *오금동, 수리동, *궁내동, 광정동

# 겹침 주의!!
# 오금동 : 경기 여주시 오금동
# 궁내동 : 경기 성남시 분당구 궁내동

# (1) 군포시만 우선 추출
rawdata_g_gunpo <- subset(rawdata_g, rawdata_g$SGNG_NM == "군포시")

# (2) 해당 행정동만 추출
sanbon <- subset(rawdata_g_gunpo, rawdata_g_gunpo$ADONG_NM %in% c("산본1동", "산본2동", "재궁동", "오금동", "수리동", "궁내동", "광정동"))

# 산본신도시 BLOCK_CD 추출
cd_sanbon <- unique(sanbon$BLOCK_CD)
head(cd_sanbon)
str(cd_sanbon)



## 5. 소지역 코드 - 중동신도시
# 행정동 : (부천시) *중동, 중1동, 중2동, 중3동, 중4동, 심곡3동, 약대동, *신흥동
# 법정동 : *송내동

# 겹침 주의!!
# 중동 : 경기 수원시 팔달구 중동, 경기 용인시 기흥구 중동, 경기 화성시 중동
# 송내동 : 경기 동두천시 송내동
# 신흥동 : 경기 성남시 수정구 신흥동, 경기 안성시 신흥동

# (1) 부천시만 우선 추출
rawdata_g_bucheon <- subset(rawdata_g, rawdata_g$SGNG_NM == "부천시")

# (2) 해당 행정동과 법정동만 추출
jungdong <- subset(rawdata_g_bucheon, rawdata_g_bucheon$ADONG_NM %in% c("중동", "중1동", "중2동", "중3동", "중4동", "심곡3동", "약대동", "신흥동"))
jungdong <- rbind(jungdong, subset(rawdata_g_bucheon, rawdata_g_bucheon$LDONG_NM == "송내동"))

# 중동신도시 BLOCK_CD 추출
cd_jungdong <- unique(jungdong$BLOCK_CD)
head(cd_jungdong)
str(cd_jungdong)


#################################################################################################################################

##### 2기 신도시 - 소지역 코드 분류 #####

## 1. 소지역 코드 - 판교신도시
# 행정동 : (성남시 분당구) 삼평동, 백현동, 판교동, 운중동
pangyo <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("삼평동", "백현동", "판교동", "운중동"))

# 판교신도시 BLOCK_CD 추출
cd_pangyo <- unique(pangyo$BLOCK_CD)
head(cd_pangyo)
str(cd_pangyo)



## 2. 소지역 코드 - 한강신도시
# 행정동 : (김포시) 장기동, 운양동, 구래동
hangang <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("장기동", "운양동", "구래동"))

# 한강신도시 BLOCK_CD 추출
cd_hangang <- unique(hangang$BLOCK_CD)
head(cd_hangang)
str(cd_hangang)



## 3. 소지역 코드 - 운정신도시
# 행정동 : (파주시) 운정1동, 운정2동, 운정3동
unjeong <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("운정1동", "운정2동", "운정3동"))

# 운정신도시 BLOCK_CD 추출
cd_unjeong <- unique(unjeong$BLOCK_CD)
head(cd_unjeong)
str(cd_unjeong)



## 4. 소지역 코드 - 광교신도시
# 행정동 : (수원시 영통구) 원천동, 광교1동, 광교2동, (용인시 수지구) 상현1동, 상현2동
gwanggyo <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("원천동", "광교1동", "광교2동", "상현1동", "상현2동"))

# 광교신도시 BLOCK_CD 추출
cd_gwanggyo <- unique(gwanggyo$BLOCK_CD)
head(cd_gwanggyo)
str(cd_gwanggyo)



## 5. 소지역 코드 - 양주신도시
# 법정동 : (양주시) 옥정동, 회정동
yangju <- subset(rawdata_g, rawdata_g$LDONG_NM %in% c("옥정동", "회정동"))

# 양주신도시 BLOCK_CD 추출
cd_yangju <- unique(yangju$BLOCK_CD)
head(cd_yangju)
str(cd_yangju)



## 6. 소지역 코드 - 동탄신도시
# 행정동 : (화성시) 동탄1동, 동탄2동, 동탄3동, 동탄4동
dongtan <- subset(rawdata_g, rawdata_g$ADONG_NM %in% c("동탄1동", "동탄2동", "동탄3동", "동탄4동"))

# 동탄신도시 BLOCK_CD 추출
cd_dongtan <- unique(dongtan$BLOCK_CD)
head(cd_dongtan)
str(cd_dongtan)
