### 1. 정류장 크롤링 (다음) ###
# 검색결과가 일정하지 않음을 유의

### 필요 패키지 설치
if(!require(rvest)){
  install.packages('rvest')
}
if(!require(dplyr)){
  install.packages('dplyr')
}
library(rvest)
library(dplyr)

## 함수생성 (다음검색)
make_st <- function(url){
  html <- read_html(url)
  temp <- html %>% html_nodes('.info') %>% html_nodes('a') %>% html_attr('data-name')
  temp <- temp[!is.na(temp)]
  return(temp)
}

## 함수생성 (네이버검색)
make_st.n <- function(url){
  html <- read_html(url)
  temp <- html %>% html_nodes('.k_bustop_info') %>% html_nodes('a') %>% html_attr('title')
  return(temp)
}

##### 2기 신도시 #####

## (다음) 경기 ''시 ''동 정류장 로 검색한 url

### 판교신도시 (경기도 성남시)
# 삼평동
url_sampyung <-'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%82%BC%ED%8F%89%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 백현동
url_beakhyun <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EB%B0%B1%ED%98%84%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 판교동
url_pangyo <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%ED%8C%90%EA%B5%90%EB%8F%99++%EC%A0%95%EB%A5%98%EC%9E%A5'
# 운중동
url_unjung <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EC%9A%B4%EC%A4%91%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

sampyung_st <- make_st(url_sampyung)
sampyung_st
beakhyun_st <- make_st(url_beakhyun)
beakhyun_st
pangyo_st <- make_st(url_pangyo)
pangyo_st
unjung_st <- make_st(url_unjung)
unjung_st

Allpangyo_st <- c(sampyung_st, beakhyun_st, pangyo_st, unjung_st)
unique(Allpangyo_st)

### 광교 신도시(경기도 수원시, 용인시)
#수원시 원천동
url_wc <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%88%98%EC%9B%90%EC%8B%9C+%EC%9B%90%EC%B2%9C%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 수원시 광교1동
url_gg1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%88%98%EC%9B%90%EC%8B%9C+%EA%B4%91%EA%B5%901%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 수원시 광교2동
url_gg2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%88%98%EC%9B%90%EC%8B%9C+%EA%B4%91%EA%B5%902%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 용인시 상현1동
url_sh1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%9A%A9%EC%9D%B8%EC%8B%9C++%EC%83%81%ED%98%841%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 용인시 상현2동
url_sh2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%9A%A9%EC%9D%B8%EC%8B%9C+%EC%83%81%ED%98%842%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5' 

woncheon_st <-make_st(url_wc)
woncheon_st
gwanggyo1_st <- make_st(url_gg1)
gwanggyo1_st
gwanggyo2_st <- make_st(url_gg2)
gwanggyo2_st
sanghyeon1_st <- make_st(url_sh1)
sanghyeon1_st
sanghyeon2_st <- make_st(url_sh2)
sanghyeon2_st

Allgwanggyo_st <- c(woncheon_st, gwanggyo1_st, gwanggyo2_st, sanghyeon1_st, sanghyeon2_st)
unique(Allgwanggyo_st)

### 동탄 1신도시 (경기도 화성시)
#동탄1동
url_dt1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%841%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 동탄2동
url_dt2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%842%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 동탄3동
url_dt3 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%843%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

dongtan1 <- make_st(url_dt1)
dongtan1
dongtan2 <- make_st(url_dt2)
dongtan2
dongtan3 <- make_st(url_dt3)
dongtan3

Alldongtan1_st <- c(dongtan1, dongtan2, dongtan3)
unique(Alldongtan1_st)

### 동탄 2신도시 (경기도 화성시)
# 동탄4동
url_dt4 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%844%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 동탄5동 
url_dt5 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%845%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 동탄5동
url_dt6 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%99%94%EC%84%B1%EC%8B%9C+%EB%8F%99%ED%83%846%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

dongtan4 <- make_st(url_dt4)
dongtan4
dongtan5 <- make_st(url_dt5)
dongtan5
dongtan6 <- make_st(url_dt6)
dongtan6

Alldongtan2_st <- c(dongtan4, dongtan5, dongtan6)
unique(Alldongtan2_st)
Alldongtan_st <- c(Alldongtan1_st, Alldongtan2_st)
unique(Alldongtan_st)

### 위례 신도시 (서울시, 성남시, 하남시)
# 위례 서울시
url_wr_songpa <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EC%84%9C%EC%9A%B8+%EC%86%A1%ED%8C%8C%EA%B5%AC+%EC%9C%84%EB%A1%80%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 위례 성남시
url_wr_sungnam <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=+%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EC%9C%84%EB%A1%80%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 위례 하남시
url_wr_hanam <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%95%98%EB%82%A8%EC%8B%9C+%EC%9C%84%EB%A1%80%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

wirye_songpa_st <- make_st(url_wr_songpa)
wirye_songpa_st
#송파는 경기도가 아니라 없을 수 있음
wirye_sungnam_st <- make_st(url_wr_sungnam)
wirye_sungnam_st
wirye_hanam_st <- make_st(url_wr_hanam)
wirye_hanam_st

Allwirye_st <- c(wirye_songpa_st, wirye_sungnam_st ,wirye_hanam_st)
unique(Allwirye_st)

### 운정 신도시 (경기도 파주시)
# 운정1동
url_unjeong1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%8C%8C%EC%A3%BC%EC%8B%9C+%EC%9A%B4%EC%A0%951%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 운정2동
url_unjeong2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%8C%8C%EC%A3%BC%EC%8B%9C+%EC%9A%B4%EC%A0%952%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
#운정3동
url_unjeong3 <-'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%ED%8C%8C%EC%A3%BC%EC%8B%9C+%EC%9A%B4%EC%A0%953%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

unjeong1_st <- make_st(url_unjeong1)
unjeong1_st
unjeong2_st <- make_st(url_unjeong2)
unjeong2_st
unjeong3_st <- make_st(url_unjeong3)
unjeong3_st

Allunjeong_st <- c(unjeong1_st, unjeong2_st, unjeong3_st)
unique(Allunjeong_st)

### 한강 신도시 (경기도 김포시)
# 장기동
url_janggi <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B9%80%ED%8F%AC%EC%8B%9C+%EC%9E%A5%EA%B8%B0%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 운양동
url_unyang <-'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B9%80%ED%8F%AC%EC%8B%9C+%EC%9A%B4%EC%96%91%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 구래동
url_gurae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B9%80%ED%8F%AC%EC%8B%9C+%EA%B5%AC%EB%9E%98%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
#마산동
url_masan <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B9%80%ED%8F%AC%EC%8B%9C+%EB%A7%88%EC%82%B0%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

janggi_st <- make_st(url_janggi)
janggi_st
unyang_st <- make_st(url_unyang)
unyang_st
gurae_st <- make_st(url_gurae)
gurae_st
masan_st <- make_st(url_masan)
masan_st

Allhangang_st <- c(janggi_st, unyang_st, gurae_st, masan_st)
unique(Allhangang_st)

### 양주 신도시
# 옥정동
url_okjeong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%96%91%EC%A3%BC%EC%8B%9C+%EC%98%A5%EC%A0%95%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 회정동
url_hoejeong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%96%91%EC%A3%BC%EC%8B%9C+%ED%9A%8C%EC%A0%95%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

okjeong_st <- make_st(url_okjeong)
okjeong_st
hoejeong_st <- make_st(url_hoejeong)
hoejeong_st

Allyangju_st <- c(okjeong_st, hoejeong_st)
unique(Allyangju_st)


##### 1기 신도시 #####
## (다음) 경기 ''시 ''동 정류장 로 검색한 url

### 분당 신도시
# 야탑동
url_yatap <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%95%BC%ED%83%91%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 이매동
url_imae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%9D%B4%EB%A7%A4%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 서현동
url_seohyeon <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%84%9C%ED%98%84%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 분당동
url_bundang <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EB%B6%84%EB%8B%B9%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 수내동
url_sunae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%88%98%EB%82%B4%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 정자동
url_jeongja <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EC%A0%95%EC%9E%90%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 구미1동 
url_gumi1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%84%B1%EB%82%A8%EC%8B%9C+%EB%B6%84%EB%8B%B9%EA%B5%AC+%EA%B5%AC%EB%AF%B81%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

yatap_st <- make_st(url_yatap)
yatap_st
imae_st <- make_st(url_imae)
imae_st
seohyeon_st <- make_st(url_seohyeon)
seohyeon_st
bundang_st <- make_st(url_bundang)
bundang_st
sunae_st <- make_st(url_sunae)
sunae_st
jeongja_st <- make_st(url_jeongja)
jeongja_st
gumi1_st <- make_st(url_gumi1)
gumi1_st


Allbundang_st <- c(yatap_st, imae_st, seohyeon_st, bundang_st, sunae_st, jeongja_st, gumi1_st)
unique(Allbundang_st)

### 일산 신도시 (동구, 서구)
# 장항2동
url_janghang2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EB%8F%99%EA%B5%AC+%EC%9E%A5%ED%95%AD2%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 마두동
url_madu <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EB%8F%99%EA%B5%AC+%EB%A7%88%EB%91%90%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 백석1동 
url_baekseok1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EB%8F%99%EA%B5%AC+%EB%B0%B1%EC%84%9D1%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 백석2동
url_baekseok2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EB%8F%99%EA%B5%AC+%EB%B0%B1%EC%84%9D2%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 정발산동 
url_jeongbalsan <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EB%8F%99%EA%B5%AC+%EC%A0%95%EB%B0%9C%EC%82%B0%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 일산1동
url_ilsan1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EC%84%9C%EA%B5%AC+%EC%9D%BC%EC%82%B01%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 일산2동
url_ilsan2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EC%84%9C%EA%B5%AC+%EC%9D%BC%EC%82%B02%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 일산3동
url_ilsan3 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EC%84%9C%EA%B5%AC+%EC%9D%BC%EC%82%B03%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 주엽동
url_juyeop <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EC%84%9C%EA%B5%AC+%EC%A3%BC%EC%97%BD%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 대화동 
url_deahwa <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B3%A0%EC%96%91%EC%8B%9C+%EC%9D%BC%EC%82%B0%EC%84%9C%EA%B5%AC+%EB%8C%80%ED%99%94%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

janghang2_st <- make_st(url_janghang2)
janghang2_st
madu_st <- make_st(url_madu)
madu_st
baekseok1_st <- make_st(url_baekseok1)
baekseok1_st
baekseok2_st <- make_st(url_baekseok2)
baekseok2_st
jeongbalsan_st <- make_st(url_jeongbalsan)
jeongbalsan_st
ilsan1_st <- make_st(url_ilsan1)
ilsan1_st
ilsan2_st <- make_st(url_ilsan2)
ilsan2_st
ilsan3_st <- make_st(url_ilsan3)
ilsan3_st
juyeop_st <- make_st(url_juyeop)
juyeop_st
deahwa_st <- make_st(url_deahwa)
deahwa_st

Allilsan_st <- c(janghang2_st, madu_st, baekseok1_st, baekseok2_st, jeongbalsan_st, ilsan1_st, ilsan2_st, ilsan3_st, juyeop_st, deahwa_st)
unique(Allilsan_st)

### 평촌신도시
# 갈산동
url_galsan <-'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EA%B0%88%EC%82%B0%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 귀인동
url_gwiin <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EA%B7%80%EC%9D%B8%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 범계동
url_beomgye <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EB%B2%94%EA%B3%84%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 부림동
url_burim <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EB%B6%80%EB%A6%BC%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 부흥동
url_buheung <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EB%B6%80%ED%9D%A5%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 신촌동
url_sinchon <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%EC%8B%A0%EC%B4%8C%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 평안동
url_pyeongan <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%ED%8F%89%EC%95%88%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 평촌동
url_pyeongchon <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EC%95%88%EC%96%91%EC%8B%9C+%EB%8F%99%EC%95%88%EA%B5%AC+%ED%8F%89%EC%B4%8C%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

galsan_st <- make_st(url_galsan)
galsan_st
gwiin_st <- make_st(url_gwiin)
gwiin_st
beomgye_st <- make_st(url_beomgye)
beomgye_st
burim_st <- make_st(url_burim)
burim_st
buheung_st <- make_st(url_buheung)
buheung_st
sinchon_st <- make_st(url_sinchon)
sinchon_st
pyeongan_st <- make_st(url_pyeongan)
pyeongan_st
pyeongchon_st <- make_st(url_pyeongchon)
pyeongchon_st

Allpyeongchon_st <- c(galsan_st, gwiin_st, beomgye_st, burim_st, buheung_st, sinchon_st, pyeongan_st, pyeongchon_st)
unique(Allpyeongchon_st)

### 산본신도시
# 산본1동
url_sanbon1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EC%82%B0%EB%B3%B81%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 산본2동
url_sanbon2 <-'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EC%82%B0%EB%B3%B82%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 재궁동
url_jaegung <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EC%9E%AC%EA%B6%81%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 오금동
url_ogeum <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EC%98%A4%EA%B8%88%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 수리동
url_suri <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EC%88%98%EB%A6%AC%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 궁내동
url_gungnae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EA%B6%81%EB%82%B4%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 광정동
url_gwangjeong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EA%B5%B0%ED%8F%AC%EC%8B%9C+%EA%B4%91%EC%A0%95%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

sanbon1_st <- make_st(url_sanbon1)
sanbon1_st
sanbon2_st <- make_st(url_sanbon2)
sanbon2_st
jaegung_st <- make_st(url_jaegung)
jaegung_st
ogeum_st <- make_st(url_ogeum)
ogeum_st
suri_st <- make_st(url_suri)
suri_st
gungnae_st <- make_st(url_gungnae)
gungnae_st
gwangjeong_st <- make_st(url_gwangjeong)
gwangjeong_st

Allsanbon_st <- c(sanbon1_st, sanbon2_st, jaegung_st, ogeum_st, suri_st, gungnae_st, gwangjeong_st)
unique(Allsanbon_st)

### 중동신도시
# 중동
url_jungdong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%A4%91%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 중1동 
url_jung1 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%A4%911%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 중2동
url_jung2 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%A4%912%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 중3동
url_jung3 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%A4%913%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 중4동
url_jung4 <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%A4%914%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 심곡동
url_simgok <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%8B%AC%EA%B3%A1%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 약대동
url_yakdae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%95%BD%EB%8C%80%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 송내동
url_songnae <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%86%A1%EB%82%B4%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 내동
url_neadong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EB%82%B4%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'
# 삼정동
url_samjeong <- 'https://search.daum.net/search?nil_suggest=btn&w=tot&DA=SBC&q=%EA%B2%BD%EA%B8%B0+%EB%B6%80%EC%B2%9C%EC%8B%9C+%EC%82%BC%EC%A0%95%EB%8F%99+%EC%A0%95%EB%A5%98%EC%9E%A5'

jungdong_st <- make_st(url_jungdong)
jungdong_st
jung1_st <- make_st(url_jung1)
jung1_st
jung2_st <- make_st(url_jung2)
jung2_st
jung3_st <- make_st(url_jung3)
jung3_st
jung4_st <- make_st(url_jung4)
jung4_st
simgok_st <- make_st(url_simgok)
simgok_st
songnae_st <- make_st(url_songnae)
songnae_st
naedong_st <- make_st(url_neadong)
naedong_st
samjeong_st <- make_st(url_samjeong)
samjeong_st
Alljungdong_st <- c(jungdong_st, jung1_st,  jung2_st, jung3_st, jung4_st, simgok_st, songnae_st, naedong_st, samjeong_st)
unique(Alljungdong_st)
