#30,40대 인구 정리
setwd("C:/Users/박소담/Desktop/통계청대회/인구 파일/1612 인구/연령별 인구")
kyonggi <- read.csv("경기도 연령별 인구.csv")

#kyonggi
kyong <- subset(kyonggi, substr(kyonggi$행정구역, 1, 3) == "경기도")
kyonggi.30 <- subset(kyong, 연령구분 == "30세~39세")
kyonggi.40 <- subset(kyong, 연령구분 == "40세~49세")

#2기 신도시
#pangyo
pan1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "삼평동")
pan2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "백현동")
pan3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "판교동")
pan4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "운중동")

pangyo.age <- rbind(pan1, pan2, pan3, pan4)
pangyo.total <- subset(pangyo.age, 연령구분 == "합계")
pangyo.30 <- subset(pangyo.age, 연령구분 == "30세~39세")
pangyo.40 <- subset(pangyo.age, 연령구분 == "40세~49세")

pangyo.r <- {sum(pangyo.30$합계) + sum(pangyo.40$합계)} / sum(pangyo.total$합계)

#gwanggyo
gwang1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "원천동")
gwang2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "광교1동")
gwang3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "광교2동")
gwang4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "상현1동")
gwang5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "상현2동")

gwanggyo.age <- rbind(gwang1, gwang2, gwang3, gwang4, gwang5)
gwanggyo.total <- subset(gwanggyo.age, 연령구분 == "합계")
gwanggyo.30 <- subset(gwanggyo.age, 연령구분 == "30세~39세")
gwanggyo.40 <- subset(gwanggyo.age, 연령구분 == "40세~49세")

gwanggyo.r <- {sum(gwanggyo.30$합계) + sum(gwanggyo.40$합계)} / sum(gwanggyo.total$합계)

#dongtan
dong1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "동탄1동")
dong2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "동탄2동")
dong3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "동탄3동")
dong4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "동탄4동")

dongtan.age <- rbind(dong1, dong2, dong3, dong4)
dongtan.total <- subset(dongtan.age, 연령구분 == "합계")
dongtan.30 <- subset(dongtan.age, 연령구분 == "30세~39세")
dongtan.40 <- subset(dongtan.age, 연령구분 == "40세~49세")

dongtan.r <- {sum(dongtan.30$합계) + sum(dongtan.40$합계)} / sum(dongtan.total$합계)

#hangang
han1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "장기동")
han2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "운양동")
han3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "구래동")

hangang.age <- rbind(han1, han2, han3)
hangang.total <- subset(hangang.age, 연령구분 == "합계")
hangang.30 <- subset(hangang.age, 연령구분 == "30세~39세")
hangang.40 <- subset(hangang.age, 연령구분 == "40세~49세")

hangang.r <- {sum(hangang.30$합계) + sum(hangang.40$합계)} / sum(hangang.total$합계)

#unjeong
un1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "운정1동")
un2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "운정2동")
un3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "운정3동")

unjeong.age <- rbind(un1, un2, un3)
unjeong.total <- subset(unjeong.age, 연령구분 == "합계")
unjeong.30 <- subset(unjeong.age, 연령구분 == "30세~39세")
unjeong.40 <- subset(unjeong.age, 연령구분 == "40세~49세")

unjeong.r <- {sum(unjeong.30$합계) + sum(unjeong.40$합계)} / sum(unjeong.total$합계)

#yangju
yang1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "회천4동")
yang2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "회천2동")

yangju.age <- rbind(yang1, yang2)
yangju.total <- subset(yangju.age, 연령구분 == "합계")
yangju.30 <- subset(yangju.age, 연령구분 == "30세~39세")
yangju.40 <- subset(yangju.age, 연령구분 == "40세~49세")

yangju.r <- {sum(yangju.30$합계) + sum(yangju.40$합계)} / sum(yangju.total$합계)

#1기신도시
#bundang
bun1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "야탑1동")
bun2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "야탑2동")
bun3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "야탑3동")
bun4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "이매1동")
bun5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "이매2동")
bun6 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "서현1동")
bun7 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "서현2동")
bun8 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "분당동")
bun9 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "수내1동")
bun10 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "수내2동")
bun11 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "수내3동")
bun12 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "정자1동")
bun13 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "정자2동")
bun14 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "정자3동")
bun15 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "정자동")
bun16 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "금곡동")
bun17 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "구미1동")

bundang.age <- rbind(bun1, bun2, bun3, bun4, bun5, bun6 ,bun7, bun8, bun9, bun10, bun11, bun12, bun13,
                     bun14, bun15, bun16, bun17)
bundang.total <- subset(bundang.age, 연령구분 == "합계")
bundang.30 <- subset(bundang.age, 연령구분 == "30세~39세")
bundang.40 <- subset(bundang.age, 연령구분 == "40세~49세")

bundang.r <- {sum(bundang.30$합계) + sum(bundang.40$합계)} / sum(bundang.total$합계)

#ilsan
il1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "장항2동")
il2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "마두1동")
il3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "마두2동")
il4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "백석1동")
il5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "백석2동")
il6 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "정발산동")
il7 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "일산1동")
il8 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "일산2동")
il9 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "일산3동")
il10 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "주엽1동")
il11 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "주엽2동")
il12 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "대화동")

ilsan.age <- rbind(il1, il2, il3, il4, il5, il6, il7, il8, il9, il10, il11, il12)
ilsan.total <- subset(ilsan.age, 연령구분 == "합계")
ilsan.30 <- subset(ilsan.age, 연령구분 == "30세~39세")
ilsan.40 <- subset(ilsan.age, 연령구분 == "40세~49세")

ilsan.r <- {sum(ilsan.30$합계) + sum(ilsan.40$합계)} / sum(ilsan.total$합계)

#pyeongchon
pyeong1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "갈산동")
pyeong2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "귀인동")
pyeong3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "범계동")
pyeong4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "부림동")
pyeong5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "부흥동")
pyeong6 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "신촌동")
pyeong7 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "평안동")
pyeong8 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "평촌동")

pyeongchon.age <- rbind(pyeong1, pyeong2, pyeong3, pyeong4, pyeong5, pyeong6, pyeong7, pyeong8)
pyeongchon.total <- subset(pyeongchon.age, 연령구분 == "합계")
pyeongchon.30 <- subset(pyeongchon.age, 연령구분 == "30세~39세")
pyeongchon.40 <- subset(pyeongchon.age, 연령구분 == "40세~49세")

pyeongchon.r <- {sum(pyeongchon.30$합계) + sum(pyeongchon.40$합계)} / sum(pyeongchon.total$합계)

#sanbon
san1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "산본1동")
san2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "산본2동")
san3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "재궁동")
san4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "오금동")
san5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "수리동")
san6 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "궁내동")
san7 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "광정동")

sanbon.age <- rbind(san1, san2, san3, san4, san5, san6, san7)
sanbon.total <- subset(sanbon.age, 연령구분 == "합계")
sanbon.30 <- subset(sanbon.age, 연령구분 == "30세~39세")
sanbon.40 <- subset(sanbon.age, 연령구분 == "40세~49세")

sanbon.r <- {sum(sanbon.30$합계) + sum(sanbon.40$합계)} / sum(sanbon.total$합계)

#jungdong
jung1 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 5) == "중동")
jung2 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "중1동")
jung3 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "중2동")
jung4 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "중3동")
jung5 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "중4동")
jung6 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "심곡3동")
jung7 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "약대동")
jung8 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "송내1동")
jung9 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 7) == "송내2동")
jung10 <- subset(kyonggi, substr(kyonggi$행정구역, 4, 6) == "신흥동")

jungdong.age <- rbind(jung1, jung2, jung3, jung4, jung5, jung6, jung7, jung8, jung9, jung10)
jungdong.total <- subset(jungdong.age, 연령구분 == "합계")
jungdong.30 <- subset(jungdong.age, 연령구분 == "30세~39세")
jungdong.40 <- subset(jungdong.age, 연령구분 == "40세~49세")

jungdong.r <- {sum(jungdong.30$합계) + sum(jungdong.40$합계)} / sum(jungdong.total$합계)

#데이터프레임 만들기
city1.n <- c("Bundang", "Ilsan", "Pyeongchon", "Sanbon", "Jungdong")
city2.n <- c("Pangyo", "Gwanggyo", "Dongtan", "Hangang", "Unjeong", "Yangju")
city1.r <- c(bundang.r, ilsan.r, pyeongchon.r, sanbon.r, jungdong.r)
city2.r <- c(pangyo.r, gwanggyo.r, dongtan.r, hangang.r, unjeong.r, yangju.r)

city1 <- data.frame(matrix(data = NA, nrow = 5, ncol = 2))
city1[,1] <- as.factor(city1.n) 
city1[,2] <- city1.r
colnames(city1) <- c("city", "ratio")

city2 <- data.frame(matrix(data = NA, nrow = 6, ncol = 2))
city2[,1] <- as.factor(city2.n) 
city2[,2] <- city2.r
colnames(city2) <- c("city", "ratio")

#bar plot 그리기
library(ggplot2)
ggplot(data=city1, aes(x = reorder(city, -ratio), y = ratio, colour = city)) + ylim(0,0.4) + geom_col() + ggtitle("1기 신도시 3040 인구 비율")
ggplot(data = city2, aes(x = reorder(city, -ratio), y =ratio, colour = city)) + ylim(0,0.5) + geom_col() + ggtitle("2기 신도시 3040 인구 비율")

#경기도 대비 각 신도시별 3040 인구 비율
##2기 신도시
pangyo.k <- {sum(pangyo.30$합계) + sum(pangyo.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
gwanggyo.k <- {sum(gwanggyo.30$합계) + sum(gwanggyo.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
dongtan.k <- {sum(dongtan.30$합계) + sum(dongtan.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
hangang.k <- {sum(hangang.30$합계) + sum(hangang.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
unjeong.k <- {sum(unjeong.30$합계) + sum(unjeong.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
yangju.k <- {sum(yangju.30$합계) + sum(yangju.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}

#1기 신도시
bundang.k <- {sum(bundang.30$합계) + sum(bundang.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
ilsan.k <- {sum(ilsan.30$합계) + sum(ilsan.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
pyeongchon.k <- {sum(pyeongchon.30$합계) + sum(pyeongchon.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
sanbon.k <- {sum(sanbon.30$합계) + sum(sanbon.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}
jungdong.k <- {sum(jungdong.30$합계) + sum(jungdong.40$합계)} / {sum(kyonggi.30$합계) + sum(kyonggi.40$합계)}

#데이터프레임 만들기
city1.n <- c("Bundang", "Ilsan", "Pyeongchon", "Sanbon", "Jungdong")
city2.n <- c("Pangyo", "Gwanggyo", "Dongtan", "Hangang", "Unjeong", "Yangju")
city1.kk <- c(bundang.k, ilsan.k, pyeongchon.k, sanbon.k, jungdong.k)
city2.kk <- c(pangyo.k, gwanggyo.k, dongtan.k, hangang.k, unjeong.k, yangju.k)

city1.k <- data.frame(matrix(data = NA, nrow = 5, ncol = 2))
city1.k[,1] <- as.factor(city1.n) 
city1.k[,2] <- city1.kk
colnames(city1.k) <- c("city", "ratio")

city2.k <- data.frame(matrix(data = NA, nrow = 6, ncol = 2))
city2.k[,1] <- as.factor(city2.n) 
city2.k[,2] <- city2.kk
colnames(city2.k) <- c("city", "ratio")

#bar plot 그리기
library(ggplot2)
ggplot(data=city1.k, aes(x = reorder(city, -ratio), y = ratio, colour = city)) + ylim(0,0.05) + geom_col() + ggtitle("경기도 대비 1기 신도시 3040 인구 비율")
ggplot(data = city2.k, aes(x = reorder(city, -ratio), y =ratio, colour = city)) + ylim(0,0.05) + geom_col() + ggtitle("경기도 대비 2기 신도시 3040 인구 비율")

#30,40대 수
pangyo.n <- pangyo.30 + pangyo.40
