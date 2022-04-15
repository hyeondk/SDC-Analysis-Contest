#2기신도시 좌표
name <- c("pangyo", "gwanggyo", "dongtan", "hangang", "unjeong", "yangju")
lon <- c(127.111097, 127.051677, 127.087928, 126.675857, 126.748158, 127.077420)
lat <- c(37.394994, 37.288628, 37.203887, 37.648854, 37.720547, 37.815504)
geo <- data.frame(name = name, lon = lon, lat = lat)

#구글맵 그리기
library(ggmap)

cen <- c(mean(geo$lon), mean(geo$lat))

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=geo, aes(x=lon,y=lat),col = 'navy',size = 0.7, na.rm = T)


#mydate
makedate <- function(x = 15, y = 16, z = 17) {
   mydate <<- NULL
   for(yy in c(x, y, z)) {
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

makedate()
mydate

#판교유동인구
pangyo.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
pangyo.fp[, 1] <- rep(mydate, 2)
pangyo.fp[1:72, 2] <- c(311472.9, 278484.7, 340282.3, 361210.3, 329458.4, 317504.3, 368934.3, 331291.1,
            278452.8, 243931.5, 245505.7, 314207.2, 313586.0, 298272.1, 321000.3, 317891.7,
            299128.8, 293789.5, 232488.2, 229770.0, 224060.8, 231459.6, 241082.2, 225534.6,
            424380.7, 498581.6, 519259.4, 551302.2, 485450.5, 549583.7, 519784.7, 544937.0,
            556249.2, 460351.7, 508433.9, 577543.0, 205583.2, 188859.9, 226590.0, 239367.1,
            226619.5, 208913.4, 246180.2, 225487.9, 192697.0, 187768.7, 187083.2, 215206.1,
            215516.2, 210042.7, 223010.7, 228530.0, 223785.0, 219803.2, 183481.4, 180292.2,
            176558.9, 182380.8, 189584.8, 180515.1, 253410.8, 298144.5, 303726.3, 337331.5,
            308272.5, 333201.5, 308487.2, 328174.6, 325470.6, 286224.3, 296053.7, 355833.3)
pangyo.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(pangyo.fp) <- c("date_ym", "population", "level")

#광교유동인구
gwanggyo.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
gwanggyo.fp[, 1] <- rep(mydate, 2)
gwanggyo.fp[1:72, 2] <- c(296872.2, 271764.7, 328641.4, 340958.1, 330643.2, 273072.4, 328192.1, 314518.3,
                          336903.5, 320695.4, 318261.6, 305182.8, 302005.1, 295868.5, 321587.1, 311079.7,
                          312453.6, 299486.8, 271539.6, 272544.8, 258687.3, 256807.1, 277614.9, 300377.6,
                          413453.3, 481666.7, 507394.6, 537705.4, 488052.4, 530763.7, 505765.5, 498382.5,
                          533807.6, 452921.8, 472928.5, 544799.5, 288524.9, 266972.7, 324108.6, 331882.2,
                          324134.6, 263710.5, 328529.5, 309611.1, 338811.6, 328212.1, 329362.4, 316852.9,
                          315440.4, 313300.1, 343459.5, 334380.5, 344707.6, 329782.4, 298389.4, 297619.8,
                          278025.5, 275853.1, 301698.1, 331734.1, 298061.2, 343638.4, 368118.2, 403646.2, 
                          365170.3, 398625.3, 371499.6, 367423.0, 392414.4, 343814.7, 344587.9, 418749.4)
gwanggyo.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(gwanggyo.fp) <- c("date_ym", "population", "level")
gwanggyo.fp

#동탄유동인구
dongtan.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
dongtan.fp[, 1] <- rep(mydate, 2)
dongtan.fp[1:72, 2] <- c(398534.3, 351965.2, 422302.8, 427495.3, 405266.3, 332043.1, 434912.3, 411189.3,
                         411636.3, 425328.1, 406148.9, 391840.3, 392678.0, 379269.0, 408470.5, 325608.1,
                         408065.3, 329887.9, 446031.0, 464311.7, 441173.4, 444740.5, 465075.1, 450895.5,
                         505417.8, 554348.4, 605671.3, 647349.8, 587446.6, 653362.0, 618800.1, 639843.9,
                         662407.6, 565192.3, 624670.1, 706440.2, 301664.5, 277048.0, 344387.5, 356231.0,
                         343637.8, 274165.0, 360497.8, 341006.2, 349218.5, 354708.5, 344927.5, 343438.3,
                         350297.6, 348196.1, 376839.4, 273033.5, 393691.2, 328747.1, 398486.7, 405903.6,
                         397231.7, 398576.1, 413365.3, 404375.6, 335682.2, 355305.0, 389362.8, 433113.4,
                         390420.9, 430535.2, 414374.1, 431594.3, 442056.5, 390047.5, 419602.2, 493888.9)
dongtan.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(dongtan.fp) <- c("date_ym", "population", "level")
dongtan.fp


#한강유동인구
hangang.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
hangang.fp[, 1] <- rep(mydate, 2)
hangang.fp[1:72, 2] <- c(136725.8, 124591.5, 146046.3, 154853.1, 152967.0, 149443.9, 168498.2, 156053.3,
                         159323.6, 158865.7, 156553.3, 157393.0, 158294.2, 145292.6, 156053.9, 158524.5,
                         151973.9, 151919.5, 151662.0, 146361.3, 138462.9, 141009.9, 154718.6, 150408.0,
                         227410.2, 267360.1, 278441.4, 302027.6, 279907.3, 314169.7, 298642.8, 305864.6,
                         315157.1, 280209.0, 297294.4, 349090.5, 138652.2, 128641.1, 153103.1, 160528.1, 
                         157911.1, 157531.9, 166792.3, 155308.5, 160829.9, 160247.7, 162794.2, 160854.3,
                         160646.1, 147199.2, 156790.7, 157728.9, 153631.2, 153949.6, 154182.9, 147535.8,
                         145289.5, 146028.2, 160215.7, 156249.4, 159383.9, 189679.0, 200532.1, 229495.2,
                         212414.6, 237980.4, 224520.4, 229997.1, 237502.9, 213712.6, 227612.9, 278161.4)
hangang.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(hangang.fp) <- c("date_ym", "population", "level")
hangang.fp

#운정유동인구
unjung.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
unjung.fp[, 1] <- rep(mydate, 2)
unjung.fp[1:72, 2] <- c(214135.6, 197469.2, 228454.0, 237791.0, 236503.5, 222276.0, 244234.4, 234470.9,
                        233145.6, 229208.0, 224881.2, 227198.4, 230498.7, 231835.2, 239509.5, 242439.3,
                        240333.4, 235114.6, 233139.3, 229970.1, 220125.8, 217529.8, 229891.6, 225135.2,
                        269922.5, 272691.2, 274218.8, 302350.2, 259740.1, 302156.2, 279810.3, 279192.0,
                        294480.0, 272612.0, 289978.2, 362414.9, 238887.9, 223266.0, 266046.2, 276526.4,
                        271519.5, 259630.8, 280528.7, 265387.1, 271592.0, 267686.9, 264823.6, 265235.8,
                        266707.6, 270462.5, 283547.7, 285675.4, 284384.4, 279566.8, 274396.0, 266805.9,
                        263875.0, 259269.8, 274308.3, 266284.0, 231419.8, 228380.4, 222153.7, 253236.6,
                        211183.1, 249036.9, 222024.2, 218578.1, 234430.0, 220042.8, 228518.0, 313622.4)
unjung.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(unjung.fp) <- c("date_ym", "population", "level")
unjung.fp

#양주유동인구
yangju.fp <- data.frame(matrix(data = NA, nrow = 72, ncol = 3))
yangju.fp[, 1] <- rep(mydate, 2)
yangju.fp[1:72, 2] <- c(35209.66, 32006.33, 37295.13, 38616.30, 38847.22, 36233.66, 38845.67, 36469.38,
                         37063.69, 35983.17, 37018.28, 36647.83, 36448.66, 36399.59, 38070.61, 35077.13,
                         34057.09, 35385.78, 32367.58, 33049.78, 32548.12, 32131.18, 31265.90, 27753.96,
                         39993.50, 52335.85, 57921.49, 61350.57, 59635.39, 61104.39, 57141.80, 57383.51,
                         61913.01, 56043.00, 56412.47, 56432.31, 26951.86, 25269.61, 29266.60, 29612.34,
                         30153.16, 27449.04, 29803.56, 28395.60, 29540.02, 28538.07, 30045.89, 30046.20,
                         30268.61, 30283.02, 31220.01, 29073.81, 28477.31, 28911.21, 26719.66, 27127.81,
                         27329.33, 26851.76, 26078.46, 23516.27, 26096.74, 34947.40, 38928.73, 41804.25,
                         40867.06, 41327.87, 39926.18, 38821.49, 41672.99, 39556.02, 38960.39, 40273.65)
yangju.fp[, 3] <- factor(rep(c("man_pop", "woman_pop"), each = 36))
colnames(yangju.fp) <- c("date_ym", "population", "level")
yangju.fp


#2017년 12월 유동인구 지도상 표현
library(dplyr)
pop.p <- pangyo.fp %>% filter(date_ym == "1712")
pop.g <- gwanggyo.fp %>% filter(date_ym == "1712")
pop.d <- dongtan.fp %>% filter(date_ym == "1712")
pop.h <- hangang.fp %>% filter(date_ym == "1712")
pop.u <- unjung.fp %>% filter(date_ym == "1712")
pop.y <- yangju.fp %>% filter(date_ym == "1712")

pop12 <- c(sum(pop.p$population), sum(pop.p$population), sum(pop.d$population), sum(pop.h$population), sum(pop.u$population), sum(pop.y$population))
geo_12 <- cbind(geo, pop12)
head(geo_12)

map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
ggmap(map) + geom_point(data=geo_12, aes(x=lon,y=lat,size=pop12, colour = pop12), alpha=0.5) +scale_size_area(max_size = 15)


#신도시별 남성,여성 유동인구 지도상 표현

#남녀구분 함수 만들기
w <- function(x) {
  woman <- x %>% filter(level == "woman_pop")
}
m <- function(x) {
  man <- x %>% filter(level == "man_pop")
}

#판교 남녀 구분
pan.w <- w(pangyo.fp)
pan.m <- m(pangyo.fp)

#광교 남녀 구분
gwang.w <- w(gwanggyo.fp)
gwang.m <- m(gwanggyo.fp)

#동탄 남녀 구분
dong.w <- w(dongtan.fp)
dong.m <- m(dongtan.fp)

#한강 남녀 구분
han.w <- w(hangang.fp)
han.m <- m(hangang.fp)

#운정 남녀 구분
un.w <- w(unjung.fp)
un.m <- m(unjung.fp)

#양주 남녀 구분
yang.w <- w(yangju.fp)
yang.m <- m(yangju.fp)

#지도에 표현
pop.w <- c(sum(pan.w$population), sum(gwang.w$population), sum(dong.w$population), sum(han.w$population),
           sum(un.w$population), sum(yang.w$population))
pop.m <- c(sum(pan.m$population), sum(gwang.m$population), sum(dong.m$population), sum(han.m$population),
           sum(un.m$population), sum(yang.m$population))
geo.w <- cbind(geo, pop.w)
geo.m <- cbind(geo, pop.m)

map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
ggmap(map) + geom_point(data=geo.w, aes(x=lon,y=lat,size=pop.w, colour = pop.w),color = "red", alpha=0.5) +scale_size_area(max_size = 15)
ggmap(map) + geom_point(data=geo.m, aes(x=lon,y=lat,size=pop.m, colour = pop.m),color = "blue", alpha=0.5) +scale_size_area(max_size = 15)


#연도별 유동인구 그래프
str(yangju.fp)
class(yangju.fp[1,1])

#15,16,17년도 함수 만들기
sepa <- function(x = pangyo.fp) {
  pangyo.fp15 <<- subset(pangyo.fp, substr(pangyo.fp$date_ym, 1, 2) == "15")
  pangyo.fp16 <<- subset(pangyo.fp, substr(pangyo.fp$date_ym, 1, 2) == "16")
  pangyo.fp17 <<- subset(pangyo.fp, substr(pangyo.fp$date_ym, 1, 2) == "17")
}

sepa()

sepa2 <- function(x = gwanggyo.fp) {
  gwanggyo.fp15 <<- subset(gwanggyo.fp, substr(gwanggyo.fp$date_ym, 1, 2) == "15")
  gwanggyo.fp16 <<- subset(gwanggyo.fp, substr(gwanggyo.fp$date_ym, 1, 2) == "16")
  gwanggyo.fp17 <<- subset(gwanggyo.fp, substr(gwanggyo.fp$date_ym, 1, 2) == "17")
}

sepa2()

sepa3 <- function(x = dongtan.fp) {
  dongtan.fp15 <<- subset(dongtan.fp, substr(dongtan.fp$date_ym, 1, 2) == "15")
  dongtan.fp16 <<- subset(dongtan.fp, substr(dongtan.fp$date_ym, 1, 2) == "16")
  dongtan.fp17 <<- subset(dongtan.fp, substr(dongtan.fp$date_ym, 1, 2) == "17")
}

sepa3()

sepa4 <- function(x = hangang.fp) {
  hangang.fp15 <<- subset(hangang.fp, substr(hangang.fp$date_ym, 1, 2) == "15")
  hangang.fp16 <<- subset(hangang.fp, substr(hangang.fp$date_ym, 1, 2) == "16")
  hangang.fp17 <<- subset(hangang.fp, substr(hangang.fp$date_ym, 1, 2) == "17")
}

sepa4()

sepa5 <- function(x = unjung.fp) {
  unjung.fp15 <<- subset(unjung.fp, substr(unjung.fp$date_ym, 1, 2) == "15")
  unjung.fp16 <<- subset(unjung.fp, substr(unjung.fp$date_ym, 1, 2) == "16")
  unjung.fp17 <<- subset(unjung.fp, substr(unjung.fp$date_ym, 1, 2) == "17")
}

sepa5()

sepa6 <- function(x = yangju.fp) {
  yangju.fp15 <<- subset(yangju.fp, substr(yangju.fp$date_ym, 1, 2) == "15")
  yangju.fp16 <<- subset(yangju.fp, substr(yangju.fp$date_ym, 1, 2) == "16")
  yangju.fp17 <<- subset(yangju.fp, substr(yangju.fp$date_ym, 1, 2) == "17")
}

sepa6()

#연도별 유동인구 지도 그리기
#15년도유동인구
pop15 <- c(sum(pangyo.fp15$population), sum(gwanggyo.fp15$population), sum(dongtan.fp15$population),
           sum(hangang.fp15$population), sum(unjung.fp15$population), sum(yangju.fp15$population))
geo15 <- cbind(geo, pop15)
map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
ggmap(map) + geom_point(data=geo15, aes(x=lon,y=lat,size=pop15, colour = pop15), alpha=0.5) +scale_size_area(max_size = 15)

#16년도유동인구
pop16 <- c(sum(pangyo.fp16$population), sum(gwanggyo.fp16$population), sum(dongtan.fp16$population),
           sum(hangang.fp16$population), sum(unjung.fp16$population), sum(yangju.fp16$population))
geo16 <- cbind(geo, pop16)
map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
ggmap(map) + geom_point(data=geo16, aes(x=lon,y=lat,size=pop16, colour = pop16), alpha=0.5) +scale_size_area(max_size = 15)

#17년도유동인구
pop17 <- c(sum(pangyo.fp17$population), sum(gwanggyo.fp17$population), sum(dongtan.fp17$population),
           sum(hangang.fp17$population), sum(unjung.fp17$population), sum(yangju.fp17$population))
geo17 <- cbind(geo, pop17)
map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
ggmap(map) + geom_point(data=geo17, aes(x=lon,y=lat,size=pop17, colour = pop17), alpha=0.5) +scale_size_area(max_size = 15)


#1기신도시 좌표
  name <- c("bundang", "ilsan", "pyeongchon", "sanbon", "jungdong")
  lon <- c(127.108952, 126.770076, 126.963901, 126.932948, 126.764538 )
  lat <- c(37.350590, 37.683228, 37.394619, 37.358198, 37.486877)
  geo <- data.frame(name = name, lon = lon, lat = lat)

  #mydate
  makedate <- function(x = 15, y = 16, z = 17) {
    mydate <<- NULL
    for(yy in c(x, y, z)) {
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
  
  makedate()
  mydate

  #분당유동인구
  bundang.fp <- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  bundang.fp[, 1] <- mydate[1:24]
  bundang.fp[1:24, 2] <- c(1849620, 1668606, 1897919, 1935281, 1870637, 1664249, 2017077, 1819374, 1638859,
                           1621497, 1662930, 1694248, 1681118, 1715986, 1787234, 1765541, 1739404, 1699031,
                           2022159, 2057591, 1982747, 1935478, 2070891, 2126420)
  colnames(bundang.fp) <- c("date_ym", "population")

  #일산유동인구
  ilsan.fp <- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  ilsan.fp[, 1] <- mydate[1:24]
  ilsan.fp[1:24, 2] <- c(1583212, 1444724, 1638595, 1723730, 1670378, 1524473, 1682525, 1639061, 1643205,
                         1608758, 1585036, 1592949, 1598974, 1588560, 1613552, 1653172, 1596947, 1503266,
                         1765945, 1797549, 1698523, 1668307, 1705205, 1692792)
  colnames(ilsan.fp) <- c("date_ym", "population")

  #평촌유동인구
  pyeongchon.fp <- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  pyeongchon.fp[, 1] <- mydate[1:24]
  pyeongchon.fp[, 2] <- c(875845.8, 796449.4, 911280.4, 932720.7, 911702.4, 833988.6, 905954.7, 834712.3, 838981.5,
                          850911.6, 851686.1, 852769.6, 849867.1, 861431.2, 892446.4, 885988.6, 882383.1, 706110.0,
                          1001351.8, 1009373.2, 980933.8, 950136.9, 999250.7, 992920.7)
  colnames(pyeongchon.fp) <- c("date_ym", "population")
  
  pyeongchon.fp
  
  
  #산본유동인구
  sanbon.fp <- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  sanbon.fp[, 1] <- mydate[1:24]
  sanbon.fp[, 2] <- c(573191.5, 529830.9, 606419.4, 625621.7, 622550.6, 553724.5, 631998.0, 596604.3, 609848.3,
                      598638.6, 600305.6, 594060.3, 603191.8, 604624.3, 611351.0, 622957.8, 618694.0, 572475.9,
                      652019.8, 653211.9, 646567.8, 637151.6, 647542.5, 634922.9)
  colnames(sanbon.fp) <- c("date_ym", "population")
  
  sanbon.fp
  
  
  #중동유동인구
  jungdong.fp <- data.frame(matrix(data = NA, nrow = 24, ncol = 2))
  jungdong.fp[, 1] <- mydate[1:24]
  jungdong.fp[, 2] <- c(1261358, 1150125, 1305397, 1329870, 1290275, 1157684, 1342754, 1261717, 1285952, 1251382,
                        1247248, 1254423, 1248628, 1233183, 1240874, 1299832, 1308767, 1280952, 1295908, 1276652,
                        1227892, 1191655, 1234722, 1214513)
  colnames(jungdong.fp) <- c("date_ym", "population")
  
  jungdong.fp
  
  #15,16년도 함수 만들기
  sepa <- function(x = bundang.fp) {
    bundang.fp15 <<- subset(bundang.fp, substr(bundang.fp$date_ym, 1, 2) == "15")
    bundang.fp16 <<- subset(bundang.fp, substr(bundang.fp$date_ym, 1, 2) == "16")
  }
  
  sepa()
  
  sepa2 <- function(x = ilsan.fp) {
    ilsan.fp15 <<- subset(ilsan.fp, substr(ilsan.fp$date_ym, 1, 2) == "15")
    ilsan.fp16 <<- subset(ilsan.fp, substr(ilsan.fp$date_ym, 1, 2) == "16")
  }
  
  sepa2()
  
  sepa3 <- function(x = pyeongchon.fp) {
    pyeongchon.fp15 <<- subset(pyeongchon.fp, substr(pyeongchon.fp$date_ym, 1, 2) == "15")
    pyeongchon.fp16 <<- subset(pyeongchon.fp, substr(pyeongchon.fp$date_ym, 1, 2) == "16")
  }
  
  sepa3()
  
  sepa4 <- function(x = sanbon.fp) {
    sanbon.fp15 <<- subset(sanbon.fp, substr(sanbon.fp$date_ym, 1, 2) == "15")
    sanbon.fp16 <<- subset(sanbon.fp, substr(sanbon.fp$date_ym, 1, 2) == "16")
  }
  
  sepa4()
  
  sepa5 <- function(x = jungdong.fp) {
    jungdong.fp15 <<- subset(jungdong.fp, substr(jungdong.fp$date_ym, 1, 2) == "15")
    jungdong.fp16 <<- subset(jungdong.fp, substr(jungdong.fp$date_ym, 1, 2) == "16")
  }
  
  sepa5()
  
  #연도별 유동인구 지도 그리기
  
  #16년도유동인구
  pop16 <- c(sum(bundang.fp16$population), sum(ilsan.fp16$population), sum(pyeongchon.fp16$population),
             sum(sanbon.fp16$population), sum(jungdong.fp16$population))
  geo16 <- cbind(geo, pop16)
  map <- get_googlemap(center=cen, maptype="roadmap", zoom= 10)
  ggmap(map) + geom_point(data=geo16, aes(x=lon,y=lat,size=pop16, colour = pop16), alpha=0.5) +scale_size_area(max_size = 15)
  