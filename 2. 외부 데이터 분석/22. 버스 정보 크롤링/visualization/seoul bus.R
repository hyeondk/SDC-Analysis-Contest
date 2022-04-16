#도시별 서울 버스 정류장 지도
library(ggmap)
#판교
x <- as.numeric(as.character(pangyo_seoul$X좌표))
y <- as.numeric(as.character(pangyo_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("판교")

#광교
x <- as.numeric(as.character(gwanggyo_seoul$X좌표))
y <- as.numeric(as.character(gwanggyo_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("광교") 

#동탄
x <- as.numeric(as.character(dongtan_seoul$X좌표))
y <- as.numeric(as.character(dongtan_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("동탄")

#한강
x <- as.numeric(as.character(hangang_seoul$X좌표))
y <- as.numeric(as.character(hangang_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("한강")

#운정
x <- as.numeric(as.character(unjeong_seoul$X좌표))
y <- as.numeric(as.character(unjeong_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("운정")

#양주
x <- as.numeric(as.character(yangju_seoul$X좌표))
y <- as.numeric(as.character(yangju_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 1, na.rm = T) + ggtitle("양주")


#분당
x <- as.numeric(as.character(bundang_seoul$X좌표))
y <- as.numeric(as.character(bundang_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue',size = 1, na.rm = T) + ggtitle("분당")



#일산
x <- as.numeric(as.character(ilsan_seoul$X좌표))
y <- as.numeric(as.character(ilsan_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue',size = 1, na.rm = T) + ggtitle("일산")


#평촌
x <- as.numeric(as.character(pyeongchon_seoul$X좌표))
y <- as.numeric(as.character(pyeongchon_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue',size = 1, na.rm = T) + ggtitle("평촌")



#산본
x <- as.numeric(as.character(sanbon_seoul$X좌표))
y <- as.numeric(as.character(sanbon_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue',size = 1, na.rm = T) + ggtitle("산본")


#중동
x <- as.numeric(as.character(jungdong_seoul$X좌표))
y <- as.numeric(as.character(jungdong_seoul$Y좌표))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue',size = 1, na.rm = T) + ggtitle("중동")


