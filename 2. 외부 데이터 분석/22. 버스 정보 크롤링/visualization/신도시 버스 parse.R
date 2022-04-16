#판교
library(XML)
library(ggmap)
pan <- as.data.frame(pangyo)

url <- NULL
for(i in 1:nrow(pan)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", pan[i,2])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)

x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red', size = 0.7, na.rm = T) + ggtitle("판교버스노선")

#광교
gwang <- as.data.frame(gwanggyo$route)

url <- NULL
for(i in 1:nrow(gwang)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", gwang[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red', size=0.7,na.rm = T)+ggtitle("광교버스노선")

#dongtan
dong <- as.data.frame(dongtan$route)

url <- NULL
for(i in 1:nrow(dong)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", dong[i,1])
}
head(url)

df <- NULL
for(i in  1:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)

x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 0.7, na.rm = T)+ggtitle("동탄버스노선")

#hangan
han <- as.data.frame(hangang$route)

url <- NULL
for(i in 1:nrow(han)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", han[i,1])
}
head(url)

df <- NULL
for(i in  1:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)

x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red', size = 0.5, na.rm = T)+ggtitle("한강버스노선")

#unjeong
un <- as.data.frame(unjeong$route)

url <- NULL
for(i in 1:nrow(un)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", un[i,1])
}
head(url)

df <- NULL
for(i in  1:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)

x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 0.7, na.rm = T) + ggtitle("운정버스노선")

#yangju
yang <- as.data.frame(yangju$route)

url <- NULL
for(i in 1:nrow(yang)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", yang[i,1])
}
head(url)

df <- NULL
for(i in  1:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)

x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'red',size = 0.7, na.rm = T) + ggtitle("양주버스노선")

#bundang
bun <- as.data.frame(bundang$route)

url <- NULL
for(i in 1:nrow(bun)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", bun[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue', size=0.7,na.rm = T)+ggtitle("분당버스노선")

#ilsan
il <- as.data.frame(ilsan$route)

url <- NULL
for(i in 1:nrow(il)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", il[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue', size=0.7,na.rm = T)+ggtitle("일산버스노선")


#pyeongchon
pyeong <- as.data.frame(pyeongchon$route)

url <- NULL
for(i in 1:nrow(pyeong)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", pyeong[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue', size=0.7,na.rm = T) + ggtitle("평촌버스노선")


#sanbon
san <- as.data.frame(sanbon$route)

url <- NULL
for(i in 1:nrow(san)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", san[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue', size=0.7,na.rm = T)+ggtitle("산본버스노선")


#jungdong
jung <- as.data.frame(jungdong$route)

url <- NULL
for(i in 1:nrow(jung)) {
  url[i] <- paste0("http://openapi.gbis.go.kr/ws/rest/busrouteservice/line?serviceKey=wl5JsU%2BjTa8zbsRleeTkQ7TN9I82sb85KzZUlWJPuQshc8vhyxApaEXcDH%2BSO6A%2Fbmyvi5HlZVWjNTscQ99NZQ%3D%3D&routeId=", jung[i,1])
}
head(url)

a<- xmlParse(url[1])
xmlRoot(a)
df <- xmlToDataFrame(getNodeSet(a, "//busRouteLineList"))
for(i in  2:length(url)) {
  k <- xmlParse(url[i])
  xmlRoot(k)
  df <- rbind(df,xmlToDataFrame(getNodeSet(k, "//busRouteLineList")))
}

head(df)


x <- as.numeric(as.character(df$x))
y <- as.numeric(as.character(df$y))
gc <- data.frame(lon = x, lat = y)
cen <- c(mean(gc$lon),mean(gc$lat))
cen

register_google(key = 'AIzaSyCrhPnQ3QC9g66vrcGwk2_BExsDpAe7jHo')
map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 9)
ggmap(map) + geom_point(data=gc, aes(x=lon,y=lat),col = 'blue', size=0.7,na.rm = T)+ggtitle("중동버스노선")
