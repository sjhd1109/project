library('tidyverse')
library('dplyr')
library('tsne')
library('readr')
library('ggplot2')
library('cluster')
library(stringr)
install.packages("xlsx")   
library(xlsx)
install.packages("WriteXLS") 
library(WriteXLS)
# R 에서 작업한 데이터 외부에 엑셀 파일로 저장하기

WriteXLS("mise_coount", "item.xlsx")



library('factoextra')
install.packages("fpc")
install.packages("rename")
install.packages('factoextra')
library('fpc')
install.packages("clValid")
library('clValid')
library(raster)
install.packages("tSNE")
setwd("C:\\Rtest")
mise <- read.csv("mise.csv")
scaled_mise <- scale(mise, center=TRUE, scale=TRUE)
mise_scaled <- read.csv("sclaed-mise.csv")
mise_scaled_name <- read.csv("sclaed-mise(name).csv")

#변수들 정리+지도
seoul <- read.csv("sample.csv", header = TRUE) #시각화할 데이터셋
seoul_id <- left_join(mise_scaled_name, seoul, by="cityname")

map_shape <- shapefile("TL_SCCO_SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")

map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]
M <- merge(seoul_map, seoul_id, by = "id")


#데이터 load
seoul_green <- read.csv("sclaed-mise(green).csv") 
seoul_mise_t <- read.csv("misetest.csv") 
seoul_id_green <- left_join(seoul_green, seoul, by="cityname")
seoul_id_mise <- left_join(seoul_mise_t, seoul, by="cityname")

m_green <- merge(seoul_map, seoul_id_green, by = "id")
m_seoul_mise_t1 <- merge(seoul_map, seoul_id_mise, by = "id")
mise_coount <- read.csv("sclaed-mise(count).csv") 


#1)면적대비 도시숲비율 + 미세 빈도수 
p2 <- ggplot() + 
  geom_polygon(data = m_green, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = city_green),
               color = "white") +
  scale_fill_gradient(low = "#D1C9D0",
                      high = "#339143",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "도시숲비율") 
  #theme_void()  
  #theme(legend.position = c(.15, .85)) +
  #geom_text(aes(x=long, y=lat+1700, label=cityname), colour="black", vjust=3, size=6, 
          #  fontface="bold", data= seoul_mise_t)
p2

seoul_mise_t1 <- read.csv("misetest(10).csv")
p2 + geom_text(aes(x=(long-300), y=(lat+1700), label=cityname), colour="black", vjust=3, size=6, 
               fontface="bold", data=seoul_mise_t) + labs(x="경도", y="위도") + 
  geom_point(data=seoul_mise_t1, aes(x=(long-200), y=lat,size=mise), alpha=0.5) +
  scale_size_area(name=c("미세먼지빈도"), max_size=25) +
  geom_text(aes(x=long, y=lat, label=mise), colour="red", vjust=3, size=6, 
            fontface="bold", data=seoul_mise_t1) + labs(x="경도", y="위도")


#1-2) 1인당도시숲면적+미세먼지
seoul_people <- read.csv("sclaed-mise(people).csv") 
seoul_id_people <- left_join(seoul_people, seoul, by="cityname")
m_people <- merge(seoul_map, seoul_id_people, by = "id")
p1<- ggplot() + 
  geom_polygon(data = m_people, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = people_green),
               color = "white") +
  scale_fill_gradient(low = "#D1C9D0",
                      high = "#3EB051",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "1인당도시숲면적") 
  #theme_void() + 
  #theme(legend.position = c(.15, .85)) 
p1
 #stat_density_2d(data=mise_coount, aes(x=long, y=lat, alpha=..level..),geom='polygon', size=1, bins=30) +
p1 + geom_text(aes(x=(long-300), y=(lat+1700), label=cityname), colour="black", vjust=3, size=6, 
            fontface="bold", data=seoul_mise_t) + labs(x="경도", y="위도") + 
  geom_point(data=seoul_mise_t1, aes(x=(long-200), y=lat,size=mise), alpha=0.5) +
  scale_size_area(name=c("미세먼지빈도"), max_size=25) +
  geom_text(aes(x=long, y=lat, label=mise), colour="red", vjust=3, size=5, 
            fontface="bold", data=seoul_mise_t1) + labs(x="경도", y="위도")

#2-1) 도시수면적+열섬현상
p

p2+geom_text(aes(x=long, y=(lat+500), label=winter), colour="red", vjust=3, size=6, 
                                      fontface="bold", data=seoul_tem) + 
  tconts + color_conts + color_pick + geom_text(aes(x=long, y=(lat+1500), label=cityname), colour="black", vjust=3, size=6, 
                                                fontface="bold", data=seoul_mise_t) + labs(x="경도", y="위도")
  

p1+ geom_text(aes(x=long, y=(lat+500), label=winter), colour="red", vjust=3, size=6, 
                                       fontface="bold", data=seoul_tem) + tconts + color_conts + color_pick +
  geom_text(aes(x=long, y=(lat+1500), label=cityname), colour="black", vjust=3, size=6, 
            fontface="bold", data=seoul_mise_t) 

#3)열섬+미세먼지 

y <- ggplot() + 
  geom_polygon(data = m_seoul_mise_t1, 
               aes(x = long.x, 
                   y = lat.x, 
                   group = group, 
                   fill = mise),
               color = "white", alpha=0.9) +
  scale_fill_gradient(low = "#A8E3F6",
                      high = "#A6ABB0",
                      space = "Lab",
                      guide = "colourbar") +
 labs(fill = "대기오염정도") 
  #theme_void() 
  #theme(legend.position = c(.15, .85)) 


library(kriging)
install.packages("kriging")
library(ggmap)

seoul_wether <- read.csv("sclaed-mise(wether).csv") 
# Interpolation (온도 등고선)
surface <- with(seoul_wether, interp(long, lat, winter, linear = FALSE)) 
str(surface)
srfc <- expand.grid(lon = surface$x, lat = surface$y) 
srfc$temp <- as.vector(surface$z) 
tconts <- geom_contour(aes(x = lon, y = lat, z = temp), data = srfc, color = "black", na.rm = TRUE, size=1, alpha = 0.3, bins=15) 
mymap_interp <- y + tconts 
mymap_interp
tpoints <- geom_text(aes(x = long, y = lat, label = winter), fontface="bold", size=6, color = "red", data = seoul_wether) 
tcities <- geom_text(aes(x = (long-500), y = (lat + 800), label = cityname), size=6,fontface="bold", color = "black", data = seoul_wether) 
y1 <- mymap_interp + tpoints + tcities

color_conts <- geom_contour(data = srfc, aes(x=lon,y=lat,z = temp, color =..level..), size=1.5,alpha=0.7, na.rm = TRUE, bins = 15) 
y1+color_conts

color_pick <- scale_colour_distiller(palette="RdBu") 
y1 + color_conts + color_pick


#맵핑
seoul_id_wether <- left_join(seoul_wether, seoul, by="cityname")
m_wether <- merge(seoul_map, seoul_id_wether, by = "id")
ggplot() + 
  geom_polygon(data = m_wether, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = winter),
               color = "white") +
  scale_fill_gradient(low = "#B2EBF4",
                      high = "#FF0000",
                      space = "Lab",
                      guide = "colourbar") +
  labs(fill = "겨울철평균온도") +
  theme_void() + 
  theme(legend.position = c(.15, .85)) +
  

#클러스터링+그림
set.seed(123)
mise_tsne = tsne::tsne(as.matrix(mise_scaled[2:12]))

# 맵핑된 결과물에 원래의 레이블을 달아보자
df_mise_tsne = mise_tsne %>% 
  as.data.frame() %>% 
  tbl_df() %>% 
  mutate(cityname = mise_scaled$cityname)
df_mise_tsne

#이걸로 사용!!
data_mise <- kmeans(df_mise_tsne,5)
cluster <- as.factor(c(data_mise$cluster))
cityname <- c("강남구",
              "강동구",
              "강북구",
              "강서구",
              "관악구",
              "광진구",
              "구로구",
              "금천구",
              "노원구",
              "도봉구",
              "동대문구",
              "동작구",
              "마포구",
              "서대문구",
              "서초구",
              "성동구",
              "성북구",
              "송파구",
              "양천구",
              "영등포구",
              "용산구",
              "은평구",
              "종로구",
              "중구",
              "중랑구"
)
cluster_df <- data.frame(cityname, cluster)
fviz_cluster(data_mise,data=df_mise_tsne, frame.type="convex")+
    theme_minimal()

seoul <- read.csv("sample.csv", header = TRUE) #시각화할 데이터셋
seoul_id <- left_join(cluster_df, seoul, by="cityname")

install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

seoul <- read.csv("sample.csv", header = TRUE) #시각화할 데이터셋
seoul_id <- left_join(cluster_df, seoul, by="cityname")

library(raster)
map_shape <- shapefile("TL_SCCO_SIG.shp")
map <- fortify(map_shape, region = "SIG_CD")
str(map)

map$id <- as.numeric(map$id)
seoul_map <- map[map$id <= 11740,]
M <- merge(seoul_map, seoul_id, by = "id")

#클러스터링  <범주로 색칠하기>
ggplot() + 
  geom_polygon(data = M, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = cluster),color = "white") +
  labs(fill = "cluster") +
  theme_void() + 
  theme(legend.position = c(.15, .85)) +
  geom_text(aes(x=(long-500), y=(lat+1700), label=cityname), colour="black", vjust=3, size=6, 
            fontface="bold", data=seoul_mise_t) + labs(x="경도", y="위도")

#변수들 정리

#1) 구면적 대비 도시숲 비율
#2) 1인당 도시숲 면적
#3) 열섬현상 겨울철 평균온도(할지말지)
#4) 미세먼지 농도 빈도수 
#5) 미세먼지 이외 대기오염 농도 빈도수


map_shape@data %>% head

sb <- read.csv("starbucks.csv") %>% as_tibble
sb %>% group_by(SIG_CD) %>%
  summarise(count=n()) -> sb2
sp::merge(map_shape, sb2) -> korea3
korea3@data %>% head
pal2 <- colorNumeric("viridis", korea3$count, reverse=TRUE)
library('rmapshaper')
library('raster')
library('leaflet')


leaflet(korea3) %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addProviderTiles('Stamen.Toner') %>%
  addPolygons(color='#444',fillColor=~pal2(count))

pal <- colorFactor("viridis", sb$code)
leaflet(sb) %>%
  setView(lng=126.9784, lat=37.566, zoom=11) %>%
  addProviderTiles('Stamen.Toner') %>%
  addCircles(lng=~long, lat=~lat, color=~pal(code))
