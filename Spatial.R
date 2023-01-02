library(ggmap)
library(geojsonio)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(gganimate)
library(gifski)

hood <- geojson_read('hoods.geojson', what = 'sp')
data <- read.csv("bikeshare.csv")

bbox = c(min(data$Start.Station.Longitude), 
         min(data$Start.Station.Latitude),
         max(data$Start.Station.Longitude),
         max(data$Start.Station.Latitude))

start_stations <- unique(data[c("Start.Station.Name","Start.Station.Longitude", "Start.Station.Latitude")])
end_stations <- unique(data[c("End.Station.Name","End.Station.Longitude", "End.Station.Latitude")])



myMap <- get_stamenmap(bbox = hood@bbox, zoom=13)
ggmap(myMap)+geom_polygon(data = hood, 
               aes( x = long, y = lat, group = group), 
               fill="cornsilk4", color="brown", alpha = 0.3)+
  geom_point(data=end_stations, mapping = aes(x=End.Station.Longitude,
                                              y=End.Station.Latitude))+
  geom_point(data=start_stations, mapping = aes(x=Start.Station.Longitude,
                                              y=Start.Station.Latitude), color = "red", alpha = 0.5)


stations <- data.frame(startlong = start_stations$Start.Station.Longitude,
                       startlat = start_stations$Start.Station.Longitude,
                       endlong = end_stations$End.Station.Longitude,
                       endlat = end_stations$End.Station.Latitude)



bikemap <- get_stamenmap(bbox = hood@bbox, zoom=13)

data$Start.Time <-  as.POSIXct(data$Start.Time)
data$Stop.Time <-  as.POSIXct(data$Stop.Time)

data <- data %>% mutate(starthour=hour(Start.Time),
                stophour=hour(Stop.Time), 
                weekday=(weekdays(Start.Time)),
                year = year(Start.Time),
                month = month(Start.Time),
                day = day(Start.Time))

data$weekpart <- if_else(condition = data$weekday %in% c("Saturday", "Sunday"), 
                         "weekend", "weekday")
vizdata <- data[data$year == 2016,]

monthchange <- vizdata %>% group_by(month) %>% count()
monthchange$name <- month.name

monthchange <- vizdata %>% group_by(month) %>% count()

prop.table(table(vizdata$weekpart,vizdata$User.Type), margin = 1)
ggplot()+geom_col(data=monthchange, mapping=aes(x=reorder(month.name,month), y=n))

sept <- vizdata[vizdata$month == 9,]
sept <- sept[sept$End.Station.ID %in% unique(sept$Start.Station.ID),]

startsept <- sept %>% group_by(Start.Station.Name, weekpart, starthour) %>% 
  count() %>% 
  mutate(startavgn=n/5)%>%
  rename(station=Start.Station.Name,
         startn = n,
         hour = starthour)

endsept <- sept %>% group_by(End.Station.Name, weekpart, stophour) %>% 
  count() %>% 
  mutate(endavgn=n/5) %>%
  rename(station=End.Station.Name,
         endn = n,
         hour = stophour)

countsept = expand.grid(c("weekday", "weekend"), seq(0,23), 
            unique(sept$Start.Station.Name))

colnames(countsept) <- c("weekpart", "hour", "station")

countsept <- left_join(countsept,startsept)

countsept <- left_join(countsept, endsept)
countsept[is.na(countsept)] <- 0
countsept$net <- countsept$endn-countsept$startn
countsept$netavg <- countsept$endavgn-countsept$startavgn
countsept

stations <- sept[c("Start.Station.Name",
                   "Start.Station.Longitude", 
                   "Start.Station.Latitude")]
colnames(stations) <- c("station", "long", "lat")
stations <- stations %>% unique.data.frame()

set.seed(1916)
kmstation <- kmeans(stations[2:3], centers = 10)
kmstation$cluster
stations$cluster <- kmstation$cluster
write.csv(x = stations, "stations.csv")

ggmap(myMap)+
  geom_point(data=stations, mapping = aes(x=long,
                                    y=lat, 
                                    color = as.factor(cluster)),
             size=5)+labs(color = "Cluster", 
                          title = "Clusters of Stations")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  

countsept <- left_join(countsept, stations)

countsept

df <- countsept %>% filter(weekpart=="weekday")
p <- ggmap(myMap)+
  geom_point(data=df, mapping = aes(x=long,
                                    y=lat, 
                                    fill = sign(netavg), 
                                    size=abs(netavg)),
             shape = 21)+
  scale_fill_stepsn(colors = c("red", "yellow" , "green"))+
  scale_size_continuous(range = c(3,8))+
  labs(x = "Longitude", y = "Latitude", title = "Weekday, Hour = {frame_time}",
       fill="Flow (+/-)", size = "Net change") +
  theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())+
  transition_time(hour) +
  ease_aes("linear")

obj = animate(p, duration = 20, fps = 24, width = 500, height = 550, renderer = gifski_renderer())
anim_save("jersey.gif",animation = obj)
obj


df <- countsept %>% filter(weekpart=="weekend")
p <- ggmap(myMap)+
  geom_point(data=df, mapping = aes(x=long,
                                    y=lat, 
                                    fill = sign(netavg), 
                                    size=abs(netavg)),
             shape = 21)+
  scale_fill_stepsn(colors = c("red", "yellow" , "green"))+
  scale_size_continuous(range = c(3,8))+
  labs(x = "Longitude", y = "Latitude", title = "Weekend, Hour = {frame_time}",
       fill="Flow (+/-)", size = "Net change") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  transition_time(hour) +
  ease_aes("linear")

obj = animate(p, duration = 20, fps = 24, width = 500, height = 550, renderer = gifski_renderer())
anim_save("jerseyweekend.gif",animation = obj)
obj