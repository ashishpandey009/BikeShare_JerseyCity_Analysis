library(tidyverse)
library(ggplot2)
library(cowplot)
library(dplyr)
library(plotly)
library(ggmap)
library(ggpubr)
library(lubridate)
library(caret)
library(sandwich)
library(party)
library(hrbrthemes)
library(viridis)
library(forcats)
library(GGally)
rawdata <- read.csv(file = "bikeshare.csv")



rawdata$Start.Time <-  as.POSIXct(rawdata$Start.Time)
rawdata$Stop.Time <-  as.POSIXct(rawdata$Stop.Time)

rawdata <- rawdata %>% mutate(starthour=hour(Start.Time),
                              stophour=hour(Stop.Time), 
                              weekday=(weekdays(Start.Time)))

rawdata$weekpart <- if_else(condition = rawdata$weekday %in% c("Saturday", "Sunday"), 
                            "weekend", "weekday")



###Visualizations
## User Type on Weekdays
ggplot(rawdata, aes(y=factor(weekday,levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")),
                    color=User.Type, fill = User.Type)) +
  geom_bar(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_wrap(~User.Type, scales ="free_x")+
  labs(y = "Weekdays", title = "User Type on Weekdays")

rawdata$age = 2017-rawdata$Birth.Year
#rawdata
## Relation between Gender and Time Duration

rawdata[rawdata$Gender == 0,]$Gender <- "Others"
rawdata[rawdata$Gender == 1,]$Gender <- "M"
rawdata[rawdata$Gender == 2,]$Gender <- "F"
rawdata$Gender <- as.factor(rawdata$Gender)
rawdata$User.Type <- as.factor(rawdata$User.Type)


## Pairs for Gender
##ggpairs(rawdata, columns = 2:4, ggplot2::aes(colour=Gender))

##duration density and age for male and female

#boxplot(rawdata$age)
rawdata = rawdata[rawdata$Gender!="Others",]
rawdata = rawdata[rawdata$User.Type!="Customer",]
rawdata = rawdata[rawdata$age<100,]
min(boxplot(rawdata$Trip.Duration)$out)
min(boxplot(rawdata$age)$out)
rawdata = rawdata[rawdata$Trip.Duration<1143,]



ggplot() +
  geom_density(rawdata,mapping =  aes(x=Trip.Duration,fill=Gender),alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() + 
  labs(title = "Trip Duration distribution based on Gender", xlab = "Trip Duration", ylab = "Density")


ggplot() +
  geom_density(rawdata,mapping =  aes(x=age,fill=Gender),alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  labs(title = "Age distribution based on Gender", xlab = "Age", ylab = "Density")
