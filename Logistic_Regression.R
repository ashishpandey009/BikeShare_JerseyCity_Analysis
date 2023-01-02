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
#setwd('/Users/ashishpandey009/Documents/STAT 515/STAT515_FINAL_PROJECT')
rawdata <- read.csv(file = "bikeshare.csv")

str(rawdata)
raw_data <- rawdata

#set.seed(10000)
#runif(5)
#runif(nrow(rawdata))
#summary(rawdata[,c(1,2,3,4)])

rawdata$Start.Time <-  as.POSIXct(rawdata$Start.Time)
rawdata$Stop.Time <-  as.POSIXct(rawdata$Stop.Time)

rawdata <- rawdata %>% mutate(starthour=hour(Start.Time),
                              stophour=hour(Stop.Time), 
                              weekday=(weekdays(Start.Time)))

rawdata$weekpart <- if_else(condition = rawdata$weekday %in% c("Saturday", "Sunday"), 
                            "weekend", "weekday")


raw_data <- rawdata
###Logistic Regression
## Prediction using single variable
rawdata[rawdata$Gender == 0,]$Gender <- "Others"
rawdata[rawdata$Gender == 1,]$Gender <- "M"
rawdata[rawdata$Gender == 2,]$Gender <- "F"
rawdata$age = 2017-rawdata$Birth.Year
#rawdata$Gender <- as.factor(rawdata$Gender)
rawdata$User.Type <- as.factor(rawdata$User.Type)
raw_data <- rawdata
table(raw_data$Gender)

subset1 = rawdata[rawdata$Gender == "M",]
subset2 = rawdata[rawdata$Gender == "F",]
subset1= subset1[sample(nrow(subset1), size=nrow(subset2)), ]
raw_data = rbind(subset1,subset2)
raw_data$Gender <- as.factor(raw_data$Gender)
str(raw_data)
#raw_data$User.Type <- table(c("Ashish", "Arman"))
#summary(raw_data)
#table(raw_data$User.Type)

#nrow(raw_data[is.na(raw_data$User.Type) | is.na(raw_data$Gender),])

#xtabs(~ Start.Station.Name + Gender, data=raw_data)
#xtabs(~ Start.Station.Name + User.Type, data=raw_data)

stations <- read.csv("stations.csv")


set.seed(1916)
kmstation <- kmeans(stations[3:4], centers = 10)
kmstation$cluster
stations$cluster <- kmstation$cluster

raw_data <- left_join(raw_data,stations[,c("station", "cluster")], by=c("Start.Station.Name" = "station"))
raw_data <- rename(raw_data, startcluster = cluster)
raw_data <- left_join(raw_data,stations[,c("station", "cluster")], by=c("End.Station.Name" = "station"))
raw_data <- rename(raw_data, endcluster = cluster)
raw_data$startcluster <- as.factor(raw_data$startcluster)
raw_data$endcluster <- as.factor(raw_data$endcluster)

raw_data$daypart <- cut(raw_data$starthour,
                        breaks=c(-1, 2, 6, 10, 14, 17, 19,21, 24),
                        labels=c('Night', 'Late Night', 'Morning',
                                 'Noon', 'Lunch', 'Going Home','Evening', 'Night'))
raw_data <- drop_na(raw_data)
set.seed(123)
training.samples <- raw_data$Gender %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- raw_data[training.samples, ]
test.data <- raw_data[-training.samples, ]
str(test.data)


#write.csv(x = stations, "stations.csv")



logistic <- glm(Gender ~ daypart +
                  weekday+age+startcluster+endcluster,
                data = train.data, family = "binomial")
summary(logistic)
logistic$coefficients
probabilities <- logistic %>% predict(test.data, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, "M", "F")

c_matrix <- confusionMatrix(data=as.factor(predicted.classes), reference = as.factor(test.data$Gender))
c_matrix
#odds <- exp(logistic$coefficients)
#odds/(1+odds)

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Customer', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Subscriber', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Customer', cex=1.2, srt=90)
  text(140, 335, 'Subscriber', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix(c_matrix)
