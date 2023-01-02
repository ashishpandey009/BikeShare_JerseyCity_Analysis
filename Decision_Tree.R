
library(dplyr)
library(leaps)
library(tidyverse)
library(lubridate)
library(creditmodel)
library(caTools)
library(tree)
library(caret)
library(randomForest)
library(ranger)
library(DALEX)
library(tidymodels)
library(rlang)




path ="bikeshare.csv"
raw_data = read.csv(path)


stations <- read.csv("stations.csv")

set.seed(1916)
kmstation <- kmeans(stations[3:4], centers = 8)
kmstation$cluster
stations$cluster <- kmstation$cluster

raw_data <- left_join(raw_data,stations[,c("station", "cluster")], by=c("Start.Station.Name" = "station"))
raw_data <- rename(raw_data, startcluster = cluster)
raw_data <- left_join(raw_data,stations[,c("station", "cluster")], by=c("End.Station.Name" = "station"))
raw_data <- rename(raw_data, endcluster = cluster)
raw_data$startcluster <- as.factor(raw_data$startcluster)
raw_data$endcluster <- as.factor(raw_data$endcluster)


raw_data$Start.Time <-  as.POSIXct(raw_data$Start.Time)
raw_data$Stop.Time <-  as.POSIXct(raw_data$Stop.Time)

raw_data <- raw_data %>% mutate(starthour=hour(Start.Time),
                        stophour=hour(Stop.Time), 
                        weekday=(weekdays(Start.Time)))

raw_data$weekpart <- if_else(condition = raw_data$weekday %in% c("Saturday", "Sunday"), 
                         "weekend", "weekday")
raw_data$weekpart <- as.factor(raw_data$weekpart)




col_list = c("startcluster","endcluster",
             "User.Type", "Trip_Duration_in_min", 
             "starthour", "stophour", "weekday", "weekpart"   )
raw_data = raw_data[,col_list]
raw_data = na.omit(raw_data)

subset1 = raw_data[raw_data$User.Type != "Customer",]
subset2 = raw_data[raw_data$User.Type == "Customer",]
subset1= subset1[sample(nrow(subset1), size=nrow(subset2)), ]
data = rbind(subset1,subset2)
data$User.Type = as.factor(data$User.Type)
data$weekday = as.factor(data$weekday)

write.csv(data,"data.csv")
data = read_csv("data.csv")

set.seed(1)
train = sample.split(data$User.Type, SplitRatio = 0.7)


tree.user = tree(User.Type ~ ., data, subset=train)
summary(tree.user)


plot(tree.user)
text(tree.user,pretty=0)


pred =predict(tree.user,newdata=data[-train,])
pred_labels <- ifelse(pred[,2] > 0.5, "Subscriber","Customer")
actual = data[-train,"User.Type"]

c_matrix <- confusionMatrix(data=as.factor(pred_labels), reference = as.factor(actual))

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


#Display results 
draw_confusion_matrix(c_matrix)






####### RANDOM Forest ##########


library(party)
data$weekpart = as.factor(data$weekpart)
data$User.Type = as.factor(data$User.Type)
cf1 <- cforest(User.Type ~ Trip_Duration_in_min + weekpart ,data=data[train,],control=cforest_unbiased(mtry=2,ntree=50))
varImp1 = varimp(cf1)

ggplot(data= varImp1, aes(x= reorder(colnames(varImp1),Overall))) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") +
  coord_flip() + geom_point(color='skyblue') + xlab(" Importance Score")+
  ggtitle("Variable Importance")

pred = predict(cf1,data=data[-train,c("Trip_Duration_in_min","User.Type","weekpart")])




