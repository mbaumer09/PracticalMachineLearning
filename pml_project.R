# Practical Machine Learning Course Project
# Exploratory analysis
# setwd("D:/Dropbox/Dropbox/Coursera/Practical Machine Learning/Project")     #LAPTOP
library(caret)
library(dplyr)
setwd("C:/Users/USER/Dropbox/Coursera/Practical Machine Learning/Project")    #DESKTOP
train <- read.csv("pml-training.csv")
set.seed(12321)
inTrain <- createDataPartition(train$classe, p = .5, list=FALSE) # Lower size of training set to make the
                                                                 # train() call go faster
train.train <- train[inTrain,]
train.test <- train[-inTrain,]

#Clean out the columns with large numbers of NAs and missing data and useless stuff
train.clean <- train.train[,-which(is.na(train[1,]))]
train.clean <- train.clean[,-which(train.clean[1,] == "")]
train.clean <- train.clean[,-c(1:7)]

# total_accel_[blank] seemed useless except for total_accel_belt
qplot(total_accel_forearm, data = train, facets = classe~.)
qplot(total_accel_belt, data = train, facets = classe~.)

train.clean <- train.clean %>%
            dplyr::select(c(-total_accel_arm, -total_accel_dumbbell, -total_accel_forearm, -total_accel_belt))

# Use random forests method; tried many other methods and could not get accuracy above 70%
# This runs significantly slower than those
predModel<-train(classe~.,data=train.clean,method="rf",
                trControl=trainControl(method="oob")) #out of bag

# Use the saved test set from the data partition call earlier for out of sample accuracy               
accuracy <- sum(predict(predModel, train.test)==train.test$classe)/nrow(train.test)
print(accuracy)

# Also tested
predModel.v2<-train(classe~.,data=train.clean,method="gbm", verbose = FALSE)
accuracy.v2 <- sum(predict(predModel.v2, train.test)==train.test$classe)/nrow(train.test)
print(accuracy.v2)

# Prediction strategy: Minimize euclidian distance between input data point and summary table values

train.test <- train.test[,-which(is.na(train.test[1,]))]
train.test <- train.test[,-which(train.test[1,] == "")]
train.test <- train.test[,-c(1, 3:7)]

numTest <- nrow(train.test)
prediction <- character()
init <- 0
for(i in 1:nrow(train.test)){
      userName <- train.test$user_name[i]
      summary.temp <- train.summary[which(train.summary$user_name == userName),]
      summary.temp[,42][summary.temp[,42] == 0] <- .001
      summary.temp[,43][summary.temp[,43] == 0] <- .001
      summary.temp[,44][summary.temp[,44] == 0] <- .001
      tester <- numeric()
      tester.1 <- summary.temp[1,3:8] - train.test[i,2:7]
      tester.1 <- sum((tester.1/(summary.temp[1,3:8]+.0001))^2)
      tester.2 <- summary.temp[2,3:8] - train.test[i,2:7]
      tester.2 <- sum((tester.2/(summary.temp[2,3:8]+.0001))^2)
      tester.3 <- summary.temp[3,3:8] - train.test[i,2:7]
      tester.3 <- sum((tester.3/(summary.temp[3,3:8]+.0001))^2)
      tester.4 <- summary.temp[4,3:8] - train.test[i,2:7]
      tester.4 <- sum((tester.4/(summary.temp[4,3:8]+.0001))^2)
      tester.5 <- summary.temp[5,3:8] - train.test[i,2:7]
      tester.5 <- sum((tester.5/(summary.temp[5,3:8]+.0001))^2)
      
      tester <- append(tester, c(tester.1,tester.2,tester.3,tester.4,tester.5))
      output.possibilities <- c("A", "B", "C", "D", "E")
      
      prediction <- append(prediction, output.possibilities[which(tester == min(tester))])
      if(length(prediction) != i & init == 0){
            print(i)
            init <- 1
      }
}

# Generate dummy variables for user_name
for(name in unique(train.clean$user_name)){
      dummy <- ifelse(train.clean$user_name == name, 1, 0)
      train.clean <- cbind(train.clean, dummy)
      names(train.clean)[ncol(train.clean)] <- name
}

# The data is a set of 38 motion descriptions taken from one of four different
# sensors: Belt, Arm, Forearm, and Dumbbell for a total of 152 columns
# The remaining 8 columns are row number (useless), timestamp (two columns, combine
# them??), user_name (probably not appropriate for this purpose), CVTD timestamp,
# new window (binary), num_window (don't know what this is), and finally
# classe (outcome, predict this)

# Excerpt from HAR data website:
# Six young health participants were asked to perform one set of 10 repetitions
# of the Unilateral Dumbbell Biceps Curl in five different fashions: 
# exactly according to the specification (Class A), 
# throwing the elbows to the front (Class B), 
# lifting the dumbbell only halfway (Class C),
# lowering the dumbbell only halfway (Class D),
# and throwing the hips to the front (Class E)

# Exploratory analysis notes:
qplot(x = roll_belt, y = pitch_belt, data=train, facets = classe ~ .)

# roll_belt only appears to be negative for cases D and E and all of the
# extreme cases on the maximal side occur in case E

df <- train %>% 
      select(roll_belt, classe) %>% 
      group_by(classe) %>% 
      summarize(maximum = max(roll_belt), 
                sd = sd(roll_belt),
                minimum = min(roll_belt))

# predictive rules: if roll_belt > 131, predict E
#                   if roll_belt < -1.24, predict D or E

qplot(x = pitch_belt, y = yaw_belt, data=train, facets = classe ~ .)
qplot(x = total_accel_belt, data=train, facets = classe ~ .)
# Extremely large values of pitch_belt and total_accel_belt 
# appear to indicate E as well
# This makes sense, because E corresponded to throwing hips to the front
# and these are belt measurements, so E should be associated with the
# strongest response from the belt sensors
# Overall predictive rule: if belt sensor sees extreme value, assign to E

qplot(x = roll_dumbbell, data=train, facets = classe ~ .)
# roll_dumbbell has a mass of C observations around -100 to -115

qplot(x = roll_belt, data=train, facets = classe ~ user_name)
# Clear separation between adelmo/charles/pedro and carlitos/eurico/
# jeremy in roll_belt; the former group are all clustered in
# 100-150 and the latter are all clustered around 0
# Assignment rule: if roll_belt < 50, CEJ otherwise ACP

qplot(x = gyros_belt_x, data=train, facets = user_name ~ .)

test <- read.csv("pml-testing.csv")

train_pedro <- dplyr::filter(train, user_name == "pedro")
