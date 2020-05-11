# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_classification.csv")
View(movie)
str(movie)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE) #imputasi with mean because it is numerical variable
movie$Start_Tech_Oscar <- as.factor(movie$Start_Tech_Oscar)

# Test-Train Split #
install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
trainc = subset(movie,split == TRUE)
testc = subset(movie,split == FALSE)

#################################### POLYNOMIAL KERNEL SVM #######################################
install.packages('e1071')
library (e1071)

# Modeling #
svmfitR = svm(Start_Tech_Oscar~., data=trainc , kernel = "radial", gamma =1, cost =1)
summary (svmfitR)
svmfitR$index #index of support vectors

# Predicting #
predr <- predict(svmfitR,testc)
cm <- table(predict = predr, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

# Tuning Parameter
tuneR = tune(svm ,Start_Tech_Oscar~.,data=trainc ,kernel="radial", ranges = list(cost=c(0.001, 0.01,0.1 ,1 ,10 ,100 ,1000), gamma=c(0.01, 0.1,0.5,1,2,3,4,10, 50) ))
summary(tuneR)
bestmodR =tuneR$best.model
summary (bestmodR) #cost=10, gamma=0.5
predR <- predict(bestmodR, testc)
cm <- table(predict = predR, truth = testc$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

