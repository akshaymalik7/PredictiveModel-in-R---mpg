#libraries
options(scipen=999)
library(aod)
library(AppliedPredictiveModeling)
library(randomForest)
library(popbio)
library(caret)
library(pscl)
library(ROCR)
library(ggmap)
library(pROC)
library(reshape2)

#pull in data
setwd("~/Documents/Github Code/data-analyst-data-test")
carData <- read.csv("Cars_mileage.csv")

#Reading data
head(carData)
str(carData)
summary(carData)

#create a new column called mpg_binary 
medMPG <- median(carData$mpg)
carData$mpg_binary <- with(carData, ifelse(mpg >median(mpg), 1, 0 ))
carData$mpg_binary <- as.factor(carData$mpg_binary)
carData$year <- as.factor(carData$year)
carData$origin <- as.factor(carData$origin)
carData$cylinders <- as.factor(carData$cylinders)
carData$horsepower <- as.integer(carData$horsepower)

#carData$ <- as.factor(carData$mpg_binary)
#carData$ <- as.factor(carData$mpg_binary)

#plotting varibales
ggplot(data =carData) +
  geom_point(mapping = aes(x= horsepower , y= mpg_binary )) 

ggplot(data =carData) +
  geom_point(mapping = aes(x= acceleration, y= mpg_binary)) 

#plotting by melting along the dependent variable
carMelt <- carData[,c(2:8, 10)]
head(carMelt)
meltMpg <- melt(carMelt, id ='mpg_binary')
str(meltMpg)

ggplot(meltMpg, aes(mpg_binary, variable, fill= mpg_binary)) +
  geom_bar(stat = "identity" ) +
  facet_wrap(~variable, as.table = FALSE )


#fit in random forest
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(100)
rfMPG <- train(mpg_binary ~ ., data = carMelt, method = "rf", trControl = fitControl,metric= 'Accuracy', importance=TRUE)
rfMPG
plot(rfMPG)

#look at the variable importance
varImp(rfMPG)

#plot predicted/observed
plot(exp(predict(rfFit,car)), carData$mpg_binary)
#MAPE
mean(abs((Boston$medv - exp(predict(rfFit,Boston)))/ Boston$medv))

MpgProb <- predict(rfFit, mpgTest, type="response")

#lets look at the distribution of the probabilities
hist(ClassProbs)
head(ClassProbs)

#put these into a data frame so we can compare probabilities with actual 
testProbs <- data.frame(Class = mpgTest$mpg_binary , Probs = ClassProbs) 
head(testProbs) 


#plot the distrubtion of predictied probabilities for each binary output.
testProbsPlot <- ggplot(testProbs, aes(x = Probs, fill= Class)) + geom_density() +
  facet_grid(Class ~ .) + xlab("Probability") + geom_vline(xintercept = .5)
testProbsPlot

#create a cutoff threshhold
testProbs$predClass  = ifelse(testProbs$Probs > .5 ,1,0)
head(testProbs)

#lets do a confusion matrix
confusionMatrix(testProbs$Class ,testProbs$predClass)


#an ROC Curve
pred <- prediction( testProbs$Probs, testProbs$Class)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

#lets calculate Area Under the Curve or 'AUC'
auc(testProbs$Class, testProbs$Probs) 

