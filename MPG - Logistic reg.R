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
carMelt <- carData[,c(1:8, 10)]
head(carMelt)
meltMpg <- melt(carMelt, id ='mpg_binary')
str(meltMpg)

ggplot(meltMpg, aes(mpg_binary, variable, fill= mpg_binary)) +
 geom_bar(stat = "identity" ) +
  facet_wrap(~variable, as.table = FALSE )


#split the data manually
set.seed(198)
trainIndex <- createDataPartition(carData$mpg_binary, p = .5,
                                  list = FALSE,
                                  times = 1)
mpgTrain <- carData[ trainIndex,]
mpgTest  <- carData[-trainIndex,]

head(mpgTrain)

#running the logit regression
mpg1 <- glm(mpg_binary ~ cylinders + displacement + horsepower + weight + acceleration + year + origin,
                 family="binomial"(link="logit"), data = carData)
summary(mpg1)
pR2(mpg1)

# Weight casuses over fitting (Macfadden > 0.4)
mpg2 <- glm(mpg_binary ~ weight,
            family="binomial"(link="logit"), data = carData)
summary(mpg2)
pR2(mpg2)

# Displacment also causes over fitting (Macfadden > 0.4) 
mpg3 <- glm(mpg_binary ~ displacement,
            family="binomial"(link="logit"), data = carData)
summary(mpg3)
pR2(mpg3)

names(carData)

# Best Model - McFadden is between 0.2 - 0.4  and AIC value highest among models tested  
mpg4 <- glm(mpg_binary ~ horsepower + acceleration + origin,
            family="binomial"(link="logit"), data = carData)
summary(mpg4)
pR2(mpg4)

#95% confidence intervals
confint.default(mpg4)


#using mpg4 to predict on test set
ClassProbs <- predict(mpg4, mpgTest, type="response")

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
