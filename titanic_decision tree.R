#install.packages("mice")
library(caTools)
library(mice)
library(rpart)

setwd('C:\\Venkat\\Data Science Projects\\Titanic')

#load the data set
titanic = read.csv('train.csv')

# analyze features
str(titanic)
titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
titanic$SibSp = as.factor(titanic$SibSp)
titanic$Parch = as.factor(titanic$Parch)

#analyse missing data
md.pattern(titanic)

#impute age with mean
titanic$Age = ifelse(is.na(titanic$Age), 
                     mean(titanic$Age,na.rm = TRUE),
                     titanic$Age)

#scale numeric features
titanic$Age = scale(titanic$Age)
titanic$Fare = scale(titanic$Fare)

#create training and test set
set.seed(123)
validation = sample.split(titanic$Survived, SplitRatio = 0.8)
train = subset(titanic, validation==TRUE)
test = subset(titanic, validation==FALSE)

#fit model
classifier = rpart(formula = Survived~Pclass+Sex+Age+SibSp+Fare+Embarked,
                 data = train)

selected_features = c(3,5,6,7,10,12)
y_predict = predict(classifier, 
                       newdata = test[,selected_features],
                       type = 'class')

confusion_matrix = table(test$Survived,y_predict)
confusion_matrix

#predict for competitiion
competition = read.csv('test.csv')
competition$Pclass = as.factor(competition$Pclass)
competition$SibSp = as.factor(competition$SibSp)
competition$Parch = as.factor(competition$Parch)

str(competition)
competition$Age = ifelse(is.na(competition$Age),
                         mean(competition$Age, na.rm = TRUE),
                         competition$Age)
competition$Age = scale(competition$Age)
competition$Fare = scale(competition$Fare)

selected_features = c(2,4,5,6,9,11)
competition_predict = predict(classifier,
                              type = 'class',
                              newdata = competition[,selected_features])

competition_output = data.frame(competition$PassengerId, 
                                competition_predict)

write.csv(competition_output, 'Test_Prediction_Submission - decision tree.csv')
