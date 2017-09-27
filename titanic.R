install.packages("mice")
library(caTools)
library(mice)

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
classifier_base = glm(formula = Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                 data = train, family = binomial)
summary(classifier_base)

## remove stastiscally insignificant features from the model
classifier_final = glm(formula = Survived~Pclass+Sex+Age+SibSp,
                       family = binomial,
                       data = train)
summary(classifier_final)

## predict for the test set
selected_features = c(3,5,6,7)
test_predict = predict(classifier_final, 
                       newdata = test[,selected_features],
                       type = 'response')

test_prob = ifelse(test_predict > 0.5,
                   TRUE,
                   FALSE)
confusion_matrix = table(test$Survived,test_prob)

#predict for competitiion
competition = read.csv('test.csv')
competition$Pclass = as.factor(competition$Pclass)
competition$SibSp = as.factor(competition$SibSp)

competition$Age = ifelse(is.na(competition$Age),
                         mean(competition$Age, na.rm = TRUE),
                         competition$Age)
competition$Age = scale(competition$Age)

selected_features = c(2,4,5,6)
competition_predict = predict(classifier_final,
                              type = 'response',
                              newdata = competition[,selected_features],
                              na.action = na.pass)

competition_prob = ifelse(competition_predict > 0.5,
                          1,
                          0)

competition_output = data.frame(competition$PassengerId, 
                                competition_prob, row.names = c(id, id1, id2))

write.csv(competition_output, 'Test_Prediction_Submission - Logistic Regression.csv')
getwd()
