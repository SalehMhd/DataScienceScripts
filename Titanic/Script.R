TitanicTrain <- read.csv("E:/DataScience/Titanic/train.csv", header = TRUE)

ggplot(data = TitanicTrain) + 
  geom_point(mapping = aes(x = Ticket, y = Fare))


table(TitanicTrain$Ticket) #Count the duplicated tickets

#Exclude: PassengerId, Name, Ticket, Cabin

#Include: Pclass, SibSp, Parch, Embarked, Fare

#Forsure: Sex, Age

#Class: Survived

table(TitanicTrain$Fare)

levels(factor(TitanicTrain$Fare))
levels(factor(TitanicTrain$Cabin))

ggplot(data = TitanicTrain) + 
  geom_point(mapping = aes(x = Fare, y = Survived))

sum(is.na(TitanicTrain$Age))

levels(factor(TitanicTrain$Embarked))
TitanicTrain$Parch <- factor(TitanicTrain$Parch)
TitanicTrain$SibSp <- factor(TitanicTrain$SibSp)

ggplot(data = TitanicTrain) + 
  geom_point(mapping = aes(x = Age, y = Survived))

levels(factor(TitanicTrain$Age))  

newVar <- table(TitanicTrain$Age, TitanicTrain$Survived)
newVar[,2]
MajorSurvived <- newVar[,2] > newVar[,1]

install.packages("randomForest")

table(TitanicTrain$Survived)

#Split 500(training) - 241 (Validation)
TitanicTrain_NA_Removed <- filter(TitanicTrain, !is.na(Age))

ValidationSetIndex <- runif(214, 1, 714)
TrainingSetIndex <- setdiff(1:714, ValidationSetIndex)

#TitanicValidation <- sample_n(TitanicTrain_NA_Removed, 214)
#TitanicTraining <- setdiff(TitanicTrain_NA_Removed, TitanicValidation)

table(TitanicValidation$Survived)['1'] / table(TitanicValidation$Survived)['0']

table(TitanicTraining$Survived)['1'] / table(TitanicTraining$Survived)['0']

table(TitanicTrain_NA_Removed$Survived)['1'] / table(TitanicTrain_NA_Removed$Survived)['0']


featuresModel <- randomForest(Survived ~ Sex+Age+Pclass+SibSp+Parch+Embarked+Fare, data = TitanicTrain_NA_Removed, ntree = 10000)

predictModel <- randomForest(Survived ~ Sex+Fare+Age+Pclass, data = TitanicTrain_NA_Removed)
randomForest(Survived ~ Sex+Fare+Age+Pclass, data = TitanicTrain)

MeanDecreaseGini
Sex              81.35524
Age              50.00014
Pclass           30.56877
SibSp            13.72750
Parch            11.99320
Embarked          8.65394
Fare             54.45188





#Missing Values
ActualDataSet <- TitanicTrain[,c('Survived','Sex','Age','Pclass','SibSp','Parch','Embarked','Fare')]
ActualDataSet$Sex <- factor(ActualDataSet$Sex)
ActualDataSet$Pclass <- factor(ActualDataSet$Pclass)
ActualDataSet$SibSp <- factor(ActualDataSet$SibSp)
ActualDataSet$Parch <- factor(ActualDataSet$Parch)
ActualDataSet$Embarked <- factor(ActualDataSet$Embarked)
ActualDataSet$Survived <- factor(ActualDataSet$Survived)

Dataset_NaImputed <- rfImpute(Survived ~ Sex+Age+Pclass+SibSp+Parch+Embarked+Fare, ActualDataSet)

Dataset_NaImputed <- rfImpute(Survived ~ Sex+Age+Pclass+Fare, ActualDataSet)

#Train the model
model <- randomForest(Survived ~ Sex+Fare+Age+Pclass, data = Dataset_NaImputed)


#Prediction Results
TitanicTestOriginal <- read.csv("E:/DataScience/Titanic/test.csv", header = TRUE)

TitanicTest <- TitanicTestOriginal
TitanicTest$Survived <- factor(1)
TitanicTestTemp <- TitanicTestOriginal
TitanicTestTemp$Survived <- factor(0)

FullTestSet <- rbind(TitanicTest, TitanicTestTemp)

FullTestSet$Sex <- factor(FullTestSet$Sex)
FullTestSet$Pclass <- factor(FullTestSet$Pclass)
FullTestSet$SibSp <- factor(FullTestSet$SibSp)
FullTestSet$Parch <- factor(FullTestSet$Parch)
FullTestSet$Embarked <- factor(FullTestSet$Embarked)

ActualTitanicTest <- rfImpute(Survived ~ Sex+Age+Pclass+Fare, FullTestSet)

TestWithoutlabel <- filter(ActualTitanicTest, Survived == 1)
TestWithoutlabel <- subset(TestWithoutlabel, select = c('Sex', 'Age', 'Pclass', 'Fare'))


result <- predict(model, TestWithoutlabel)


TestWithoutlabel$result <- result


rownames(TitanicTest) <- TitanicTest$PassengerId
predict(model, TitanicTest)

#ActualTestSet <- TitanicTest[,c('Survived','Sex','Age','Pclass','SibSp','Parch','Embarked','Fare')]


is.na(TitanicTest$Age)


a <- randomForest(Survived ~ Sex+Age+Pclass+SibSp+Parch+Embarked+Fare, data = TitanicTrain, ntree = 10000)

a <- TitanicTrain
a$Sex <- factor(a$Sex)
a$Pclass <- factor(a$Pclass)
a$SibSp <- factor(a$SibSp)
a$Parch <- factor(a$Parch)
a$Embarked <- factor(a$Embarked)
a$Survived <- factor(a$Survived)


summary(a)