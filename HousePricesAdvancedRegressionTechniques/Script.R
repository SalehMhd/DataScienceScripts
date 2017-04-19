train <- read.csv(file = "D:/DataScience/Datasets/HousePricesAdvancedRegressionTechniques/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "D:/DataScience/Datasets/HousePricesAdvancedRegressionTechniques/test.csv", stringsAsFactors = FALSE)
test$SalePrice <- -1
dataset <- rbind(train, test)

#####Preprocessing

##Removing many levels variables (Id)
sort(unlist(lapply(dataset, function(x) length(levels(as.factor(x))))))
#Excluding Id(1459 levels) variables
dataset <- dataset[ , -which(names(dataset) %in% c("Id")) ]


unlist(lapply(dataset, function(x) any(is.na(x))))
a <- lapply(dataset, function(x) sum(is.na(x)))
sort(unlist(a), decreasing = FALSE)

#Replacing values of FireplaceQu, Fence, Alley, MiscFeature, PoolQC with Addequate values
dataset[which(is.na(dataset$PoolQC)), c('PoolQC')] <- c('NoPool')
dataset[which(is.na(dataset$MiscFeature)), c('MiscFeature')] <- c('NoMiscFeature')
dataset[which(is.na(dataset$Alley)), c('Alley')] <- c('NoAlley')
dataset[which(is.na(dataset$Fence)), c('Fence')] <- c('NoFence')
dataset[which(is.na(dataset$FireplaceQu)), c('FireplaceQu')] <- c('NoFireplaceQu')

#Set Na as NoGarage
dataset[which(is.na(dataset$GarageType)), c('GarageType','GarageYrBlt','GarageFinish','GarageQual','GarageCond')] <- c('NoGarage', '1111', 'NoGarage', 'NoGarage', 'NoGarage')

#Set Na as NoBsmt
dataset[which(is.na(dataset$BsmtFinType1)), c('BsmtQual','BsmtCond','BsmtFinType1','BsmtExposure','BsmtFinType2')] <- c('NoBsmt', 'NoBsmt', 'NoBsmt', 'NoBsmt', 'NoBsmt')

#####Predicting values of LotFrontage with a decision tree

library('rpart')

completeDataset <- dataset[which(complete.cases(dataset) == TRUE),]
incompleteDataset <- dataset[which(complete.cases(dataset) == FALSE),]

lotFrontageModel <- rpart(LotFrontage ~ ., data = completeDataset)

which(is.na(incompleteDataset[, 1:3]))

#####Missing values
#Missing Values solving in increasing order of missing values for each variable

#Order 1(test set): Exterior1st, Exterior2nd, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, KitchenQual, 
#GarageCars, GarageArea, SaleType, Utilities, BsmtFullBath, BsmtHalfBath, Functional, MSZoning
#Order 1(training set): Electrical
#Order 2: MasVnrArea, MasVnrType
#Order 3: BsmtFinType1, BsmtFinType2, BsmtQual, BsmtExposure, BsmtCond
#Order 4: GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond
#Order 5: LotFrontage

###Missing Values: over correlation
lapply(train, function(x) cor(train$Electrical, x, use = "complete.obs"))

class(train$Electrical)
which(is.na(train[,'MasVnrType']))
which(is.na(train[,'MasVnrArea']))

#NA here means no basement. excepts the indicated rows
#Set Na as NoBsmt
which(is.na(train[,'BsmtQual']))
which(is.na(train[,'BsmtCond']))
which(is.na(train[,'BsmtFinType1']))
which(is.na(train[,'BsmtExposure'])) #Row: 949
which(is.na(train[,'BsmtFinType2'])) #Row: 333
which(is.na(test[,'BsmtQual']))
which(is.na(test[,'BsmtCond']))
which(is.na(test[,'BsmtFinType1']))
which(is.na(test[,'BsmtExposure'])) #Row: 949
which(is.na(test[,'BsmtFinType2'])) #Row: 333

train[which(is.na(train$BsmtQual)), c('BsmtQual','BsmtCond','BsmtFinType1','BsmtExposure','BsmtFinType2')] <- c('NoBsmt', 'NoBsmt', 'NoBsmt', 'NoBsmt', 'NoBsmt')


#NA here means no Garaget.
#Set Na as NoGarage
which(is.na(train[,'GarageType']))
which(is.na(train[,'GarageYrBlt']))
which(is.na(train[,'GarageFinish']))
which(is.na(train[,'GarageQual']))
which(is.na(train[,'GarageCond']))
which(is.na(test[,'GarageType']))
which(is.na(test[,'GarageYrBlt']))
which(is.na(test[,'GarageFinish']))
which(is.na(test[,'GarageQual']))
which(is.na(test[,'GarageCond']))
train[which(is.na(train$GarageType)), c('GarageType','GarageYrBlt','GarageFinish','GarageQual','GarageCond')] <- c('NoGarage', '1111', 'NoGarage', 'NoGarage', 'NoGarage')




library(randomForest)
model <- randomForest::randomForest(SalePrice ~ ., data = train)