train <- read.csv(file = "D:/DataScience/Datasets/HousePricesAdvancedRegressionTechniques/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "D:/DataScience/Datasets/HousePricesAdvancedRegressionTechniques/test.csv", stringsAsFactors = FALSE)

library(randomForest)

which(is.na(train$SalePrice) == TRUE)

unlist(lapply(train, function(x) any(is.na(x))))
a <- lapply(train, function(x) sum(is.na(x)))
sort(unlist(a), decreasing = FALSE)

#Missing Values solving by order

#Order 1: Exterior1st, Exterior2nd, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, KitchenQual, 
#GarageCars, GarageArea, SaleType, Utilities, BsmtFullBath, BsmtHalfBath, Functional, MSZoning
#Order 2: MasVnrArea, MasVnrType
#Order 3: BsmtFinType1, BsmtFinType2, BsmtQual, BsmtExposure, BsmtCond
#Order 4: GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond
#Order 5: LotFrontage
#Exclude: FireplaceQu, Fence, Alley, MiscFeature, PoolQC

#Excluding so many missing value variables
#Excluding: FireplaceQu, Fence, Alley, MiscFeature, PoolQC
train <- train[ , -which(names(train) %in% c("FireplaceQu","Fence","Alley","MiscFeature","PoolQC")) ]


sort(unlist(lapply(train, function(x) length(levels(as.factor(x))))))


model <- randomForest::randomForest(SalePrice ~ ., data = train)