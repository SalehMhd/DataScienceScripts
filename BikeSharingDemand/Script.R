train <- read.csv(file = "E:/DataScience/BikeSharingDemand/train.csv", stringsAsFactors = FALSE)

library(lubridate)

train$Hour <- hour(ymd_hms(train$datetime))
train$Month <- month(ymd_hms(train$datetime))
train$Weekday <- wday(ymd_hms(train$datetime), label = TRUE, abbr = TRUE)

#Afer plotting Month VS count on season
train$Season2[train$Month %in% c(3,4,5)] <- 1
train$Season2[train$Month %in% c(6,7,8)] <- 2
train$Season2[train$Month %in% c(9,10,11)] <- 3
train$Season2[train$Month %in% c(12,1,2)] <- 4


drops <- c('Season2')
train <- train[, !(names(train) %in% drops)]

summary(train)
str(train)
# 
# a <- "2011-44-33 22:11:00"
# strsplit(strsplit(a, " ")[[1]][1], "-")[[1]][3]
# sapply(a, FUN = function(x) strsplit(x, ":")[[1]][1])
# 
# 
# train$Date <- sapply(train$datetime, FUN = function(x) strsplit(x, " ")[[1]][1] )
# train$Time <- sapply(train$datetime, FUN = function(x) strsplit(x, " ")[[1]][2] )
# 
# train$Year <- sapply(train$Date, FUN = function(x) strsplit(x, "-")[[1]][1])
# train$Month <- sapply(train$Date, FUN = function(x) strsplit(x, "-")[[1]][2])
# train$Day <- sapply(train$Date, FUN = function(x) strsplit(x, "-")[[1]][3])
# 
# train$Hour <- sapply(train$Time, FUN = function(x) strsplit(x, ":")[[1]][1])
# train$Minute <- sapply(train$Time, FUN = function(x) strsplit(x, ":")[[1]][2])
# train$Second <- sapply(train$Time, FUN = function(x) strsplit(x, ":")[[1]][3])

# train$Weekend <- abs(abs(train$workingday-1) - train$holiday)

factorVars <- c('season', 'holiday', 'workingday', 'weather', 'Season2')
train[factorVars] <- lapply(train[factorVars], function(x) as.factor(x))


#Visulalizing

#Varying of count along with Month and temperature
ggplot(data = train[order(train$temp),], 
       mapping = aes(x = as.factor(Month), y = count, fill = temp)) + 
  geom_bar(stat = "identity")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "green", midpoint = mean(train$temp))


ggplot(data = train[order(train$Weekday),], 
       mapping = aes(x = as.factor(Hour), y = count, fill = Weekday)) + 
  geom_bar(stat = "identity")


ggplot(data = train[order(train$workingday),], 
       mapping = aes(x = as.factor(Hour), y = count, fill = workingday)) + 
  geom_bar(stat = "identity")


ggplot(data = train[order(train$weather),], 
       mapping = aes(x = as.factor(Hour), y = count, fill = weather)) + 
  geom_bar(stat = "identity")
  
ggplot(data = train[order(train$weather),], 
       mapping = aes(x = as.factor(Month), y = count, fill = weather)) + 
  geom_bar(stat = "identity")


#Important to show the misunderstanding
ggplot(data = train[order(train$season),], 
       mapping = aes(x = as.factor(Month), y = count, fill = season)) + 
  geom_bar(stat = "identity")

ggplot(data = train[order(train$Season2),], 
       mapping = aes(x = as.factor(Month), y = count, fill = Season2)) + 
  geom_bar(stat = "identity")
###### End of error correcting


ggplot(data = train[order(train$Season2),], 
       mapping = aes(x = as.factor(Hour), y = count, fill = Season2)) + 
  geom_bar(stat = "identity")


#North Sphere Country
ggplot(data = train, 
       mapping = aes(x = as.factor(Month), y = temp, color = season)) +
  geom_boxplot() +
  labs(x = "Month", y = "Temperature(Celsius)")







ggplot(data = train[order(train$holiday),], 
       mapping = aes(x = as.factor(Month), y = count, fill = holiday)) + 
  geom_bar(stat = "identity")


df2 <- aggregate(count ~ Month, data = train, sum)
ggplot(data = df2) + 
  geom_col(mapping = aes(x = Month))

scale_colour_gradient(low = "#0000ff", high = "#ff0000")
cbbPalette <- c("#0000ff", "#00ffff", "#00ff00", "#ffff00", "#ff0000")



sum(train$count[train$Month == "1"])


#All variables except: Casual, registered


library(randomForest)
model <- randomForest(count ~ season+holiday+workingday+weather+temp+atemp+humidity+windspeed+Month+Hour+Weekday,
                      data = train,
                      importance = TRUE)

b <- importance(model)
sort(b[,1])




#Day & Hour - too many values (Gini mistake) - is the second important variable, know why because it is weird (Gini problem is expected with variable has many values)
#Day: values from 1 -> 19 ????
#Hour: too many values split them into morning, afternoon, evening, night
#Introduce a variable: Working ride (From 7 to 9) and (from 16 to 18)
#Introduce a variable: Picnic ride (From 10 to 16) and Holiday or Weekend
#Formate DateTime: http://stats.stackexchange.com/questions/147063/r-how-to-separate-date-time-data-types 
#Make Hour, Day, Month as continous variables (int)

install.packages("magrittr")
library("magrittr")

which( is.na(train) == TRUE )

a <- subset(train, Month == "01" & Year == "2011")
a$datetime


which(train$Weekend[train$Month == "01" & train$Year == "2011"] == 1)
which(train$holiday[train$Month == "01" & train$Year == "2011"] == 1)


rle(train$Weekend)
