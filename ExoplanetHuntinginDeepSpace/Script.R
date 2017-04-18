train <- read.csv(file = "D:/DataScience/ExoplanetHuntinginDeepSpace/exoTrain.csv", stringsAsFactors = FALSE)

ExoStar <- train[train$LABEL==2,]
NonExoStar <- train[train$LABEL==1,]

starNumber <- 2

p <- ExoStar[starNumber,3:length(ExoStar[1,])]
p <- t(p)
a <- c(1:length(p[,1]))
p <- data.frame(p,a)
colnames(p) <- c('value','time')
ggplot(data = p, mapping = aes(x = time, y = value))+
  geom_point() +
  geom_smooth (method = "loess")

