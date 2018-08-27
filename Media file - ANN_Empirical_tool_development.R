setwd('E:/PAWAN/Pothole_project/Empirical tool/data_analysis/Combined data for Walnut-Bunny-Lettuce/18th Aug 2018')

new.dat <- read.csv("./newdata_PRISM_2000-2016_1.csv")

# Let's create the train and test dataset
library(caret)
library(caTools)
ind = createDataPartition(new.dat$DailyWaterDepth_mm, p = 2/3, list = FALSE)

trainDF <- new.dat[ind,]
testDF <- new.dat[-ind,]



library(caret)

library(doSNOW)

cl <- makeCluster(7, type = "SOCK")

registerDoSNOW(cl)

nnetTune_ <- train(y = trainDF$DailyWaterDepth_mm,
                  x = trainDF[,2:11],
                  tuneGrid = expand.grid(size = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
                                         decay = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)),
                  method = "nnet", 
                  trace = FALSE,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  maxit = 500,
                  trControl = trainControl(method = "repeatedcv", repeats = 50, number = 10))

saveRDS(nnetTune_, file = "./nnetTune.rds")

x <- readRDS("./nnetTune.rds")


stopCluster(cl)

summary(nnetTune_)
plot(nnetTune_)

summary(x)
plot(x)


###########  PREDICTIONS  ###########

testDF$predictions <- predict(nnetTune_, newdata = testDF)
str(testDF$predictions)

library(csv)
write.csv(testDF, "E:/PAWAN/Pothole_project/Empirical tool/data_analysis/Combined data for Walnut and Bunny/testDF.csv")

str(testDF)
head(testDF)


#Mean squared error
MSE <- sum((testDF$predictions - testDF$DailyWaterDepth_mm)^2)/nrow(testDF)
MSE


# Statistical analysis

R2 <- 1 - (sum((testDF$DailyWaterDepth_mm-testDF$predictions)^2)/sum((testDF$DailyWaterDepth_mm-mean(testDF$DailyWaterDepth_mm))^2))
R2

