library(randomForest)
library(datasets)

setwd("C:/Users/Machine/Desktop/Pattern")
yelp_train = read.csv("PAbusiness_regression_input.csv")
colnames(yelp_train)
yelpData <- yelp_train[,c('clusters',
                           'regression_value',
                           'after_Jan2010_count',
                           'after_Jan2014_count',
                           'after_Jun2014_count',
                           'after_Jan2015_count',
                           'weighted_regression_value',
                           'ratings_avg',
                           'users_sum'
                           )]

str(yelpData)
colnames(yelpData)

## setting the seed to make our partition reproducable exactly the same way:
set.seed(4410)
## Taking training dataset as 80% of the sample size:
smp_size <- floor(0.80 * nrow(yelpData))
train_ind <- sample(seq_len(nrow(yelpData)), size = smp_size)
train <- yelpData[train_ind, ]
test <- yelpData[-train_ind, ]

## randomizing the test dataset:
smp_size <- floor(1* nrow(test))
test_ind <- sample(seq_len(nrow(test)), size = smp_size)
test <- test[test_ind, ]
set.seed(4410)
ozone.rf <- randomForest(regression_value ~ ., data=train, mtry=6,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)
# ozone.pred <- predict(ozone.rf, testData, predict.all=TRUE)

ozone.pred <- predict(ozone.rf, yelpData, nodes=TRUE)
yelp_train[,"predicted_value"]=ozone.pred
table(observed = yelpData[, "regression_value"], predicted = ozone.pred)
head(test, n=10)
write.csv(yelp_train, file = "PAregression_output_model.csv")

