
library(randomForest)

# read in data
df <- read.csv("crime.csv")

# sample data
# Year Month Day DayOfWeek Location is.Violent is.Property DayID
# 2014     1   1         3        1      FALSE       FALSE     1
# 2014     1   2         4        1      FALSE       FALSE     2
# 2014     1   3         5        1      FALSE       FALSE     3
# 2014     1   4         6        1      FALSE       FALSE     4
# 2014     1   5         7        1      FALSE       FALSE     5
# 2014     1   6         1        1      FALSE       FALSE     6
# 2014     1   7         2        1      FALSE       FALSE     7
# 2014     1   8         3        1      FALSE       FALSE     8
# 2014     1   9         4        1      FALSE       FALSE     9
# 2014     1  10         5        1      FALSE       FALSE    10
# ..  ...   ... ...       ...      ...        ...         ...   ...
# Variables not shown: LastDay (int), LastWeek (int), LastMonth (int),
# Tmax (int), Tmin (int), Tavg (int), Depart (int), Heat (int), Cool
# (int), PrecipTotal (dbl)


# getting grid coordinates for crime location
df$X <- as.numeric(df$Location) %% 23
df$Y <- 1 + (as.numeric(df$Location) %/% 23)

# split into training and test datasets
df.test <- df[df$Year==2016,]
df.train <- df[df$Year<2016,]

# drop irrelevant variables
dropVar <- c("Year","is.Property","Location")
df.test <- df.test[,-which(names(df.test) %in% dropVar)]
df.train <- df.train[,-which(names(df.train) %in% dropVar)]
actual <- df.test$is.Violent
df.test <- df.test[,-which(names(df.test) %in% c("is.Violent"))]

# shortlist variables
pred <- c("Month","Tavg","Heat","Cool","PrecipTotal")
df.test <- df.test[,-which(names(df.test) %in% pred)]
df.train <- df.train[,-which(names(df.train) %in% pred)]

# format variables
df.test$DayOfWeek <- as.factor(df.test$DayOfWeek)
df.train$DayOfWeek <- as.factor(df.train$DayOfWeek)


# classification model
rf <- randomForest(y = as.factor(df.train$is.Violent),
                   x = df.train[,-which(names(df.train) %in% "is.Violent")],
                   ntree = 10,
                   nodesize = 500,
                   importance = T)

# get importance of each predictor
importance(rf)
varImpPlot(rf)

# predict crime
pred <- predict(rf,df.test)

# classification accuracy
table(pred,actual)


