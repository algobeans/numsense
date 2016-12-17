library(randomForest)

##############################################
# Random Forest
##############################################

# read in data
df <- read.csv("crimedayloc.csv")

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

# regression model
rf2 <- randomForest(y = df.train$is.Violent,
                    x = df.train[,-which(names(df.train) %in% "is.Violent")],
                    ntree = 100,
                    nodesize = 1000,
                    importance = T)

# get importance of each predictor
importance(rf)

# predict crime
pred <- predict(rf2,df.test)

threshold <- sort(pred,decreasing=T)[40000]
pred[pred >= threshold] <- 1
pred[pred < threshold] <- 0
table(pred,actual)

##############################################
# Growing 1000 individual trees
##############################################

# vector to store accuracy of each tree
numTrees <- 1000
accuracy <- rep(0,numTrees)

#  list to store all trees
# create 1000 trees
for (i in 1:numTrees){

  # creating a single tree
  tree <- randomForest(y = df.train$is.Violent,
                       x = df.train[,-which(names(df.train) %in% "is.Violent")],
                       ntree = 1,
                       nodesize = 1000,
                       importance = F)
  # calculate accuracy for created tree against test dataset
  pred <- predict(tree,df.test)
  threshold <- sort(pred,decreasing=T)[40000]
  pred[pred >= threshold] <- 1
  pred[pred < threshold] <- 0
  accuracy[i] <- table(pred,actual)[2,2]
  if(i == 1){
    forest <- tree
  } else {
    forest <- combine(forest,tree)
  }
  print(i)
}

pred<-predict(forest,df.test)
threshold <- sort(pred,decreasing=T)[40000]
pred[pred >= threshold] <- 1
pred[pred < threshold] <- 0
table(pred,actual)[2,2]

hist(accuracy,breaks=(length(accuracy)/10),
     xlim=c(0.55,0.9),xlab="Accuracy",ylab="Number of trees")


##############################################
# Crime Heatmap
##############################################

kk <- as.numeric(names(sort(table(dd$Location),decreasing=TRUE)))
kkx <- kk %% 23
kky <- (kk %/% 23) + 1
df.test.tp <- df.test[pred==actual & actual==1,]
df.test.fn <- df.test[pred!=actual & actual==1,]
df.test.fp <- df.test[pred!=actual & actual==0,]
kkmain <- c("(Sat)","(Sun)","(Mon)","(Tue)","(Wed)","(Thu)","(Fri)")

for(i in 1:7){
  tpx <- df.test.tp[df.test.tp$DayID==(766+i),]$X
  tpy <- df.test.tp[df.test.tp$DayID==(766+i),]$Y
  fnx <- df.test.fn[df.test.fn$DayID==(766+i),]$X
  fny <- df.test.fn[df.test.fn$DayID==(766+i),]$Y
  fpx <- df.test.fp[df.test.fp$DayID==(766+i),]$X
  fpy <- df.test.fp[df.test.fp$DayID==(766+i),]$Y
  tppch <- rep(19,length(tpx))
  fnpch <- rep(4,length(fnx))
  fppch <- rep(1,length(fpx))
  tpcol <- rep("black",length(tpx))
  fncol <- rep("black",length(fnx))
  fpcol <- rep("black",length(fpx))
  tpcex <- rep(1,length(tpx))
  fncex <- rep(1,length(fnx))
  fpcex <- rep(1,length(fpx))

  kkcol <- c(rep("red",60),rep("orange",95),rep("yellow",145),rep("grey",341))
  plot(c(kkx,tpx,fnx,fpx),
       c(kky,tpy,fny,fpy),
       pch = c(rep(15,641),tppch,fnpch,fppch),
       col = c(kkcol,tpcol,fncol,fpcol),
       cex = c(rep(2,641),tpcex,fncex,fpcex),
       xlab = "X-Axis",
       ylab = "Y-Axis",
       main = paste("Day",i,kkmain[i]),
       xlim = c(0,22)
  )
}

