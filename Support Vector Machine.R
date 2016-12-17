setwd("/Users/Annalyn/Work/Book Project/11 SVM/data")

rm(list=ls())

library(e1071) # svm
library(caret) # tuning via cross-validation

# read in data
d <- read.csv("processed.cleveland (cont var).csv")


# code outcome as binary
# 0 (healthy) vs. 1 (at risk)
d$num[d$num > 0] = 1
d$num <- factor(d$num)

# select predictors:
# maximum heart rate achieved (thalach) vs.
# ST depression (ECG) induced by exercise wrt rest (oldpeak)


# cross-validation to tune parameters
svmModel <- train(d$num ~ d$age + d$thalach,
                  data = d, method = "svmRadial",
                  tuneGrid = expand.grid(
                    sigma=seq(1,3,by=0.5),
                    C=seq(0.1,2,by=0.1)),
                  trControl = trainControl(method = "cv"))
gamma <- svmModel$bestTune$sigma
cost <- svmModel$bestTune$C


# predictions
model <- svm(num ~ age + thalach, data = d,
             kernel = "radial",
             gamma = gamma,
             cost = cost)
prop.table(table(fitted(model, d), d$num),2)
err <- prop.table(table(fitted(model, d), d$num))
err
sum(err[1,1], err[2,2])


# plot
plot(model, data = d,
     thalach ~ age,
     ylab = "Maximum Heart Rate during Exercise",
     xlab = "Age",
     grid = 50,
     symbolPalette = c("green", "black"),
     svSymbol = 16,
     dataSymbol = 16,
     color.palette = terrain.colors)

# #e89e00

# library(kernlab)
# model <- ksvm(num ~ oldpeak + thalach, data = d, type="C-svc")
# plot(model, data=d,
#      grid = 50,
#      pch = 20)
