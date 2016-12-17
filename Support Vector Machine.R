library(e1071) # svm
library(caret) # tuning via cross-validation

# read in data
d <- read.csv("cleveland.csv")

# sample data

# age trestbps chol thalach oldpeak num
# 63      145  233     150     2.3   0
# 67      160  286     108     1.5   2
# 67      120  229     129     2.6   1
# 37      130  250     187     3.5   0
# 41      130  204     172     1.4   0
# 56      120  236     178     0.8   0


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

# confusion matrix (types of error)
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
