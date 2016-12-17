library(class) # for knn
library(scales) # transparent plot points
library(ElemStatLearn) # visualize knn
library(caret) # for cross-validation

####################################

# read in and normalize wine data
dred <- read.csv("winequality-red.csv")
dred$c <- 2

dwhite <- read.csv("winequality-white.csv")
dwhite$c <- 1

# sample data

#   fixed.acidity volatile.acidity citric.acid residual.sugar
# 1           7.4             0.70        0.00            1.9
# 2           7.8             0.88        0.00            2.6
# 3           7.8             0.76        0.04            2.3
# 4          11.2             0.28        0.56            1.9
# 5           7.4             0.70        0.00            1.9
# 6           7.4             0.66        0.00            1.8
# Variables not shown: chlorides (dbl), free.sulfur.dioxide
# (dbl), total.sulfur.dioxide (dbl), density (dbl), pH
# (dbl), sulphates (dbl), alcohol (dbl), quality (int), c
# (dbl)

####################################

# combine red and white wine datasets
d <- rbind.data.frame(dred,dwhite)
d$c <- factor(d$c)
d[,1:11] <- data.frame(scale(d[,1:11]))

plot(d$chlorides, d$total.sulfur.dioxide,
     col=d$c)

# subset data for prettier visualization
d <- d[d$chlorides < 1.8 & d$total.sulfur.dioxide < 2.8,]

####################################

# index variables most predictive of wine type
selvar <- c("total.sulfur.dioxide", "chlorides")
selvari <- which(names(dred) %in% selvar)

# cross-validation to find k
knnModel <- train(d$c ~ d$total.sulfur.dioxide + d$chlorides,
                  data = d, method = "knn",
                  tuneLength = 20,
                  trControl = trainControl(method = "cv"))
knnModel # cross-valiation suggests that optimal k = 9

####################################

# visualize decision boundary
kgrid <- c(3, 17, 50) # comparing 3 different values of k

# compile misclassifcation rates
miss <- NULL

for (k in kgrid)  {

  x <- d[,selvari]  # training data
  g <- d$c          # actual class

  # identify misclassifications
  knnr <- knn(d[,selvari], d[,selvari], d$c, k = k)
  d$misclass <- NA
  d$misclass[d$c != knnr] <- 1
  miss <- c(miss, sum(d$misclass, na.rm = T))

  # decision boundary indicated by background color
  px1 <- seq(-1.5, 2.0, by = 0.02)
  px2 <- seq(-2.0, 3.0, by = 0.02)
  gd <- expand.grid(x=px1, y=px2)

  # run knn on all background points to classify them
  mod <- knn(x, gd, g, k=k, prob=TRUE)
  prob <- attr(mod, "prob")
  prob <- ifelse(mod=="1", prob, 1-prob)
  prob <- matrix(prob, length(px1), length(px2))
  par(mar=rep(2,4))

  # save plot
  png(filename = paste("k = ", k, " results.png", sep=""),
      width = 600, height = 600)

  # plot contours
  contour(px1, px2, prob,
          levels=0.5, labels="",
          xlab="Chlorides", ylab="Sulfur Dioxide",
          main=paste(k,"- nearest neighbors"),
          axes = F)

  # actual points that are correctly classified
  points(x[is.na(d$misclass),],
         pch = 16,
         cex = 1,
         col=alpha(ifelse(g==1, "black", "red"), 0.5))

  # color background
  points(gd, pch=".",
         cex=1.2,
         col=ifelse(prob>0.5, "black", "red"))

  # color misclassifications
  points(d[d$misclass ==1,selvari],
         pch = 16,
         cex = 1,
         col=alpha(ifelse(g[d$misclass==1]==1, "black", "red"), 0.8))

  box()

  dev.off()
}

# accuracy rate
100 - miss/nrow(d)*100



####################################

# plot without predictions

# save plot
png(filename = paste("wine classification knn.png", sep=""),
    width = 600, height = 600)

# actual points that are correctly classified
# plot contours
plot(gd, col=NA,
     xlab="Chlorides", ylab="Sulfur Dioxide",
     axes = F)

points(x[is.na(d$misclass),],
       pch = 16,
       cex = 1,
       col=alpha(ifelse(g==1, "black", "red"), 0.5))

# color misclassifications
points(d[d$misclass ==1,selvari],
       pch = 16,
       cex = 1,
       col=alpha(ifelse(g[d$misclass==1]==1, "black", "red"), 0.8))


box()
dev.off()
