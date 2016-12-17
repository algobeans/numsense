library(scales) # plot transparent data points
library(Hmisc) # correlation significance


d <- read.csv("foods.csv")

# sample data

# food        Vitamin.C Energy  Fiber Protein Fat     Vitamin.A
# Asparagus   5.6       20      2.1   2.20    0.12    38
# Beef        0.0       198     0.0   19.42   12.73   0
# Bluefish    0.0       124     0.0   20.04   4.24    120

##############################

# remove food with missing data
d <- na.omit(d[order(d$food),])

# run PCA
dpca <- d[,-c(1,2,4,8)]
dpca <- prcomp(dpca, scale = T)

# screeplot
plot(dpca$sdev*100/sum(dpca$sdev), type = "l", axes = F,
     xlab = "No. of Components",
     ylab = "% of Data Spread Accounted for")
axis(side=1, at=c(1:4))
axis(side=2, at=seq(0, 50, by=10))

# plot food
plot(predict(dpca)[,1:2], type='n')
text(predict(dpca)[,1:2], labels=d$food, cex=0.6)

##############################

# correlations
dcor <- d[,-c(1,2,4,8)]
rcorr(as.matrix(dcor), type="pearson") # p values

##############################

# attach PC values
d <- cbind.data.frame(d[,1:2], scale(d[,-c(1:2)]))
d <- cbind.data.frame(d, predict(dpca)[,1:2])
d$vitCfiber <- d$Vitamin.C..total.ascorbic.acid.mg.Per.100.g + d$Fiber..total.dietary.g.Per.100.g
