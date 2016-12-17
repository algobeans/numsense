# load decision tree package
library(rpart)
library(tree)

# read in data
d <- read.csv("Titanic.csv")

# sample data

# Class   Age  Sex Survived
# First Adult Male      Yes
# First Adult Male      Yes
# First Adult Male      Yes

############################

# run decision tree
minsplit = 20
fit <- rpart(Survived ~., data = d, method = "class",
              minsplit = minsplit, minbucket = round(minsplit/3))
fit

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE, margin=0.2)
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# nicer plot of tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

