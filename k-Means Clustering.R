# read in data
d <- read.csv("movies.csv")

# sample data

# movie     openness      conscientiousness extraversion  agreeableness neuroticism
# Avatar  	-0.279828202  0.191167006       -0.718972805	0.335909756   -0.578141335
# Big Fish	1.63379515	  -0.805515676      -0.718972805	-0.301430683	-0.076482424
# Salt	    -0.279828202	1.187849689	      0.705544973	  -0.938771121	-0.578141335

#################################
# Reduce no. of dimensions for 2D visualization
#################################

# examine correlations between personality traits
cor(d[,-1])

# combine correlated traits
d$conExt <- d$conscientiousness + d$extraversion
d$neuOpe <- d$neuroticism + d$openness

dclus <- d[,c("conExt", "neuOpe")]
rownames(dclus) <- d$movie

#################################
# k-means clustering
#################################

# standardize variables
dclus <- scale(dclus)

# Determine number of clusters with a scree plot
set.seed(95)
wss <- (nrow(dclus) - 1) * sum(apply(dclus, 2, var))
for (i in 2:10) {
  clus <- kmeans(dclus, centers = i)
  wss[i] <- sum(clus$withinss)
}
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within Cluster Scatter")



nc <- 3 # number of clusters

# cluster solution
set.seed(95)
fit <- kmeans(dclus, nc)

# get cluster means
aggregate(dclus, by=list(fit$cluster), FUN = mean)

# append cluster assignment
dclusFit <- data.frame(fit$cluster, dclus)
dclusFit[order(dclusFit$fit.cluster),]


# color-code clusters
dclusFit$col[dclusFit$fit.cluster == 1] = "#F44268" # red
dclusFit$col[dclusFit$fit.cluster == 2] = "#4274F4" # blue
dclusFit$col[dclusFit$fit.cluster == 3] = "#cc8814" # brown


# visualize clusters
plot(dclusFit$neuOpe, dclusFit$conExt,
     type = 'n',
     xlim = c(-2, 2.5),
     ylim = c(-2, 2))
text(dclusFit$neuOpe, dclusFit$conExt,
     rownames(dclusFit),
     cex = 0.8,
     col = dclusFit$col)



