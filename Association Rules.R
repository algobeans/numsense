set.seed(88)

# load packages
require(arules)
require(arulesViz)
require(cluster)

# load data
data("Groceries")

################################################
# Find itemsets with minimum support
################################################

# get list of rules with 2 items
rules <- apriori(Groceries,
                 parameter = list(maxlen = 2,
                                  minlen = 2,
                                  supp = 0.0005,
                                  conf = 0.009))

# get lift values for chosen items
chosenitems <- c("soda", "other vegetables",
                 "tropical fruit", "whole milk",
                 "male cosmetics", "sausage",
                 "yogurt", "canned beer")

subrules <- subset(rules,
                    subset = lhs %in% chosenitems)

# subset rules to those with high support/lift
subrules <- c(head(sort(subrules, by=c("lift", "support")), 30))

inspect(subrules)

# plot rules
plot(subrules, method="graph")
plot(subrules, method="grouped")



########################################
# Find clusters of transaction patterns
########################################

# load sample of dataset
s <- sample(Groceries, 2000)

# get dissimilarity matrix for clustering
d <- dissimilarity(s, method = "Jaccard")

# perform PAM clustering with 8 clusters
clus <- pam(d, k = 8)
plot(clus)

# predict labels for the rest of the dataset
clusPred <- predict(s[clus$medoids], Groceries, method = "Jaccard")
clusters <- split(Groceries, clusPred)

# get clusters of purchases, support threshold = 0.05
itemFrequencyPlot(clusters[[8]],
                  population = s,
                  support = 0.05,
                  ylim=c(0,0.7),
                  ylab="Support")

itemFrequencyPlot(clusters[[3]],
                  population = s,
                  support = 0.05,
                  ylim=c(0,0.7),
                  ylab="Support")

