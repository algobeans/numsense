library(calibrate)


######################################
# Plotting Function
######################################

graph <- function(x, xname, y, trendline = "Y",
                  trendEg ="N", trendx = NULL) {

  # plot points
  plot(x, y,

       # format plot points
       col = "black", pch = 16, cex = 0.7,

       # clean plot labels
       xlab = "",
       ylab = "")

  # axis labels
  title(xlab = xname, line=3, cex.lab=1.2)
  title(ylab = "Median House Prices in $1000's",
        line=3, cex.lab=1.2)


  # plot trendline
  if (trendline == "Y") {
    mod <- lm(y ~ x)
    abline(mod, col = "blue", lwd = 5)

    # plot example trend prediction
    if (trendEg == "Y") {

      # get default plot axis limits
      plotLim <- par('usr')

      # get predicted y
      predy <- round(coef(mod)[1] + coef(mod)[2]*(trendx),2)

      # vertical prediction line
      segments(trendx, 0,
               trendx, predy,
               lwd = 1,
               lty = "dotted")

      # horizontal prediction line
      segments(plotLim[1], predy,
               trendx, predy,
               lwd = 1,
               lty = "dotted")
      textxy(X = plotLim[1] + 0.05,
             Y = predy + 0.5,
             labs = predy,
             cex = 1) # label

    }
  }
}


######################################
# Read in and examine the data
######################################

# read in data
d <- read.csv("housePrice.csv")

# sample data
# crime     zone  indus river nox   room   age   dist   highway
# 10.0623    0    18.1  0     0.584 6.833  94.3 2.0882      24
# 10.6718    0    18.1  0     0.740 6.459  94.8 1.9879      24
# 11.1604    0    18.1  0     0.740 6.629  94.6 2.1247      24
# 12.0482    0    18.1  0     0.614 5.648  87.6 1.9512      24
# 12.2472    0    18.1  0     0.584 5.837  59.7 1.9976      24
# ..     ...  ...   ...   ...   ...   ...   ...    ...     ...
# Variables not shown: tax (int), ptratio (dbl), black (dbl),
# lstat (dbl), medv (dbl)


# summarize data
str(d)
summary(d)

# sort predictors by correlation coeff
sort(cor(d)[,which(names(d) == "medv")])



######################################
# Deriving Coefficients
######################################

# regression on top 2 strongest predictors
regModscale <- lm(scale(medv) ~ scale(room) + scale(log(lstat)),
             data = d)
summary(regModscale)
regMod <- lm(medv ~ room + log(lstat),
             data = d)
summary(regMod)
# calculate mean error
mean(abs(predict(regMod) - d$medv))


# regression on room
roomMod <- lm(medv ~ room, data = d)
summary(roomMod)
# calculate mean error
mean(abs(predict(roomMod) - d$medv))


# regression on lsat
lsatMod <- lm(medv ~ log(lstat), data = d)
summary(lsatMod)
# calculate mean error
mean(abs(predict(lsatMod) - d$medv))



# correlation coefficients
cor(d$medv, d$room)
cor(d$medv, d$lstat)
cor(d$medv, log(d$lstat))
cor(d$medv, predict(regMod))



######################################
# Generate and save plots
######################################

# plot strongest predictors
graph(x = d$room,
      xname = "Average No. of Rooms",
      y = d$medv,
      trendline = "Y",
      trendEg = "Y",
      trendx = 8)



graph(x = d$lstat,
      xname = "% of Population with Low SES",
      y = d$medv,
      trendline = "Y")



graph(x = log(d$lstat),
      xname = "% of Population with Low SES (Log)",
      y = d$medv,
      trendline = "Y")



# plot predicted regression values against observed values
graph(x = predict(regMod),
      xname = "Combined Predictors",
      y = d$medv,
      trendline = "Y")




