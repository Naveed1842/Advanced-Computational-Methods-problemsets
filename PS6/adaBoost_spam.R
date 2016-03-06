# packages
library(gbm)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

# read in data from working directory
spambase <- read.csv("spambase.data", header=FALSE)

# split into training (60%) and testing (40%) data 
sampVec <- sample(1:nrow(spambase), size=0.6*nrow(spambase), replace=FALSE)
train   <- spambase[sampVec, ]
test    <- spambase[-sampVec, ]

# convert classes 0 & 1 to -1 & 1. 
train$V58 <- ifelse(train$V58==0,-1,1)
test$V58 <- ifelse(test$V58==0,-1,1)

# error evolution - adaBoost
noIterations <- 400
trainErr <- testErr <- rep(NA, noIterations)

for (i in 1:noIterations) {
    trainErr[i] <- mean( adaBoost(V58 ~ ., train, 5, i)$predLabels != train$V58 )
    testErr[i]  <- mean( adaBoost(V58 ~ ., train, 5, i, test)$predLabels != test$V58 )
}

# combining errors in a data frame
errors_adaBoost <- data.frame(Iterations = 1:noIterations,
                         Train = trainErr, 
                         Test = testErr) %>%
    melt(id.vars = "Iterations")

# plotting adaBoost results
plotadaBoost <- ggplot(data = errors_adaBoost, 
                       aes(x = Iterations, y = value, color = variable)) +
                geom_line() +
                scale_y_continuous( "Misclassification error", 
                                      limits = c(0,0.5),
                                      breaks = seq(0, 0.5, 0.1)) +
                ggtitle("Error Evolution - adaBoost") +
                theme_bw()

########################## gmb package results ###############################
# convert classes -1 & 1 to 0 & 1.
train$V58 <- ifelse(train$V58==-1,0,1)
test$V58 <- ifelse(test$V58==-1,0,1)

# build model using gbm package
boost <-gbm(formula = V58 ~ .,
            distribution = "adaboost",
            data = rbind(train, test),
            n.trees = noIterations,
            interaction.depth = 1,
            shrinkage = 1,
            bag.fraction = 1,
            train.fraction = (2/12))

(boostAcc <- mean((predict(boost, test) > 0) != test$V58))

# error evolution - gbm
trainErr_gbm <- testErr_gbm <- rep(NA, noIterations)
for (i in 1:noIterations) {
    trainErr_gbm[i] <- mean((predict(boost, train, n.trees = i) > 0) != train$V58)
    testErr_gbm[i] <- mean((predict(boost, test, n.trees = i) > 0) != test$V58)
}

# combining errors in a data frame
errors_gbm <- data.frame(Iterations = 1:noIterations,
                     Train = trainErr_gbm, 
                     Test = testErr_gbm) %>%
    melt(id.vars = "Iterations")

# plotting gbm results
plotgbm <- ggplot(data = errors_gbm, aes(x = Iterations, y = value, color = variable)) +
                geom_line() +
                scale_y_continuous( "Misclassification error", 
                                      limits = c(0,0.5),
                                      breaks = seq(0, 0.5, 0.1)) +
                ggtitle("Error Evolution - gbm package") +
                theme_bw()

# save both plots to pdf
pdf("adaBoost.pdf")
grid.arrange(plotadaBoost, plotgbm)
dev.off()
