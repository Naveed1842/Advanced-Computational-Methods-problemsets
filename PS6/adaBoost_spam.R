# read in data from working directory
spambase <- read.csv("spambase.data", header=FALSE)

# split into training (60%) and testing (40%) data 
sampVec <- sample(1:nrow(spambase), size=0.6*nrow(spambase), replace=FALSE)
train   <- spambase[sampVec, ]
test    <- spambase[-sampVec, ]

# convert classes 0 & 1 to -1 & 1. 
train$V58 <- ifelse(train$V58==0,-1,1)
test$V58 <- ifelse(test$V58==0,-1,1)

# training error evolution
noIterations <- 400
trainErr <- testErr <- rep(NA, noIterations)

for (i in 1:noIterations) {
    trainErr[i] <- mean( adaBoost(V58 ~ ., train, 5, i)$predLabels != train$V58 )
    testErr[i]  <- mean( adaBoost(V58 ~ ., train, 5, i, test)$predLabels != test$V58 )
}

pdf("adaBoost.pdf")
plot(x=noIterations, y=trainErr, type='n', 
     main="Evolution of Errors - adaBoost",
     xlab="Number of trees",
     ylab="Misclassification error")
lines(x=noIterations, y=trainErr, type='l', col='red')
lines(x=noIterations, y=trainErr, type='l', col='blue')
legend(noIterations*0.9, 0.9, c("Training error", "Test error"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"))
dev.off()






# testing
formula <- V58 ~ .
data <- train
depth <- 5
noTrees <- 100
myPredTrain <- adaBoost(V58 ~ ., train, depth=5, noTrees=100) 
myPredTest  <- adaBoost(V58 ~ ., train, depth=5, noTrees=100, test) 