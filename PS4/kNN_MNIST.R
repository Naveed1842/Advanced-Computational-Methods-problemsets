# read in data
train <- read.csv("MNIST_training.csv", header=FALSE)
test <- read.csv("MNIST_test.csv", header=FALSE)


############################ Train using random samples ########################
# define samples for training and validating
set.seed(1234)
sampleRows <- train[sample(nrow(train), 6000), ]
train.train <- sampleRows[1:4200, ]
train.validation <- sampleRows[4201:6000, ]

# define features and labels for both sets of data
memory <- train.train[,2:257]
labels <- train.train[,1]
features <- train.validation[,2:257]
true.labels <- train.validation[,1]

# optimise for parameters k and p - WARNING! This takes a long time.
k <- c( 1,3,5,7,9,11 )
p <- c( 1,2,Inf )
accuracyMatrix <- matrix(NA, length(p), length(k))

for (i in 1:length(p)) {
    cat(".")
    distMatrix.prev <- NULL
    
    for (j in 1:length(k)) {
        cat(".")
        results <- kNN(features, labels, memory, k=k[j], p=p[i], type='predict', 
            distanceMatrix=distMatrix.prev)
        
        accuracyMatrix[i,j] <- mean(results$predLabels == true.labels)
        
        distMatrix.prev <- results$distMatrix
    }
}

indices <- which(accuracyMatrix == max(accuracyMatrix), arr.ind = TRUE)
paste("Highest accuracy occurs when p=", p[indices[1,1]], " and k=", 
      k[indices[1,2]], sep='')
# [1] "Highest accuracy occurs when p=2 and k=1"


################## Cross-Validation - as a double check (faster) ###############
if (!require("class")) install.packages("class")
# for training data define features and labels
features <- as.matrix(train[,2:257])
labels <- train[,1]

# optimise k using cross-validation
k <- c( 1,3,5,7,9,11 )
accuracy <- rep(NA, length(k))

for (i in 1:length(k)) {
    cat(".")
    predictedClasses <- knn.cv(features, labels, k = k[i], prob = TRUE)
    accuracy[i] <- mean(predictedClasses == train[,1])
}

# plot results
plot(k,accuracy, type='l')

# optimal k (with p=2)
k_opt <- which.max(accuracy) # [1] 1


######################### Predict Labels for Test Data #########################
k <- 1
p <- 2
    
resultsTest <- knn(train[,2:257], test, train[,1])

# write file
write.csv(resultsTest, file="MNIST_predictions.csv", row.names = FALSE)
