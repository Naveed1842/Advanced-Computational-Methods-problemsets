# defint kNN function
# if type = "train", features = x-vars, labels = y-vars, memory = NULL
# if type = "predict", features = new x-vars, labels = y-var (training), memory = x-var (training)
kNN <- function(features, labels, memory=NULL, k = 1, p = 2, type = 'train', 
                distanceMatrix=NULL) {
    
    # test the inputs
    library(plyr)
    library(assertthat)
    not_empty(features) 
    not_empty(labels)
    if (type == "train") {
    assert_that(nrow(features) == length(labels))
    }
    is.string(type)
    assert_that(type %in% c("train", "predict"))
    is.count(k)
    assert_that(p %in% c(1, 2, Inf))
    if (type == "predict") {
        assert_that(not_empty(memory) & 
                        ncol(memory) == ncol(features) & 
                        nrow(memory) == length(labels))
    }
    if (!(is.data.frame(features)))     features <- as.data.frame(features)
    
    # define a couple of variables
    noObs <- nrow(features)
    noVars <- ncol(features)
    
    # Compute distances
    if (is.null(distanceMatrix)) {
    if (type == "train") {
        distMatrix <- matrix(NA, noObs, noObs)
        diag(distMatrix) <- 0
        for (obs in 1:noObs) {
        
            # getting the probe for the current observation (from features)
            probe <- as.numeric(features[obs,])
            probeExpanded <- matrix(probe, nrow = noObs, ncol = noVars, byrow = TRUE) 
        
            # computing distances between the probe and exemplars in the training X
            ### Only calculate half of matrix. Copy the other half (since symmetric for training data)
            if (p %in% c(1,2)) {
                if (obs>1) {
                    distMatrix[obs, 1:(obs-1) ] <- distMatrix[1:(obs-1),obs]
                }
                if (obs<noObs) {
                    distMatrix[obs, (obs+1):noObs ] <- (rowSums((abs(features[(obs+1):noObs, ] - 
                                                                probeExpanded[(obs+1):noObs, ]))^p) )^(1/p)
                }
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
            }  
        }
    } else if (type == "predict") {
        noMemory <- nrow(memory) 
        distMatrix <- matrix(NA, noObs, noMemory)
        for (obs in 1:noObs) {
            
            # getting the probe for the current observation
            probe <- as.numeric(features[obs,])
            probeExpanded <- matrix(probe, nrow = noMemory, ncol = noVars, byrow = TRUE)
            
            # computing distances between the probe and exemplars in the memory (distMatrix not symmetric)
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(memory - probeExpanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(memory - probeExpanded), 1, max)
            }  
        }
    }
    } else {
        distMatrix <- distanceMatrix
    }
    # Sort the distances in increasing numerical order and pick the first 
    # neighbours for point 1 are in COLUMN 1
    neighbors <- apply(distMatrix, 1, order) 
    
    # Compute and return the most frequent class in the k nearest neighbors 
    # - ties broken randomly
    predictedClasses <- rep(NA, noObs)
    prob <- rep(NA, noObs)
    for (obs in 1:noObs) {
        classCounts <- plyr::count(labels[neighbors[1:k, obs]])
        possiblePredClasses <- classCounts[classCounts$freq == max(classCounts$freq),1]
        predictedClasses[obs] <- possiblePredClasses[sample(length(possiblePredClasses),1)]    
        prob[obs] <- max(classCounts$freq)/k
    }
    
    # examine the performance
    if (type == "train") {
        errorCount <- table(predictedClasses, labels)
        accuracy <- mean(predictedClasses == labels)
    } else if (type == "predict") {
            errorCount <- NA
            accuracy <- NA
    }
    
    # return the results
    return(list(predLabels = predictedClasses, 
                prob = prob,
                accuracy = accuracy,
                errorCount = errorCount, 
                distMatrix = distMatrix))
}





