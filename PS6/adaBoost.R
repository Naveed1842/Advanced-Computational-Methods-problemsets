# adaBoost function
# Inputs: formula, data (data frame), depth (int), no. of trees to grow (int), 
# and test data if applicable (data frame)
# Returns: predicted labels of data points (vector)
adaBoost <- function(formula, data, depth, noTrees, test=NULL) {
    
    # install and load required packages
    if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
    if (!require("rpart")) install.packages("rpart"); library(rpart)
    if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
    
    #check inputs 
    not_empty(data); not_empty(formula)
    assert_that(class(formula)=="formula") # possible problem
    assert_that(is.data.frame(data))
    is.count(depth); is.count(noTrees)
    
    # split out features
    ModelFrame  <- model.frame(formula,data)
    Terms       <- attr(ModelFrame, "terms")
    features    <- attr(Terms, "term.labels")
    X_data      <- subset(ModelFrame, select = features)
    
    # split out dependent variable
    Y.names     <- get.vars(lhs(formula))
    Y_data      <- factor(data[,Y.names])
    
    # convert dependent variable to a factor
    data[,Y.names] <- factor(data[,Y.names])
    
    # initialise the observation weights and an empty matrix and vector for G 
    # and alpha
    N       <- nrow(data)
    w       <- rep(1/N, N)
    G       <- as.data.frame(matrix(NA, N, noTrees))
    alpha   <- rep(NA, noTrees) 
    trees   <- list()
    
    for (m in 1:noTrees) { 
        cat('.')
        # train weak classifier Gm(x) with training data using initial weights
        trees[[m]] <- rpart(formula, 
                            data, 
                            weights = w, 
                            control = rpart.control(maxdepth=depth))
        G[,m] <- predict(trees[[m]], newdata = X_data, type = "class")
    
        # compute error of the classifier
        err <- sum( w*(Y_data != G[,m]) ) / sum(w) 
    
        # calculate alpha (classifier weight)
        alpha[m] <- log((1-err)/err)
        
        # update observation weights
        w <- w * exp( alpha[m] * (Y_data != G[,m]) )
    }
    
    if (is.null(test)) { 
        # calculate final labels - training data
        G_final <- apply( G, 1, function(x) sign(alpha %*% as.numeric(x)) )
    } else {
        # predicting for test data
        predictions <- lapply(trees, function(x) predict(x, newdata = test, type = "class") )
        G_test <- data.frame( matrix(unlist(predictions), nrow=nrow(test)) )
        
        # calculate final labels - test data
        G_final <- apply( G, 1, function(x) sign(alpha %*% as.numeric(x)) )
    }
    
    return( list(predLabels = G_final) )
}