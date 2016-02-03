############################ Data generating function ##########################
# ----
# Hrvoje's plain 2D XOR 
# ----

genXOR <- function(noObs=50, seed=1111, saveData=TRUE, savePlot=TRUE) {
    
    # load the required libraries
    library(assertthat)
    library(mvtnorm)
    library(ggplot2)
    
    # check the inputs
    assert_that(is.scalar(noObs) && is.double(noObs))
    assert_that(is.scalar(seed) && is.double(seed))
    assert_that(is.scalar(saveData) && is.logical(saveData))
    assert_that(is.scalar(savePlot) && is.logical(savePlot))
    
    # defining a function for generating bivariate normal data
    genBVN <- function(n = 1, muXY = c(0,1), sigmaXY = diag(2)) {
        rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
        return(rdraws)
    }
    
    # generate XOR data and add some simple names
    set.seed(seed)
    class1 <- rbind(genBVN(noObs, c(1,1), diag(2)),
                    genBVN(noObs, c(10,10), diag(2)) )
    class2 <- rbind(genBVN(noObs, c(1,10), diag(2)),
                    genBVN(noObs, c(10,1), diag(2)) )
    dataset <- rbind(cbind(class1, 0), cbind(class2, 1))
    dataset <- as.data.frame(dataset)
    colnames(dataset) <- c("x1", "x2", "y")
    
    return(dataset)
}

dataset <- genXOR(noObs=50)

############################ Find Nearest Neighbours ###########################
results <- kNN(dataset[,1:2], dataset[,3], k=3, p=2)
output <- cbind(dataset, predLabels = results$predLabels, prob = results$prob)

# write file
write.csv(output, file="predictions.csv", row.names = FALSE)


########################## Plot Decision Boundaries ############################
# create a grid covering all x1 and all x2 values - these define the graph boundaries
X1 <- seq(min(dataset$x1), max(dataset$x1), by=0.2)
X2 <- seq(min(dataset$x2), max(dataset$x2), by=0.2)
grid <- expand.grid(x1=X1, x2=X2)

# predict results for all points on grid
predGrid <- kNN(features=grid, labels=dataset[,3], memory=dataset[,1:2], k=3, p=2, 
                type="predict") 
predGridClasses <- predGrid$predLabels

# spread out predicted classes in the grid pattern 
probs <- matrix(predGridClasses, length(X1), length(X2))

# plot and save
ggplot( data=grid, aes(x=x1, y=x2, z=predGridClasses) ) + 
    stat_contour( bins=1, size=2) +
    geom_tile(aes(fill=as.factor(predGridClasses)), alpha=0.5) +
    geom_point(data=dataset, size=2, aes(x=x1, y=x2, z=y, colour=as.factor(dataset$y))) +
    scale_fill_manual("Class", values=c("blue", "red")) +
    scale_colour_manual("Training points", values=c("blue", "red")) +
    theme_bw() +
    theme(text = element_text(family = "Helvetica"))

ggsave("plot.pdf", scale = 1, width = 4, height = 4)
