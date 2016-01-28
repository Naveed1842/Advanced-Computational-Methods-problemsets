# load necessary packages
library(mvtnorm)
library(ggplot2)

# create small wrapper functions (from lecture notes)
sigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                       2, 2, byrow = TRUE)
    return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
}

# function generates 2D data for categories: denied, approved, undecided
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, 
                     muUndecided, sdApproved, sdDenied, sdUndecided, 
                     rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
    sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], 
                             sdY=sdApproved[2])
    sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
    sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], 
                              sdY=sdUndecided[2])
    approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
    denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
    undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
    loanDf <- as.data.frame(rbind(approved, denied, undecided))
    
    # create labels for each category
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), 
              rep("Undecided", noUndecided))
    
    # include one binary dependent variable for each category
    target_deny <-  c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
    target_appr <- c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
    target_undec <- c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
    
    # 
    loanDf <- data.frame(loanDf, deny, target_deny, target_appr, target_undec)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target_deny", 
                          "target_approve", "target_undecided")
    return(loanDf)
}

# call function
noApproved <- 50; noDenied <- 50; noUndecided <- 50
loanDf <- loanData(noApproved, noDenied, noUndecided, 
                   c(7, 150), c(10, 100), c(13,250), 
                   c(2,20), c(2,30), c(1,15), 
                   -0.5, 0.3, 0.5)

# separate out X and Y and solve for the optimal weights
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c("target_deny", "target_approve", "target_undecided")])
weights <- solve(t(X) %*% X) %*% t(X) %*% Y

# decision boundaries: 1=deny, 2=approve, 3=undecided
x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), 
         length.out = nrow(loanDf))

y_12 <- -(weights[1,2]-weights[1,1])/(weights[3,2]-weights[3,1]) - 
    (weights[2,2] - weights[2,1])/(weights[3,2]-weights[3,1]) * x
y_23 <- -(weights[1,3]-weights[1,2])/(weights[3,3]-weights[3,2]) - 
    (weights[2,3] - weights[2,2])/(weights[3,3]-weights[3,2]) * x
y_31 <- -(weights[1,1]-weights[1,3])/(weights[3,1]-weights[3,3]) - 
    (weights[2,1] - weights[2,3])/(weights[3,1]-weights[3,3]) * x

boundaryDf_12 <- data.frame(PIratio = x, solvency = y_12, 
                            deny = rep("Boundary_12", length(x)))
boundaryDf_23 <- data.frame(PIratio = x, solvency = y_23, 
                            deny = rep("Boundary_23", length(x)))
boundaryDf_31 <- data.frame(PIratio = x, solvency = y_31, 
                            deny = rep("Boundary_31", length(x)))

# truncate lines


# plot data
ggplot(data = loanDf, aes(x = solvency, y = PIratio, 
                          colour = deny, fill = deny)) +
    geom_point() +
    xlab("solvency") + 
    ylab("PIratio") + 
    theme_bw() + 
    geom_line(data=boundaryDf_12) +
    geom_line(data=boundaryDf_23) +
    geom_line(data=boundaryDf_31) +
    scale_color_manual("deny", 
                       values = c("Approved" = "blue", "Denied" = "red", 
                                  "Undecided" = "green", "Boundary_12" = "grey",
                                  "Boundary_23" = "orange", 
                                  "Boundary_31" = "black"))
ggsave("discFunction3C.pdf", scale = 1, width = 6, height = 4)

# compute predictions
predictions <- X %*% weights

# classify training data according to argmax criterion 
category <- (predictions==apply(predictions, 1, max))
predictedLabels <- rep(NA, nrow(loanDf))
for (i in 1:(nrow(loanDf))) {
    if (category[i,"target_deny"]) predictedLabels[i] <- "Denied"
    else if (category[i,"target_approve"]) predictedLabels[i] <- "Approved"
    else predictedLabels[i] <- "Undecided"
}

# combine predictions and final class decision to data frame and return
loanDf <- cbind(loanDf, predictions, predictedLabels)
loanDf

# save dataset with predictions
write.csv(loanDf, file = "predictions.csv")


