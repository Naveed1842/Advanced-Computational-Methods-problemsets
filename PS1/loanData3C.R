library(mvtnorm)

# create small wrapper functions
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

# create main function for 2D, bivariate data simulation
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
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), 
              rep("Undecided", noUndecided))
    
    # include one binary dependent variable for each category
    target_deny <-  c(rep(0, noApproved), rep(1, noDenied), rep(0, noUndecided))
    target_appr <- c(rep(1, noApproved), rep(0, noDenied), rep(0, noUndecided))
    target_undec <- c(rep(0, noApproved), rep(0, noDenied), rep(1, noUndecided))
    
    loanDf <- data.frame(loanDf, deny, target_deny, target_appr, target_undec)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target_deny", "target_approve", "target_undecided")
    return(loanDf)
}

# simulate data
noApproved <- 45; noDenied <- 45; noUndecided <- 10
loanDf <- loanData(noApproved, noDenied, noUndecided, 
                   c(7, 150), c(10, 100), c(8.5,125), 
                   c(2,20), c(2,30), c(1,5), 
                   -0.5, 0.3, -0.1)

# plot data - NEED TO ADD DECISION BOUNDARIES
ggplot(data = loanDf, aes(x = solvency, y = PIratio, 
                          colour = deny, fill = deny)) +
    geom_point() +
    xlab("solvency") + 
    ylab("PIratio") + 
    theme_bw() + 
    scale_color_manual("deny", 
                       values = c("Approved" = "blue", "Denied" = "red", 
                                  "Undecided" = "green"))
ggsave("discFunction3C.pdf", scale = 1, width = 4, height = 4)

# separate out X and Y
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c("target_deny", "target_approve", "target_undecided")])
# solve for beta
weightsOptim <- solve(t(X) %*% X) %*% t(X) %*% Y

# compute predictions
predictions <- X %*% weightsOptim

# classify according to argmax criterion 
category <- (predictions==apply(predictions, 1, max))
predictedLabels <- rep(NA, nrow(loanDf))
for (i in 1:(nrow(loanDf))) {
    if (category[i,"target_deny"]) predictedLabels[i] <- "Denied"
    else if (category[i,"target_approve"]) predictedLabels[i] <- "Approved"
    else predictedLabels[i] <- "Undecided"
}

loanDf <- cbind(loanDf, weightsOptim, predictedLabels)

# save dataset with predictions
write.csv(loanDf, file = "predictions.csv")

# confusion matrix - HOW DO I DO THIS WITH 3 CATEGORIES???

