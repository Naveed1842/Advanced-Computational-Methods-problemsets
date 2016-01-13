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
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
    sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
    sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
    approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
    denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
    loanDf <- as.data.frame(rbind(approved,denied))
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
    target = c(rep(0, noApproved), rep(1, noDenied))
    loanDf <- data.frame(loanDf, deny, target)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
    return(loanDf)
}

# simulate data
noApproved <- 50; noDenied <- 50
loanDf <- loanData(noApproved, noDenied, c(7, 150), c(10, 100), 
                   c(2,20), c(2,30), -0.5, 0.3)

# add third (and second) categories
loanDf <- cbind(loanDf,
                target1 = c(rep(0, noApproved), rep(1, noDenied))
                target2 = c())