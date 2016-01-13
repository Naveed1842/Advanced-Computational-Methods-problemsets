library(ggplot2)
library(MASS)
# create function that generates data Chinese fluency and salary data for Chinese nationals and non-Chinese.
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


genData <- function(noCat1=100, noCat2=100, muCat1=c(5,5), sdCat1=c(1,1), rhoCat1=0.5, minCat2=c(0,0), 
                    maxCat2=c(10,10), rhoCat2=0.9, seed=1111, writeCSV = TRUE) {
    
    # generate category 1 data
    sigmaCat1 <- sigmaXY(rhoCat1, sdCat1[1], sdCat1[2])
    cat1 <- genBVN(noCat1, seed, muCat1, sigmaCat1)
    
    # generate category 2 data
    #cat2_X <- runif(noCat2, min=minCat2[1], max=maxCat2[1])
    #cat2_Z <- runif(noCat2, min=minCat2[2], max=maxCat2[2])
    #cat2_U <- rbinom(noCat2, 1, rhoCat2)
    #cat2_Y <- cat2_X * cat2_U + cat2_Z * (1 - cat2_U)
    #cat2 <- cbind(cat2_X, cat2_Y)
    
    # second method of generating cat 2 data
    S <- matrix(c(1, 0.9, 0.9, 1), nrow = 2)
    cat2_Z <- mvrnorm(noCat2, mu = c(5,5), Sigma = S)
    cat2_X <- cat2_Z[,1]
    cat2_Y <- pnorm(cat2_Z[,2])
    cat2 <- cbind(cat2_X, cat2_Y)
    
    # combine data and add binary outcome and labels
    Df <- as.data.frame(rbind(cat1, cat2))
    catLabel <- c(rep("category1", noCat1), rep("category2", noCat2))
    target <- c(rep(0, noCat1), rep(1, noCat2))
    Df <- data.frame(Df, catLabel, target)
    colnames(Df) <- c("X1", "X2", "catLabel", "target")
    
    #plot
    ggplot(data = Df, aes(x = X2, y = X1, colour = catLabel, fill = catLabel)) +
        geom_point() +
        xlab("X2") +
        ylab("X1") + 
        theme_bw() #+
        #theme(text=element_text(family="Arial"))
    ggsave("dataPlot.pdf", scale = 1, width = 4, height = 4)
    
    # write dataset to csv file
    if (writeCSV == TRUE ) {
        write.csv(Df, file = "dataset.csv")
    }
    return(Df)
}

genData(100, 100, muCat1=c(5,5), sdCat1=c(1, 1), rhoCat1=0.5, minCat2=c(0,0), 
        maxCat2=c(10,10), rhoCat2=0.9, seed=1111, writeCSV = TRUE)


