#install required packages
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("mvtnorm")) install.packages("mtvnorm"); library(mvtnorm)


shinyServer(function(input, output, session) {
  
    # First define required functions
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
    
    # calculate all variables
    data <- reactive({
        loanDf <- loanData(50,50, c(input$muXA,input$muYA), 
                           c(input$muXD, input$muYD), c(input$sdXA, input$sdYA), 
                           c(input$sdXD,input$sdYD), -0.1, 0.6, 1221) 
      
        datafit <- lm(target ~ solvency + PIratio + 1, data=loanDf)
      
        # calculating the boundaries 
        weights     <- coef(datafit)[c("solvency", "PIratio")]
        bias        <- coef(datafit)[1]
        intercept   <- (-bias + 0.5)/weights["PIratio"]
        slope       <- -(weights["solvency"]/weights["PIratio"])
        x           <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), length.out = nrow(loanDf))
        y           <- -( weights["PIratio"]/weights["solvency"] ) * x + (0.5-bias)/weights["solvency"]
        boundaryDf  <- data.frame(PIratio=x, solvency=y, deny=rep("Boundary", length(x)))
        
        # predicted labels
        predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
      
        # confusion matrix
        confMatrixFreq <- table(loanDf$deny, predictedLabels)
  
        return(list(loanDf,boundaryDf,confMatrixFreq))
        
    })
    
    # function for the plot - calling output of previous reactive "data"
    plotDiscFnc <- function() {
        ggplot(data = data()[[1]], 
               aes(x = solvency, y = PIratio, colour=deny)) + 
            geom_point() +
            xlab("solvency") +
            ylab("PI ratio") +
            geom_line(data=data()[[2]]) + 
            scale_color_manual("", values = c("Boundary" = "grey", 
                                              "Approved" = "blue", "Denied" = "red"))
    }
    
    # plot and confusion matrix
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plotDiscFnc()
    }, height = 500, width=750)
  
    output$confmatrix<-renderTable({
        data()[[3]]    
    })
    
})