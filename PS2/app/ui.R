shinyUI(pageWithSidebar( # template for the page
  
    headerPanel('Loan Data'), #title
    
    # user choosen input
    sidebarPanel(
    
      numericInput("muXA", label = h3("muX of Accepted"),4), # input names need to be unique - they're references
      numericInput("muYA", label = h3("muY of Accepted"),150),
      numericInput("sdXA", label = h3("sdX of Accepted"),1,min=0),
      numericInput("sdYA", label = h3("sdY of Accepted"),20,min=0),
      numericInput("muXD", label = h3("muX of Denied"),10),
      numericInput("muYD", label = h3("muY of Denied"),100),
      numericInput("sdXD", label = h3("sdX of Denied"),2,min=0),
      numericInput("sdYD", label = h3("sdY of Denied"),30,min=0)
    
    ),
  
    #whenever the user chooses the input, server.R calculates the outcome and 
    #shows it in main panel 
    mainPanel(
        tabsetPanel( # set up tabs, one for the plot and one for the conf matrix
            tabPanel("Plot",plotOutput("plot1",height="auto")),
            tabPanel("Confusion Matrix",tableOutput("confmatrix"))
        )
    )
))