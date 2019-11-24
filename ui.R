




pageWithSidebar(
  headerPanel('Permutation tests'),
  sidebarPanel(
    h5("Permutation test for OECD data"),
    h5("Please select two OECD countries"),
    br(""),
   
    selectInput("Country1", "Please select the first country:",
                choices=as.list(Countries),selected="Germany"),
    selectInput("Country2", "Please select the second country:",
                choices=as.list(Countries),selected="Poland"),
    
    sliderInput("no_permutations", "Please give a number of permutations:", 
                min=100, max=150, value=100, step=10),
    selectInput("my_variable", "Please select the variable:",
                choices=as.list(oecd_numvars),selected="Self"),
br("")

  ),
  mainPanel(
    plotOutput('plotObserved'),
#    plotOutput('plotAnimated'),
    plotOutput('plotDiffs'),
    plotOutput('plotDist'),
 #   verbatimTextOutput('values')
#  )

fluidRow(
#  box(plotOutput("plotObserved", height = 400)),
#  box(imageOutput("plotAnimated", height = 400)),     
#  box(plotOutput("plotDiffs", height = 250)),
#  box(plotOutput("plotDist", height = 250))
)
)
)



