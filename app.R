# Load Libraries
library(openxlsx)
library(Hmisc)
library(mosaic)
library(shiny)
library(shinydashboard)
library(markdown)

######################################################
### Start: Data preprocessing
######################################################

# Download file (only once)
# url <- "https://www.oecdregionalwellbeing.org/assets/downloads/OECD-Regional-Well-Being-Data-File.xlsx"
# download.file(url = url, destfile = "OECD.xlsx")

# Read data into R
oecd <-read.xlsx("OECD.xlsx", 
                 startRow = 9, colNames = FALSE,
                 sheet = 4, rows = 9:410, cols=2:17)


colnames(oecd)<-c("Country","Region","Code","Labour","Employment","Unemployment", "Household",
                  "Homicide","Mortality","Life","Air","Voter","Broadband","Number","Perceived",
                  "Self")

oecd_numvars <- c("Labour","Employment","Unemployment", "Household",
                  "Homicide","Mortality","Life","Air","Voter","Broadband","Number","Perceived",
                  "Self")

oecd_labels <- c("Country","Region","Code","Labour force with at least secondary education", 
                 "Employment rate","Unemployment rate", "Household Household disposable income per capita", 
                 "Homicide rate","Mortality rate","Life expectancy","Air pollution","Voter turnout",
                 "Broadbandaccess","Number of rooms per person","Perceived social network support", 
                 "Self assessment of life satisfaction")

# To convert some missing data in Excel sheet

setna <- function(x) replace(x, x=="..", NA)
oecd <- oecd %>%
  mutate_all(setna) %>%
  mutate_at(oecd_numvars, as.numeric)

Countries <- oecd %>%
  select(Country) %>%
  unique() %>%
  pull()

Variables <- oecd_numvars

# Number of permutations
nperm <- 100
######################################################
### End: Data preprocessing
######################################################

######################################################
### Start: ui
######################################################

ui <- dashboardPage(
  dashboardHeader(title = "OECD Permutation data"),
  dashboardSidebar(
    h5("Permutation test for OECD data"),
    h5("Please select two OECD countries"),
    br(""),
    selectInput("country1", "Please select the first country:",
                choices=as.list(Countries),selected="Germany"),
    selectInput("country2", "Please select the second country:",
                choices=as.list(Countries),selected="Poland"),
    selectInput("variable", "Please select the variable:",
                choices=as.list(oecd_numvars),selected="Self"),
    actionButton("go", "Go!")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plotObserved", height = 500), title="Observed Difference"),
      # box(imageOutput("plotAnimated", height = 450)),     
      box(plotOutput("plotDiffs", height = 205), title="Permuted Differences"),
      box(plotOutput("plotDist", height = 205), title="Permutation Distribution"),
      box(withMathJax(includeHTML("Background.html")), width = 12, title = "Background")
      )
    )
  )
  
######################################################
### Start: Server
######################################################

server <- function(input, output) {

data <- reactiveValues()


observeEvent(input$go,{ 
  #################################################
  # Selection of Countries and Variable of Interest
  my_countries <- (c(input$country1, input$country2))
  my_variable <- (input$variable)
  
  my_data <- oecd %>%
    select("Country", "Region", my_variable) %>%
    filter(Country %in% my_countries) %>%
    rename(Y = my_variable)
  
  # Observed Difference
  Obs_Diff <- diffmean(Y ~ Country, data = my_data)
  
  #################################################
  # Monte Carlo Permutation

  
  set.seed(1896)
  
  shuffled_data <- list()
  
  for(i in 1:nperm)
  {
    # Shuffle Country
    my_shuffle <- my_data %>%
      mutate(Shuffled_Country = shuffle(Country)) %>%
      mutate(Shuffle = i)
    # Add data frame
    shuffled_data <- shuffled_data %>%
      bind_rows(my_shuffle)
  }
  
  # Calculate difference in means
  Shuffled_Diffs <- shuffled_data %>%
    group_by(Shuffle) %>%
    summarise(Diff_Mean = diffmean(Y ~ Shuffled_Country)) %>%
    ungroup()
  
  
  isolate({
    data$my_data <- my_data
    data$Obs_Diff <- Obs_Diff
    data$shuffled_data <- shuffled_data
    data$Shuffled_Diffs <- Shuffled_Diffs
    data$my_countries <- my_countries
    data$my_variable <- my_variable
    })
  })


  
  
  ##############################
  # Scatterplot of Observed data
  output$plotObserved <- renderPlot({
    if (input$go){
    ggplot(data$my_data, aes(x = Country, y = Y, color = Country)) +
      geom_point() +
      stat_summary(fun.data = "mean_cl_boot", aes(colour = Country), size = 1.5, alpha = 0.5) +
      labs(title = paste("Observed difference in means:", data$Obs_Diff),
           y=data$my_variable)}
  })
  
  # #######################
  # # Permutation Animation
  # output$plotAnimated <- renderImage({
  # 
  #   outfile <- tempfile(fileext='.gif')
  #   if (input$go){
  #     if (input$animate){
  #       p <- ggplot(data$shuffled_data, aes(x = Shuffled_Country, y = Y, color = Country)) +
  #         geom_point() +
  #         stat_summary(fun.data = "mean_cl_boot", aes(colour = Shuffled_Country), size = 1.5, alpha = 0.5) +
  #         labs(title = "Shuffle: {closest_state}") +
  #         transition_states(Shuffle)
  #   anim_save("outfile.gif", animate(p))
  #     }
  #   }
  # 
  #   # Return a list containing the filename
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif'
  #        )
  #   })



  ######################
  # Permuted Differences
  output$plotDiffs <- renderPlot({
    if (input$go){
    ggplot(data$Shuffled_Diffs, aes(x = Shuffle, y = Diff_Mean, color = abs(Diff_Mean) >= abs(data$Obs_Diff))) +
      geom_point() +
      geom_hline(yintercept = abs(data$Obs_Diff)) +
      geom_hline(yintercept = -abs(data$Obs_Diff)) +
      labs(title = paste("Number of permutations with larger difference in means \n than observed:",
                         sum(abs(data$Shuffled_Diffs$Diff_Mean) >= abs(data$Obs_Diff)),
                         "\n for countries ", data$my_countries[1], "and ", data$my_countries[2]),
           subtitle = paste("Observed difference in means:", data$Obs_Diff),
           y = paste("Difference in means of Variable:\n ", data$my_variable)) +
      theme(legend.position = "none")}
  })

  ##########################
  # Permutation Distribution
  output$plotDist <- renderPlot({
    if (input$go){
    ggplot(data$Shuffled_Diffs, aes(x = Diff_Mean, fill = abs(Diff_Mean) >= abs(data$Obs_Diff))) +
      geom_histogram(center=0, bins=21) +
      geom_rug() +
      geom_vline(xintercept = data$Obs_Diff) +
      labs(title = paste("Number of permutations with larger difference in means \n than observed:",
                         sum(abs(data$Shuffled_Diffs$Diff_Mean) >= abs(data$Obs_Diff)), " of ", nperm,
                         "\n for countries ",  data$my_countries[1], "and ",  data$my_countries[2]),
           x = paste("Difference in means of Variable:\n ",  data$my_variable)) +
      theme(legend.position = "none")}
  })
  
  
}

shinyApp(ui, server)
