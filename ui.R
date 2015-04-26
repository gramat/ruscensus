## Coursera Data Science
## Developing Data Products Course Project:
## Shiny application
## ui.R
## 
library(shiny)

shinyUI(fluidPage(

  headerPanel("Russia Population Censuses", windowTitle = "Russia Censuses"),
    
  sidebarLayout(
    sidebarPanel(
      selectInput("census",
                  label = "Choose a census year",
                  choices = list("1897", "1926", "1939", "1959", "1970",
                                "1979", "1989", "2002", "2010"), selected = "1897"),
      sliderInput("breack", 
                  label = "Choose a cohort size",
                  min = 1, max = 25, value = 1),
      wellPanel(
        h4("Information:"),
        p("The application shows the distribution of the age groups in the population of Russia 
          across the census data since 1897 until 2010."),
        p("An user can select the desired census year and specify a cohorts (age groups) size 
          and the app draws the corresponding diagram."),
        p(strong("NB! "), "To ensure data comparability the application considers the population 
          that is/was dwelling the territory of contemporary Russian Federation within the borders of 2013."),
        p(strong("NB!"), "This application is for educational purposes only. Some or all the data may be approximate.")
        )
      ),
    mainPanel(
      wellPanel(
        h4(textOutput("caption")),
        br(h6(tableOutput("info")))
      ),
      wellPanel(
        plotOutput("main", height = "600px")
        )
    )
  )
))


