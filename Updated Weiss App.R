library(shinydashboard)
library(googlesheets)
library(tidyverse)
library(plotly)

#Checks whether author is paul.weiss.survey@gmail.com, 
#then accesses the necessary datasheet.
if (isTRUE(check.Author("paul.weiss.survey")))
  temp <- extract.data("Test", "Sheet2")

ui <- dashboardPage(
  dashboardHeader(title = "Paul Weiss"),
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(
        menuItem("Univariate", tabName = "univar",
                 selectInput(inputId = "Variable", label = h4("Select Variable"), 
                             choices = list("HXSTAT" = 1, "HXCALC" = 2, "AGE" = 3, "HEIGHT" = 4,
                                            "WEIGHT" = 5, "SEX" = 6, "CONTINENT" = 7), selected = 1),
                 selectInput(inputId = "Graph", label = h4("Select Chart Type"), 
                             choices = list("Histogram" = 1, "Bar Plot" = 2, 
                                            "Pie Chart" = 3, "Boxplot" = 4), selected = 1)),
        tabName = "univar"
      ),
      convertMenuItem(
        menuItem("Multivariate", tabName = "multivar",
                 selectInput("Independent", label = h4("Select Independent Variable"), 
                             choices = list("HXSTAT" = 1, "HXCALC" = 2, "AGE" = 3, "HEIGHT" = 4,
                                            "WEIGHT" = 5, "SEX" = 6, "CONTINENT" = 7), selected = 1),
                 selectInput("Dependent", label = h4("Select Dependent Variable"), 
                             choices = list("HXSTAT" = 1, "HXCALC" = 2, "AGE" = 3, "HEIGHT" = 4,
                                            "WEIGHT" = 5, "SEX" = 6, "CONTINENT" = 7), selected = 2),
                 selectInput("Regression", label = h4("Select Regression Type"), 
                             choices = list("Linear" = 1, "Quadratic" = 2, "Hyperbolic" = 3,
                                            "Natural Log X" = 4, "Natural Log Y" = 5), selected = 1)),
        tabName = "multivar"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "univar", 
              fluidRow( #For ggplotly, use plotlyOutput instead of plotOutput.
                box(title = "Plot", plotOutput("Plot"), status = "primary", 
                    solidHeader = TRUE, width = 7),
                box(title = "Summary", tableOutput("Tabs"), status = "warning", 
                    solidHeader = TRUE, width = 4)
              )
      ),
      tabItem(tabName = "multivar", 
              fluidRow(
                box(title = "Scatterplot", plotOutput("Scatterplot"), status = "primary", 
                    solidHeader = TRUE, width = 12))
      )
    )
  )
)

server <- function (input,output) {
  
  get.Data <- reactive({
    data <- switch(input$Variable,
                   "1" = temp$HXSTAT,
                   "2" = temp$HXCALC,
                   "3" = temp$AGE,
                   "4" = temp$HEIGHT,
                   "5" = temp$WEIGHT,
                   "6" = temp$SEX,
                   "7" = temp$CONTINENT)
  })
  
  get.Chart.Type <- reactive({
    data <- switch(input$Graph,
                   "1" = "Histogram",
                   "2" = "Bar Plot",
                   "3" = "Pie Chart",
                   "4" = "Boxplot")
  }) 
  
  #Difficulty trying to adjust the x/y labels to change along with
  #the user input. 
  output$Plot <- renderPlot({
    create.Plot(temp, get.Data(), get.Chart.Type())
  })
  
  get.Dependent <- reactive({
    data <- switch(input$Dependent,
                   "1" = temp$HXSTAT,
                   "2" = temp$HXCALC,
                   "3" = temp$AGE,
                   "4" = temp$HEIGHT,
                   "5" = temp$WEIGHT,
                   "6" = temp$SEX,
                   "7" = temp$CONTINENT)
  })
  
  get.Independent <- reactive({
    data <- switch(input$Independent,
                   "1" = temp$HXSTAT,
                   "2" = temp$HXCALC,
                   "3" = temp$AGE,
                   "4" = temp$HEIGHT,
                   "5" = temp$WEIGHT,
                   "6" = temp$SEX,
                   "7" = temp$CONTINENT)
  })
  
  get.Regression.Type <- reactive({
    data <- switch(input$Regression,
                   "1" = "Linear",
                   "2" = "Quadratic",
                   "3" = "Hyperbolic",
                   "4" = "Natural Log X",
                   "5" = "Natural Log Y")
  })
  
  #The regressions are only present when both independent and dependent variables
  #are continuous. Difficulty changing x/y labels in accordance with user input.
  output$Scatterplot <- renderPlot({
    create.Scatterplot(temp, get.Independent(), get.Dependent(), get.Regression.Type())
  })
  
  output$Tabs <- renderTable({
    #Not sure how to change the column labels.
    if (is.character(get.Data()) == TRUE){
      tab <- count(temp, get.Data())
    }
    else
    {
      tab <- data.frame(Quantiles = c("Minimum", "First Quartile", 
                                      "Median", "Third Quartile", "Maximum", "Standard Deviation"))
      tab <- tab %>% 
        mutate(
          Values = c(fivenum(get.Data()), sd(get.Data())))
    }
  })
}

shinyApp(ui, server)
