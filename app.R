#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(dplyr)

raw <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

ref <- raw %>% group_by(CAMIS, INSPECTION.DATE) %>%
  na.omit() %>% summarise(S = first(SCORE), B = first(BORO), TYP = first(CUISINE.DESCRIPTION), Grade = first(GRADE), Name = first(DBA))

case <- ref %>% filter(Name %in% c("SUBWAY", "MCDONALD'S", "DOMINO'S")) %>%
  filter(Grade %in% c("A", "B", "C"))

subway <- case %>% filter(Name == "SUBWAY")
domino <- case %>% filter(Name == "DOMINO'S") %>% filter(CAMIS != "40746892") %>% filter(CAMIS != "50010587") %>% filter(CAMIS != "40866297") 
mc <- case %>% filter(Name == "MCDONALD'S") %>% filter(CAMIS != "440367790") %>% filter(CAMIS != "40401953") %>% filter(CAMIS != "50035245") %>% filter(CAMIS != "40367790")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("3 popular restaurant results"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "restaurant",
                  label = "Choose a restaurant:",
                  choices = c("SUBWAY", "DOMINO'S", "MCDONALD'S")),
      selectInput(inputId = "metric",
                  label = "Choose a metric:",
                  choices = c("Score", "Grade"))
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput1 <- reactive({
    switch(input$restaurant,
           "SUBWAY" = subway,
           "DOMINO'S" = domino,
           "MCDONALD'S" = mc)
  })
  
  datasetInput2 <- reactive({
    switch(input$metric,
           "Score" = 1,
           "Grade" = 2)
  })
  output$distPlot <- renderPlot({

    x = datasetInput1()
    y = datasetInput2()

    if(y == 1){
      ggplot(x, aes(S,fill=Grade)) +
        geom_bar() +
        xlab("Score")
    } else {
      ggplot(x, aes(Grade,fill=Grade)) +
        geom_bar()
    }
  })
  
}

shinyApp(ui = ui, server = server)

