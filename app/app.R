library(shiny)
library(ggplot2)
library(dplyr)
library(devtools)
library(shinythemes)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  #Feature 1: Adding a theme to the app page for aesthetic purpose.
  theme = shinytheme("sandstone"),
  #Feature 2: Adding an image to make it more appealing for the user
  titlePanel(title = div(img(width = "20%", src="bcliquor.png", align = "center"),"BC Liquor Store Prices")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
    ),
    mainPanel(
       tabsetPanel(
         #Feature 3: Seperating the table and plot into different panels so information is not too crowded for the user.
        tabPanel("Plot", plotOutput("alcohol_hist")),
        #Adding DT:: in order to make an interactive table for user's ease.
        tabPanel("Table", DT::dataTableOutput("data_table"))
       )
    )
  ),
  a(href = "https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Download data")
)


server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }

    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })

  output$alcohol_hist <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  #Feature 4: Creating an interactive table for user to explore data more efficiently.
  output$data_table <-
    DT::renderDataTable({
      filtered()
  })
}
#This code helps run the app.
shinyApp(ui = ui, server = server)
