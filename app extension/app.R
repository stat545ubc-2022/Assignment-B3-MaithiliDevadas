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
  titlePanel(title = div(img(width = "40%", src="bcliquor.png", align = "right"),"BC Liquor Store Prices")),
  #Feature 5: Add app introduction and description
  h4("Welcome to the BC Liquor Store App"),
  h5("With the help of this app we hope you find the drink of your choice from a variety of alcohol made across the world!"),
  h6("Apply your choice of filters to find your ideal drink :)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
     
#Feature 8: Allowing users to select more than one type of alcohol       
      checkboxGroupInput("typeInput", "Type", 
                         choices = c("BEER", "REFRESHMENT", 
                                     "SPIRITS", "WINE"), 
                         selected = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),

      #Feature 7: Enabling user to download the data
      downloadButton("downloadTable", "Download Table"),
      uiOutput("countryOutput"),
    ),
    mainPanel(
       tabsetPanel(
         #Feature 3: Seperating the table and plot into different panels so information is not too crowded for the user.
        tabPanel("Plot", plotOutput("alcohol_hist")),
        #Adding DT:: in order to make an interactive table for user's ease.
        tabPanel("Table", DT::dataTableOutput("data_table")),
        #Feature 6: Enabling user know the number of results based on the filters they selected.
        h5(textOutput("numberofresults")),
       
        
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
    
  #Feature 9 : Adding color to histogram based on type of product ie. beer, refreshment, spirits or wine. 
    ggplot(filtered(), aes(Alcohol_Content, fill =Type)) +
      geom_histogram(colour = "black") + theme_bw()
  })
  
  #Feature 6: Letting the user know we found x results based on their filters.
  output$numberofresults <- renderText({
    paste("You have ", nrow(filtered()), "choices based on your filters")
  })
  
  #Feature 4: Creating an interactive table for user to explore data more efficiently.
  output$data_table <-
    DT::renderDataTable({
      filtered()
  })
  
  #Feature 7: Enable user to download the CSV file
  output$downloadTable <-
    downloadHandler(
      filename = function(){
        "bcl-data.csv"
      },
      content = function(file){
        write.csv(bcl, file) 
      }
    )
}

#This code helps run the app.
shinyApp(ui = ui, server = server)
