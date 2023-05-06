library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(plotly)
library(usmap)

netflixData <- read.csv('netflix_titles.csv')
mapData <- read.csv('choropleth.csv')
mapData$Total[mapData$Total <= 10] <- 10
mapData$Total[mapData$Total >= 1000] <- 1000

minYear <- 2008
maxYear <- 2020

ui <- dashboardPage(
  dashboardHeader(title = "Stats 8426"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Domination", tabName = "Netflix", 
               icon = icon("dashboard"))
    )    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Netflix",
              fluidRow(
                width = 12,
                box(sliderInput("slider", "Countries with Netflix available by year",
                                sep = '',
                                min = minYear, 
                                max = maxYear,
                                value = minYear,
                                ticks = TRUE,
                                animate = TRUE), 
                    width='90%')),
              fluidRow(
                width = 12,
                plotlyOutput("netflix"),
                  height = '100%',
                  width = '90%'
              )
      )
    )
  )
)


server <- function(input, output) {
   #light grey boundaries
   l <- list(color = toRGB("grey"), width = 0.5)
  
   # specify map projection/options
   g <- list(
    showframe = TRUE,
    showcoastlines = FALSE,
    projection = list(type = 'mercator')
  )
   
  output$netflix <- renderPlotly({
    val <- input$slider
    
    chart <- mapData %>%
      mutate(available = ifelse(Introduced <= val, 1, 0))
    
    fig <- plot_geo(chart, width = 800, height = 700, hoverinfo = 'text') %>%
      add_trace(
        z = ~available, color = ~Introduced, colors = 'Blues',
        text = ~Country, locations = ~code, marker = list(line = l),
        showscale = FALSE
      ) %>%
      layout(
        geo = g,
        showlegend = FALSE,
        modebar = FALSE,
        title = list("foo")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)
