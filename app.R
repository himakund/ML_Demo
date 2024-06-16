library("shinydashboard")
library("shinybusy")
library("DT")
library("shinycssloaders")
library("plotly")
library("dplyr")
library("shinytoastr")
library("shinyjs")
library("dplyr")
library("ggplot2")
library("tidyr")

ui = dashboardPage(
  dashboardHeader(title = "ML Tools"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Landing Page",
      tabName = "titlepage",
      icon = icon("fas fa-file")
    ),
    menuItem(
      "Data Tools",
      tabName = "datatools",
      icon = icon("fas fa-chart-bar"),
      menuSubItem(tabName = "data_import", text = "Data Import"),
      menuSubItem(tabName = "data_summary", text = "Data Summary"),
      menuSubItem(tabName = "data_eda", text = "Exploratory Data Analysis")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "titlepage", uiOutput("titleUI")),
    tabItem(tabName = "data_import", uiOutput("dataimportUI")),
    tabItem(tabName = "data_summmary", uiOutput("datasummaryUI")),
    tabItem(tabName = "data_eda", uiOutput("dataedaUI"))
  ))
)


#### Shiny Configuration ####

server = shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 1024 * 1024 ^ 2)
  options(show.error.messages = TRUE)
  options = list(lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
                 pageLength = 5)
  
  ## Render UI Code ##
  
  output$titleUI <- renderUI({
    fluidPage(tags$head(tags$style(
      HTML(
        "
        h1 {
          font-family:  fantasy;
          font-size: 120px;
          color: #2C3E50;
          text-align: center;
        }
      "
      )
    )),
    h1("Welcome")
    )
  })
  
  
})

# Run the application
shinyApp(ui = ui, server = server)
