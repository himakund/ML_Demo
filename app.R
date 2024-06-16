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

#### UI code ####

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
    h1("Welcome"),
    fluidRow(style = 'padding-left:170px;',
    imageOutput(outputId = "image1",height = "1000px",width = "1000px")),
    fluidRow(htmlOutput(outputId = "welcome_text"))
    )
  })
  
  output$dataimportUI <- renderUI({
    
    fluidPage(
      h3("Data Import"),
      h4("Please Select the files"),
      fluidRow(radioButtons(inputId = "datatype1",
                            label = "Select a import method",
                            choices = c("File import","Sample Files"),
                            selected = "File import",inline = T)),
      conditionalPanel(
        condition = "input.datatype1 == 'File import'",
        
        fluidRow(column(
          width = 7,
          box(
            title = "File Import",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fileInput(
              inputId = "file1",
              multiple = TRUE,
              label = strong('Please Select a data File'),
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv",
                ".xls",
                ".xlsx"
              )
            ),
            actionButton("loadBtn_1", "Load data"),
            verbatimTextOutput("import_text_fileimport")
            
          )
        ))),
      conditionalPanel(
        condition = "input.datatype1 == 'Sample Files'",
        fluidRow(column(
          width = 7,
          box(
            title = "File Import",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              inputId = "samplefile1",
              label = "Please select a sample data file",
              choices = list.files("./data"),
              selectize = F
            ),
            actionButton("loadBtn_2", "Load data"),
            verbatimTextOutput("import_text_sampledata")
            )))),
      fluidRow(
        h3("Data Summary"),
        dataTableOutput("data_df"))
      )

  })
  
  
  
  ##### server code #####
  
 # Data import code #
  
  df_fileimport <- eventReactive(input$loadBtn_1,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait uploading files...")
   
    tryCatch({
      data1 <- read.csv(input$file1$datapath)
      msg1  <- "File imported sucessfully"
      remove_modal_spinner()
      return(list(data1,msg1))
      },
      error = function(e){
      data1 <- NULL
      msg1 <- "File import Failed"
      remove_modal_spinner()
      return(list(data1,msg1))
    })

  })
  
  df_sampledata <- eventReactive(input$loadBtn_2,{
    show_modal_spinner(spin = "atom",
                       color = "firebrick",
                       text = "Please wait uploading files...")


    tryCatch({
      dpath <- paste0("./data/",input$samplefile1)
      data1 <- read.csv(dpath)
      msg1  <- "File imported sucessfully"
      remove_modal_spinner()
      return(list(data1,msg1))
    },
    error = function(e){
      data1 <- NULL
      msg1 <- "File import Failed"
      remove_modal_spinner()
      return(list(data1,msg1))
    })

  })
  
  ##########
  
  data_df1 <- reactive({
    if (input$datatype1 == "File import") {
      df <- df_fileimport()[[1]]
    } else{
      df <- df_sampledata()[[1]]
    }
  })
  
  output$data_df <-
    renderDataTable(datatable(
      data_df1(),
      selection = "none",
      options = list(pageLength = 5, scrollX = TRUE)
    ))
  
  output$image1 <- renderImage({
    width <- "100%"
    height <- "100%"
    list(
      src = "WWW/MLpic.jpg",
      contentType = "image/jpg",
      width = width,
      height = "auto"
    )
  }, deleteFile = FALSE)
  

output$welcome_text <- renderText({
  intro_message()
})

output$import_text_fileimport <- renderText({df_fileimport()[[2]]})
output$import_text_sampledata <- renderText({df_sampledata()[[2]]})



intro_message <- reactive({
  msg <-
    c(
      '<p 
      style="font-size: x-large;
      color: black;
      font-family: serif;
      align-content: center;
      margin : 20px 0px 0px 90px;">
      This tool is meant to illustrate few examples of machine learining process and methods <br></p>'
    )
  msg
})

})
# Run the application
shinyApp(ui = ui, server = server)
