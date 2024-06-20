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
library("checkmate")
library("plyr")
library("mice")
library("tidyverse")

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
      menuSubItem(tabName = "data_eda", text = "Exploratory Data Analysis"),
      menuSubItem(tabName = "data_sampling", text = "Data Sampling")
    ),
    menuItem(
      "Machine Learning Models",
      tabName = "ml_models",
      icon = icon("fas fa-gear"),
      menuSubItem(tabName = "base_models", text = "Base Models"),
      menuSubItem(tabName = "tree_models", text = "Tree models")
      
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(tabName = "titlepage", uiOutput("titleUI")),
    tabItem(tabName = "data_import", uiOutput("dataimportUI")),
    tabItem(tabName = "data_summary", uiOutput("datasummaryUI")),
    tabItem(tabName = "data_eda", uiOutput("dataedaUI")),
    tabItem(tabName = "data_sampling", uiOutput("datasampleUI"))
    
  ))
)


#### Shiny Configuration ####

server = shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 1024 * 1024 ^ 2)
  options(show.error.messages = TRUE)
  options = list(lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
                 pageLength = 5)
  
  #### Render UI Code ####
  
  output$titleUI <- renderUI({
    fluidPage(
      tags$head(tags$style(
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
      fluidRow(
        style = 'padding-left:170px;',
        imageOutput(
          outputId = "image1",
          height = "1000px",
          width = "1000px"
        )
      ),
      fluidRow(htmlOutput(outputId = "welcome_text"))
    )
  })
  
  output$dataimportUI <- renderUI({
    fluidPage(
      h3("Data Import"),
      h4("Please Select the files"),
      fluidRow(
        radioButtons(
          inputId = "datatype1",
          label = "Select a import method",
          choices = c("File import", "Sample Files"),
          selected = "File import",
          inline = T
        )
      ),
      conditionalPanel(condition = "input.datatype1 == 'File import'",
                       
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
      conditionalPanel(condition = "input.datatype1 == 'Sample Files'",
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
                         )
                       )))
      
    )
    
  })
  
  output$datasummaryUI <- renderUI({
    fluidPage(
      fluidRow(h3("Data Summary"),
               dataTableOutput("data_df")),
      fluidRow(
        h3("Please verfiy the column types before proceeding"),
        dataTableOutput("data_column_structure")
        
      ),
      fluidRow(
        radioButtons(
          inputId = "type_change_1",
          label = "Would to like to change the column type",
          choices = c("Yes", "No"),
          selected = "No",
          inline = TRUE
        )
      ),
      conditionalPanel(condition = "input.type_change_1 == 'Yes'",
                       fluidRow(
                         column(
                           width = 4,
                           selectInput(
                             inputId = "selected_cols1",
                             label = "Please Select the columns",
                             choices = c(names(data_df1$data)),
                             selected = " ",
                             multiple = TRUE,
                             selectize = TRUE
                           )
                         ),
                         column(
                           width = 4,
                           selectInput(
                             inputId = "selected_conversion",
                             label = "Please Select the conversion type",
                             choices = c(
                               "factor" = "as.factor",
                               "numeric" = "as.numeric",
                               "character" = "as.character"
                             ),
                             selected = " ",
                             multiple = TRUE,
                             selectize = TRUE
                           )
                         ),
                         column(
                           style = 'padding-top:26px;',
                           width = 4,
                           actionButton("convert_button", label = "Convert")
                           
                         )
                         
                       )
                       # fluidRow(verbatimTextOutput("import_text_conversion")))
      ))
  })
    
    output$dataedaUI <- renderUI({
      fluidPage(
        h2("Data Summary"),
        fluidRow(dataTableOutput("datastructure")),
        fluidRow(h3("Missing Values"),
                 plotlyOutput("missing_value_plot")),
        fluidRow(
          h2("Distribution Plots"),
          radioButtons(
            inputId = "plot_type",
            label = "Please select the type of plot",
            choices = c("Bar", "Pie"),
            selected = "Bar",
            inline = TRUE
          ),
          h3("Plots"),
          selectInput(
            inputId = "selected_col_1",
            label = "Please select a column",
            choices = c(" ", req_cols()[[2]]),
            selected = " ",
            multiple = FALSE,
            selectize = TRUE
          ),
          plotlyOutput("dist_bar_plot") %>% withSpinner(type = 8)
        ),
        fluidRow(
          h3("Histogram"),
          selectInput(
            inputId = "selected_col_2",
            label = "Please select a column",
            choices = c(" ", req_cols()[[1]]),
            selected = " ",
            multiple = FALSE,
            selectize = TRUE
          ),
          plotlyOutput("histogram_plot") %>% withSpinner(type = 8)
        ),
        fluidRow(
          h3("Box Plots"),
          selectInput(
            inputId = "selected_col_3",
            label = "Please select a column",
            choices = c(" ", req_cols()[[1]]),
            selected = " ",
            multiple = TRUE,
            selectize = TRUE
          ),
          plotlyOutput("box_plots_1") %>% withSpinner(type = 8)
        )
      )
      
      
      
    })
    
    output$datasampleUI <- renderUI({
      fluidPage(
        fluidRow(h3("Data Summary1"),
                 dataTableOutput("data_df_dup")),
        fluidRow(
          h4("Column Fitering"),
          selectInput(
            inputId = "filtered_cols",
            label = "Select the columns to delete from data",
            choices = names(data_df1$data),
            selected = NULL,
            multiple = T,
            selectize = T
          ),
          actionButton(inputId = "filter_button", "Proceed")
          
        ),
        fluidRow(
          h4("Missing value imputations"),
          selectInput(
            inputId = "missing_imputate",
            label = "Select columns you want to impute",
            choices = names(data_df1$data),
            selected = NULL,
            multiple = T,
            selectize = T
          ),
          selectInput(
            inputId = "impute_method",
            label = "Select imputation method",
            choices = c("delete", "Mean", "Median", "cart", "rf", "linearreg"),
            selected = NULL,
            multiple = F,
            selectize = T
          )
        ),
        fluidRow(actionButton(
          inputId = "imp_button", label = "Impute"
        )),
        fluidRow(
          verbatimTextOutput("imputation_message")
        )
      )
      
    })
    
    
    
    #### server code ####
    
    #### Welcome Image ####
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
    
    #### Data import code ####
    
    df_fileimport <- eventReactive(input$loadBtn_1, {
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait uploading files...")
      
      tryCatch({
        data1 <- read.csv(input$file1$datapath, na.strings = "")
        msg1  <- "File imported sucessfully"
        remove_modal_spinner()
        return(list(data1, msg1))
      },
      error = function(e) {
        data1 <- NULL
        msg1 <- "File import Failed"
        remove_modal_spinner()
        return(list(data1, msg1))
      })
      
    })
    
    df_sampledata <- eventReactive(input$loadBtn_2, {
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait uploading files...")
      
      
      tryCatch({
        dpath <- paste0("./data/", input$samplefile1)
        data1 <- read.csv(dpath, na.strings = "")
        msg1  <- "File imported sucessfully"
        remove_modal_spinner()
        return(list(data1, msg1))
      },
      error = function(e) {
        data1 <- NULL
        msg1 <- "File import Failed"
        remove_modal_spinner()
        return(list(data1, msg1))
      })
      
    })
    
    
    output$import_text_fileimport <-
      renderText({
        df_fileimport()[[2]]
      })
    output$import_text_sampledata <-
      renderText({
        df_sampledata()[[2]]
      })
    
    #### Data import View ####
    
    data_df1 <- reactiveValues(data = NULL)
    
    observeEvent(input$loadBtn_1, {
      data_df1$data <- df_fileimport()[[1]]
    })
    
    observeEvent(input$loadBtn_2, {
      data_df1$data <- df_sampledata()[[1]]
    })
    
    observeEvent(input$convert_button, {
      data_df1$data <- column_converison()
    })
    
    observeEvent(input$filter_button, {
      data_df1$data <- column_filter()
    })
    
    observeEvent(input$imp_button, {
      data_df1$data <- missing_value_imputation()
    })
    
    data_df2 <- reactive({
      if (input$datatype1 == "File import") {
        df <- df_fileimport()[[1]]
      } else{
        df <- df_sampledata()[[1]]
      }
    })
    
    output$data_df <-
      renderDataTable(datatable(
        data_df1$data,
        selection = "none",
        options = list(pageLength = 5, scrollX = TRUE)
      ))
    
    output$data_df_dup <-
      renderDataTable(datatable(
        data_df1$data,
        selection = "none",
        options = list(pageLength = 5, scrollX = TRUE)
      ))
    
    
    #### Data summary ####
    
    df_str1 <- reactive({
      df <- data_df1$data
      num_cols <-
        names(df[, !sapply(df, function(x) {
          is.character(x) || is.factor(x)
        })])
      # chr_cols<-names(df[,sapply(df, function(x){is.factor(x)})])
      
      new_df <- NULL
      for (i in num_cols) {
        summary_stats <- summary(df[[i]])
        summary_df <- data.frame(Statistic = names(summary_stats),
                                 i = as.numeric(summary_stats))
        reshaped_df <- data.frame(t(summary_df[-1]))
        colnames(reshaped_df) <- summary_df$Statistic
        rownames(reshaped_df) <- i
        new_df <- rbind.fill(new_df, reshaped_df)
        
      }
      num_df <- new_df
      rownames(num_df) <- num_cols
      return(num_df)
    })
    
    output$datastructure = renderDataTable(datatable(df_str1()))
    
    #### Data frame column validation ####
    
    data_structure1 <- reactive({
      df <- data_df1$data
      df_structure <- capture.output(str(df))
      new_df_1 <- NULL
      for (i in df_structure[2:length(df_structure)]) {
        input_string = i
        x  = as.data.frame(extract_variable_and_type(input_string))
        new_df_1 = rbind(new_df_1, x)
      }
      new_df_1
    })
    
    output$data_column_structure <-
      renderDataTable({
        datatable(data_structure1())
      })
    
    #### Data Column Correction ####
    
    column_converison <- eventReactive(input$convert_button, {
      df <- data_df1$data
      df1 <- df
      selected_columns <- input$selected_cols1
      conversion_type  <- input$selected_conversion
      
      df[selected_columns] <-
        lapply(df[selected_columns], conversion_type)
      df
      
    })
    
    #### Missing Values Plot ####
    
    missing_values_df <- reactive({
      df <- data_df1$data
      missing_values = sapply(df, function(x) {
        sum(is.na(x))
      })
      missing_values = missing_values[missing_values > 0]
      library(plotly)
      plt <-
        plot_ly(x = names(missing_values),
                y = missing_values,
                type = "bar")
      plt
    })
    output$missing_value_plot <- renderPlotly({
      missing_values_df()
    })
    
    
    #### Distribution PLOTS ####
    
    req_cols <- reactive({
      df <- data_df1$data
      num_cols <-
        names(df[, !sapply(df, function(x) {
          is.character(x) || is.factor(x)
        })])
      chr_cols <-
        names(df[, sapply(df, function(x) {
          is.character(x) || is.factor(x)
        })])
      
      return(list(num_cols, chr_cols))
    })
    
    #### Bar plot ####
    dbar_plot <- reactive({
      df <- data_df1$data
      val1 = df[[input$selected_col_1]]
      x_val = names(table(val1))
      y_val = table(val1)
      
      if (input$plot_type == "Bar") {
        print("plotting")
        plt1 <- plot_ly(
          x = x_val,
          y = y_val,
          type = "bar",
          color = x_val
        )
        plt1
      } else if (input$plot_type == "Pie") {
        plt1 <- plot_ly(labels = x_val,
                        values = y_val,
                        type = 'pie')
        plt1
      }
      
    })
    output$dist_bar_plot <- renderPlotly({
      dbar_plot()
    })
    
    #### histogram ####
    
    histo_plot <- reactive({
      df <- data_df1$data
      val1 = df[[input$selected_col_2]]
      x_val = names(table(val1))
      y_val = table(val1)
      
      plt1 <- plot_ly(x = val1, type = "histogram")
      plt1
    })
    
    output$histogram_plot <- renderPlotly({
      histo_plot()
    })
    
    #### box Plots ####
    
    box_plot <- reactive({
      df <- data_df1$data
      val1 <- input$selected_col_3
      columns <- c(val1)
      fig <- plot_ly()
      for (col_name in columns) {
        fig <- fig %>%
          add_trace(
            y = df[[col_name]],
            type = "box",
            name = col_name,
            boxpoints = "outliers",
            jitter = 0
            # pointpos = -1.8
          )
      }
      fig
      
    })
    
    output$box_plots_1 <- renderPlotly({
      box_plot()
    })
    
    #### data Filtering ####
    
    column_filter <- eventReactive(input$filter_button, {
      df <- data_df1$data
      df <- df %>% select(-one_of(input$filtered_cols))
      df
    })
    
    #### Missing Value imputation ####
    
    missing_value_imputation <- eventReactive(input$imp_button,
                                              {
                                                df <-  data_df1$data
                                                selected_cols <- input$missing_imputate
                                                impute_method <- input$impute_method
                                                if (impute_method == "delete") {
                                                  df <- df %>% drop_na(c(selected_cols))
                                                } else if (impute_method == "Mean") {
                                                  df2 <- df %>% select(selected_cols)
                                                  m1 <- mice(df2, m = 1, method = "pmm")
                                                  df2 <- complete(m1)
                                                  df[c(selected_cols)] <- df2
                                                }  else if (impute_method == "median") {
                                                  df2 <- df %>% select(selected_cols)
                                                  med_values <- sapply(df2, median, na.rm = TRUE)
                                                  for (i in names(med_values)) {
                                                    df2[is.na(df2[i]), ] <- med_values[i]
                                                    df[c(selected_cols)] <- df2
                                                  }
                                                  df
                                                } else if (impute_method == "linearreg") {
                                                  df2 <- df %>% select(selected_cols)
                                                  m1 <- mice(df2, m = 1, method = "norm")
                                                  df2 <- complete(m1)
                                                  df[c(selected_cols)] <- df2
                                                  
                                                } else {
                                                  df2 <- df %>% select(selected_cols)
                                                  m1 <- mice(df2, m = 1, method = input$impute_method)
                                                  df2 <- complete(m1)
                                                  df[c(selected_cols)] <- df2
                                                  
                                                }
                                                df
                                                
                                              })
    imp_msg <- eventReactive(input$imp_button,{
      txt <- "Missing values imputation finished"
      txt
    })
    output$imputation_message <- renderText({
      txt <- imp_msg()
      txt
    })
    
    #### static function move to other file after finish build ####
    
    
    extract_variable_and_type <- function(input_string) {
      # Extract variable name and data type using regular expressions
      variable_name <-
        stringr::str_extract(input_string, "(?<=\\$\\s)[^:]+")
      variable_name <- trimws(variable_name)
      data_type <-
        stringr::str_extract(input_string, "(:\\s+[[:alpha:]]+)")
      data_type <- gsub(pattern = ":\\s+",
                        replacement = "",
                        x = data_type)
      
      return(list(variable_name = variable_name, data_type = data_type))
    }
    
    
})
  # Run the application
  shinyApp(ui = ui, server = server)
  