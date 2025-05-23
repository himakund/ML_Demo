# UI Libraries
library("shinydashboard")
library("shinybusy")
library("DT")
library("shinycssloaders")
library("shinytoastr")
library("shinyjs")
library("shinyWidgets")
library("shinyvalidate")
library("readxl") # For Excel file support

# Data Manipulation and Visualization packages
library("tidyverse") # includes ggplot2, dplyr
library("plotly")
library("plyr")

# Machine Learning and Statistics
library("caret")
library("ModelMetrics")
library("mice")
library("checkmate")
library("rpart")
library("rpart.plot")

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
    tabItem(tabName = "data_sampling", uiOutput("datasampleUI")),
    tabItem(tabName = "base_models", uiOutput("basemodelUI")),
    tabItem(tabName = "tree_models", uiOutput("treemodelUI"))
    
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
                         
                       ))
    )
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
      fluidRow(actionButton(inputId = "imp_button", label = "Impute")),
      fluidRow(verbatimTextOutput("imputation_message"))
    )
    
  })
  
  output$basemodelUI <- renderUI({
    fluidPage(h2("Baseline models"),
              fluidRow(
                h3("baseline model"),
                radioButtons(
                  inputId = "model1",
                  label = "Please select a model",
                  choiceNames = c("Linear Regression", "Logistic regression"),
                  choiceValues = c("linreg", "logreg"),
                  selected = NULL,
                  inline = TRUE
                )
              ), fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "target_var",
                    label = "Select the target variable",
                    choices = names(data_df1$data),
                    multiple = F,
                    selectize = F
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    inputId = "independent_var",
                    label = "Select the independent variable",
                    choices = c(),
                    selected = NULL,
                    multiple = T,
                    selectize = T
                  )
                ),
                column(
                  width = 3,
                  sliderInput(
                    inputId = "tr_split_per",
                    label = "Select the train split percentage",
                    min = 0,
                    max = 1,
                    step = 0.1,
                    value = 0.8,
                    animate = T
                  )
                )
              ), fluidRow(actionButton(inputId = "run_bmodel", label = "Run model")), fluidRow(box(
                title = "Model Summary",
                width = "500px",
                collapsible = T,
                verbatimTextOutput("lin_reg_op")
              )), fluidRow(checkboxInput(
                inputId = "run_pred",
                label = "Run Predictions",
                value = FALSE
              )), conditionalPanel(
                condition = "input.run_pred == 1",
                fluidRow(
                  radioButtons(
                    inputId = "test_preds",
                    label = "Select an option",
                    choiceNames = c("Run on test data from original df",
                                    "upload manual test data"),
                    choiceValues = c("sp_test", "mn_test"),
                    selected = NULL,
                    inline = TRUE
                  )
                ),
                conditionalPanel(
                  condition = "input.test_preds == 'mn_test'",
                  fluidRow(
                  fileInput(
                    inputId = "test_file1",
                    multiple = TRUE,
                    label = strong('Please Select a data File'),
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      ".xls",
                      ".xlsx"
                    )
                  )
                )
              ),
                fluidRow(verbatimTextOutput("lin_reg_pr_op"))
              )
    )
  })

  output$treemodelUI <- renderUI({
    fluidPage(
      h2("Decision Tree Model"),
      fluidRow(
        column(
          width = 3,
          selectInput(
            inputId = "tree_target",
            label = "Select target variable",
            choices = names(data_df1$data),
            multiple = FALSE,
            selectize = FALSE
          )
        ),
        column(
          width = 3,
          selectInput(
            inputId = "tree_features",
            label = "Select features",
            choices = c(),
            selected = NULL,
            multiple = TRUE,
            selectize = TRUE
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "tree_depth",
            label = "Maximum tree depth",
            value = 3,
            min = 1,
            max = 30
          )
        ),
        column(
          width = 3,
          sliderInput(
            inputId = "tree_split",
            label = "Train/Test split ratio",
            min = 0.5,
            max = 0.9,
            value = 0.7,
            step = 0.1
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          actionButton("train_tree", "Train Decision Tree")
        )
      ),
      fluidRow(
        box(
          title = "Tree Visualization",
          width = 12,
          plotOutput("tree_plot")
        )
      ),
      fluidRow(
        box(
          title = "Model Performance",
          width = 12,
          verbatimTextOutput("tree_metrics")
        )
      )
    )
  })

  # Update feature choices when target is selected
  observeEvent(input$tree_target, {
    updateSelectInput(
      session,
      "tree_features",
      choices = names(data_df1$data)[names(data_df1$data) != input$tree_target]
    )
  })

  # Train decision tree model
  tree_model <- eventReactive(input$train_tree, {
    req(input$tree_target, input$tree_features, input$tree_depth)
    
    # Prepare data
    data <- data_df1$data[c(input$tree_target, input$tree_features)]
    set.seed(123)
    train_idx <- createDataPartition(data[[input$tree_target]], p = input$tree_split, list = FALSE)
    train_data <- data[train_idx,]
    test_data <- data[-train_idx,]
    
    # Train model
    formula <- as.formula(paste(input$tree_target, "~."))
    tree <- rpart(formula, data = train_data, method = "class", maxdepth = input$tree_depth)
    
    # Make predictions
    predictions <- predict(tree, test_data, type = "class")
    
    # Calculate metrics
    metrics <- list(
      accuracy = mean(predictions == test_data[[input$tree_target]]),
      confusion_matrix = table(predictions, test_data[[input$tree_target]])
    )
    
    list(model = tree, metrics = metrics)
  })

  # Render tree plot 
  output$tree_plot <- renderPlot({
    req(tree_model())
    rpart.plot(tree_model()$model,nn = TRUE)
  })

  # Display model metrics
  output$tree_metrics <- renderPrint({
    req(tree_model())
    cat("Model Performance Metrics:\n")
    cat("\nAccuracy:", round(tree_model()$metrics$accuracy, 4))
    cat("\n\nConfusion Matrix:\n")
    print(tree_model()$metrics$confusion_matrix)
  })
    
    
    
    #### server code ####
    
    #### Welcome Image ####
  
    iv <- InputValidator$new()
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
      req(input$file1)
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait uploading files...")
      
      tryCatch({
        validate(need(input$file1$datapath, "Please select a file"))
        validate(need(tools::file_ext(input$file1$name) %in% c("csv", "xls", "xlsx"), "Invalid file format. Please upload CSV or Excel files."))
        
        data1 <- if(tools::file_ext(input$file1$name) == "csv") {
          read.csv(input$file1$datapath, na.strings = c("", "NA", "N/A"), stringsAsFactors = TRUE)
        } else {
          readxl::read_excel(input$file1$datapath) %>% as.data.frame()
        }
        
        validate(need(ncol(data1) > 0, "Empty file detected"))
        validate(need(nrow(data1) > 0, "No data rows found in file"))
        
        msg1  <- "File imported successfully"
        remove_modal_spinner()
        return(list(data1, msg1))
      },
      error = function(e) {
        remove_modal_spinner()
        data1 <- NULL
        msg1 <- paste("File import failed:", e$message)
        return(list(data1, msg1))
      })
    })
    
    df_sampledata <- eventReactive(input$loadBtn_2, {
      req(input$samplefile1)
      show_modal_spinner(spin = "atom",
                         color = "firebrick",
                         text = "Please wait uploading files...")
      
      tryCatch({
        dpath <- file.path("data", input$samplefile1)
        validate(need(file.exists(dpath), "Sample file not found"))
        data1 <- read.csv(dpath, na.strings = "", stringsAsFactors = TRUE)
        validate(need(ncol(data1) > 0, "Empty file detected"))
        msg1  <- "File imported successfully"
        remove_modal_spinner()
        return(list(data1, msg1))
      },
      error = function(e) {
        remove_modal_spinner()
        data1 <- NULL
        msg1 <- paste("File import failed:", e$message)
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
    
    observeEvent(input$target_var,{
      otpt <-
        names(data_df1$data[!names(data_df1$data) == input$target_var])
      updateSelectInput(
        session,
        inputId = "independent_var",
        label = "Select the independent variable",
        choices = otpt,
        selected = NULL)
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
    
    iv$add_rule("missing_imputate", function(value) {
      impute_method <- input$impute_method
      print(impute_method)
      if (length(value) < 2 && (impute_method != "delete" && impute_method != "Mean" && impute_method != "Median" )) {
        "Make sure to select atleast 2 columns when using cart,rf or linearreg"
      }
    })
    iv$enable()
    
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
                                                }  else if (impute_method == "Median") {
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
    
    #### Linear regression code ####
    
    lin_reg <- eventReactive(input$run_bmodel,{
      df <- data_df1$data
      set.seed(19)
      target_variable <- input$target_var
      ind_var <- input$independent_var
      tr_split_ratio <- as.numeric(input$tr_split_per)

      trainIndex <- createDataPartition(df[[target_variable]],
                                        p = tr_split_ratio,
                                        list = FALSE,
                                        times = 1)
      head(trainIndex)
      train <- df[trainIndex,]
      test <- df[-trainIndex,]
      
      cv_method = "repeatedcv"
      cv_number = 10
      cv_repeats = 10
      
      
      fitControl <- trainControl(
        method = cv_method,
        number = cv_number,
        repeats = cv_repeats)
      
      form1 <-
        as.formula(as.formula(paste(
          target_variable, "~", paste(ind_var, collapse = "+")
        )))
      if (input$model1 == "linreg"){
      set.seed(825)
      lmfit1 <- train(
        form1,
        data = train,
        method = "lm",
        trControl = fitControl,
        verbose = FALSE,
        metric = "Rsquared"
      )
      summary(lmfit1)
      
      return(list(lmfit1,test))
      
      } else if(input$model1 == "logreg"){
        set.seed(825)
        logisticFit1 <- train(form1, data = train,
                              method = "bayesglm", 
                              trControl = fitControl)
        
        pr<- predict(logisticFit1,test)
        # print(pr)
        # print(test[[target_variable]])
        CF1 <- caret::confusionMatrix(pr,test[[target_variable]])
        print(CF1)
      }
       

      
    })
    lin_reg_pred <- reactive({
      lmfit1 <- lin_reg()[[1]]
      test <- lin_reg()[[2]]
      pr <- predict(lmfit1, test)
      # pr
      rmse = rmse(test[[input$target_var]], pr)
      r2 = R2(pr,test[[input$target_var]])
      mape = 100 - MAPE(test[[input$target_var]], pr)
      met_df = list2DF(list("rmse" = rmse, "rsquared" = r2,"Mape" = mape))
      met_df

    })
    
    output$lin_reg_op <- renderPrint(summary(lin_reg()[[1]]))
    output$lin_reg_pr_op <- renderPrint(lin_reg_pred())
    
#### Decision Tree code ####
    
dec_tree <- eventReactive(input$run_bmodel,{
  df <- data_df1$data
  set.seed(19)
  target_variable <- input$target_var
  ind_var <- input$independent_var
  tr_split_ratio <- as.numeric(input$tr_split_per)
  
  trainIndex <- createDataPartition(df[[target_variable]], 
                                  p = tr_split_ratio,
                                  list = FALSE, 
                                  times = 1)
  
  train <- df[trainIndex,]
  test <- df[-trainIndex,]
  
  cv_method = "repeatedcv"
  cv_number = 10
  cv_repeats = 10
  
  fitControl <- trainControl(
    method = cv_method,
    number = cv_number,
    repeats = cv_repeats)
  
  form1 <- as.formula(paste(target_variable, "~", paste(ind_var, collapse = "+")))
  
  if(input$model1 == "dectree"){
    set.seed(825)
    # Train decision tree model
    dtree <- train(
      form1,
      data = train,
      method = "rpart",
      trControl = fitControl,
      tuneLength = 10
    )
    
    # If target is numeric, return regression metrics
    if(is.numeric(train[[target_variable]])){
      pred <- predict(dtree, test)
      rmse <- rmse(test[[target_variable]], pred)
      r2 <- R2(pred, test[[target_variable]])
      mape <- 100 - MAPE(test[[target_variable]], pred)
      metrics <- list("rmse" = rmse, "rsquared" = r2, "mape" = mape)
      
      return(list(dtree, test, metrics, "regression"))
    }
    # If target is categorical, return classification metrics  
    else {
      pred <- predict(dtree, test)
      conf_matrix <- confusionMatrix(pred, test[[target_variable]])
      return(list(dtree, test, conf_matrix, "classification"))
    }
  }
})

dec_tree_pred <- reactive({
  model_output <- dec_tree()
  dtree <- model_output[[1]]
  test <- model_output[[2]]
  metrics <- model_output[[3]]
  model_type <- model_output[[4]]
  
  if(model_type == "regression"){
    metrics_df <- list2DF(metrics)
    metrics_df
  } else {
    metrics
  }
})

output$dec_tree_op <- renderPrint(summary(dec_tree()[[1]]))
output$dec_tree_pr_op <- renderPrint(dec_tree_pred())
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
    #### Mape function ####
    MAPE<-function(actual,predicted){(mean(abs((actual-predicted)/actual)))*100}
    
    
  # Random Forest code
  rf_model <- eventReactive(input$run_bmodel, {
    req(input$model1 == "rf")
    
    df <- data_df1$data
    set.seed(19)
    target_variable <- input$target_var
    ind_var <- input$independent_var
    tr_split_ratio <- as.numeric(input$tr_split_per)
    
    trainIndex <- createDataPartition(df[[target_variable]], 
                                    p = tr_split_ratio,
                                    list = FALSE, 
                                    times = 1)
    
    train <- df[trainIndex,]
    test <- df[-trainIndex,]
    
    cv_method = "repeatedcv"
    cv_number = 5  # Reduced from 10 for faster processing
    cv_repeats = 3 # Reduced from 10 for faster processing
    
    fitControl <- trainControl(
      method = cv_method,
      number = cv_number,
      repeats = cv_repeats)
    
    form1 <- as.formula(paste(target_variable, "~", paste(ind_var, collapse = "+")))
    
    # Train random forest model
    set.seed(825)
    rf <- train(
      form1,
      data = train,
      method = "rf",
      trControl = fitControl,
      importance = TRUE
    )
    
    # Make predictions and calculate metrics based on target type
    if(is.numeric(train[[target_variable]])) {
      # Regression metrics
      pred <- predict(rf, test)
      rmse <- rmse(test[[target_variable]], pred)
      r2 <- R2(pred, test[[target_variable]])
      mape <- 100 - MAPE(test[[target_variable]], pred)
      metrics <- list("rmse" = rmse, "rsquared" = r2, "mape" = mape)
      
      return(list(rf, test, metrics, "regression", varImp(rf)))
    } else {
      # Classification metrics
      pred <- predict(rf, test)
      conf_matrix <- confusionMatrix(pred, test[[target_variable]])
      return(list(rf, test, conf_matrix, "classification", varImp(rf)))
    }
  })

  rf_pred <- reactive({
    req(rf_model())
    model_output <- rf_model()
    metrics <- model_output[[3]]
    model_type <- model_output[[4]]
    
    if(model_type == "regression"){
      metrics_df <- list2DF(metrics)
      metrics_df
    } else {
      metrics
    }
  })

  output$rf_model_op <- renderPrint({
    req(rf_model())
    summary(rf_model()[[1]])
  })
  
  output$rf_importance <- renderPlot({
    req(rf_model())
    plot(rf_model()[[5]])
  })
  
  output$rf_pred_op <- renderPrint({
    req(rf_pred())
    rf_pred()
  })
})


  # Run the application
  shinyApp(ui = ui, server = server)
  