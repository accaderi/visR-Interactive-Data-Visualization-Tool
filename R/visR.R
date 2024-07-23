# File: R/app.R

#' @import shiny
#' @import plotly
#' @import dplyr
#' @import readr
#' @import DT
#' @import caret
#' @import randomForest
#' @import bslib

library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(DT)
library(caret)
library(randomForest)
library(bslib)

#' Interactive Data Visualization Tool
#'
#' @param csv Optional. Path to a CSV file to load data from.
#' @param chart Optional. Comma-separated list of chart types to include.
#'
#' @return A Shiny app object
#' @export
#'
#' @import shiny plotly dplyr readr DT caret randomForest bslib
#'
#' @examples
#' \dontrun{
#' visR()
#' visR(csv = "path/to/your/data.csv")
#' visR(chart = "scatter,line,bar")
#' }
visR <- function(csv = NULL, chart = NULL) {
# Define UI
  ui <- fluidPage(
    theme = bs_theme(bootswatch = "sandstone"),
    uiOutput("navbar")
  )

  # Define server logic
  server <- function(input, output, session) {
    
    data <- reactiveVal(NULL)
        
    # Load data function
    data_load <- function(csv, session) {
      tryCatch({
        df <- read_csv(csv)
        updateSelectInput(session, "scatter_x", choices = names(df))
        updateSelectInput(session, "scatter_y", choices = names(df))
        updateSelectInput(session, "scatter_color", choices = c("None" = "", names(df)))
        updateSelectInput(session, "scatter_size", choices = c("None" = "", names(df)))
        updateSelectInput(session, "line_x", choices = names(df))
        updateSelectInput(session, "line_y", choices = names(df))
        updateSelectInput(session, "bar_x", choices = names(df))
        updateSelectInput(session, "bar_y", choices = names(df))
        updateSelectInput(session, "pie_var", choices = names(df))
        updateSelectInput(session, "hist_x", choices = names(df))
        updateSelectInput(session, "violin_x", choices = names(df))
        updateSelectInput(session, "violin_y", choices = names(df))
        updateSelectInput(session, "box_x", choices = names(df))
        updateSelectInput(session, "box_y", choices = names(df))
        updateSelectInput(session, "heat_x", choices = names(df))
        updateSelectInput(session, "heat_y", choices = names(df))
        updateSelectInput(session, "heat_z", choices = names(df))
        updateSelectInput(session, "target_variable", choices = names(df))
        updateSelectInput(session, "predictor_variables", choices = names(df))
        data(df)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        NULL  # Return NULL in case of error
      })
    }
    
    output$navbar <- renderUI({
      # Define the tabs
      tabs <- list(
        about = tabPanel(
          "About", value = "about",
          tags$div(
            tags$h1("visR: Interactive Data Visualization Tool"),
            tags$p("visR is a powerful and flexible R Shiny package designed for interactive data visualization and basic AI model analysis. It provides a user-friendly interface for exploring and visualizing data through various chart types and running simple machine learning models."),
            
            tags$h2("Features"),
            tags$ul(
              tags$li("Data Loading: Upload CSV files or use pre-loaded datasets."),
              tags$li("Data Exploration: View and modify data types of columns."),
              tags$li("Visualization Types:"),
              tags$ul(
                tags$li("Scatter Plots"),
                tags$li("Line Plots"),
                tags$li("Bar Charts"),
                tags$li("Donut Charts"),
                tags$li("Histograms"),
                tags$li("Violin Plots"),
                tags$li("Box Plots"),
                tags$li("Heatmaps")
              ),
              tags$li("AI Analysis: Run basic machine learning models:"),
              tags$ul(
                tags$li("Binary Classification"),
                tags$li("Multi-Class Classification"),
                tags$li("Linear Regression")
              ),
              tags$li("Interactive Plots: All visualizations are interactive, allowing for zooming, panning, and hovering for more information."),
              tags$li("Customization: Adjust plot titles, axis labels, and other parameters.")
            ),
            
            tags$h2("Usage"),
            tags$p("To use visR, you can either run it with default settings or customize it with parameters:"),
            tags$pre("
# Load the visR package
library(visR)

# Run with default settings
visR()

# Run with a specific CSV file
visR(csv = \"path/to/your/data.csv\")

# Run with specific chart types
visR(chart = \"scatter,line,bar\")

# Run with both CSV and chart types
visR(csv = \"path/to/your/data.csv\", chart = \"scatter,line,bar\")
      "),
            
            tags$h2("Installation"),
            tags$p("To install and run visR, you need R and the visR package."),
            tags$p("After installation, you can load the visR library and call the visR function as shown in the usage examples."),
            
            tags$div(
              tags$h2("Note:"),
              tags$div(
                style = "background-color: #fef9e5; padding: 10px; border: 1px solid #ddd;",
                "This 'About' page is a high-level overview of the visR application. For more detailed information about each feature and how to use them, please refer to the README.md file."
              )
            ),
            br(),
            tags$h2("Contributing"),
            tags$p("Contributions to visR are welcome! Please feel free to submit pull requests, create issues or spread the word."),
            
            tags$h2("License"),
            tags$p("This project is open source and available under the MIT License."),
            
            tags$p("Created with ❤️ by the visR team. ", tags$a(href = "https://github.com/accaderi/visR-Interactive-Data-Visualization-Tool", "GitHub"))
          )
          ),
        
        data = tabPanel(if (is.null(csv)) "Load Data" else "Data", uiOutput("data_tab_content")
        ),
        
        scatter = tabPanel("Scatter Plot", value = "scatter",
                           fluidRow(
                             column(4,
                                    div(class = "card",
                                        div(class = "card-header", "Scatter Plot Options"),
                                        div(class = "card-body",
                                            selectInput("scatter_x", "X-axis", ""),
                                            selectInput("scatter_y", "Y-axis", ""),
                                            selectInput("scatter_color", "Color (optional)", c("None" = ""), multiple = FALSE),
                                            selectInput("scatter_size", "Size (optional)", c("None" = ""), multiple = FALSE),
                                            textInput("scatter_title", "Plot Title", value = "Scatter Plot"),
                                            tags$script("$('#scatter_title').attr('maxlength', 60);"),
                                            actionButton("draw_scatter", "Draw Plot")
                                        )
                                    )
                             ),
                             column(8,
                                    div(class = "card",
                                        div(class = "card-header", "Scatter Plot"),
                                        div(class = "card-body",
                                            plotlyOutput("scatter_plot", height = "800px")
                                        )
                                    )
                             )
                           )
        ),
        
        line = tabPanel("Line Plot", value = "line",
                        fluidRow(
                          column(4,
                                 div(class = "card",
                                     div(class = "card-header", "Line Plot Options"),
                                     div(class = "card-body",
                                         selectInput("line_x", "X-axis", choices = NULL),
                                         selectInput("line_y", "Y-axis", choices = NULL),
                                         textInput("line_title", "Plot Title", value = "Line Plot"),
                                         tags$script("$('#line_title').attr('maxlength', 60);"),
                                         actionButton("draw_line", "Draw Line Plot")
                                     )
                                 )
                          ),
                          column(8,
                                 div(class = "card",
                                     div(class = "card-header", "Line Plot"),
                                     div(class = "card-body",
                                         plotlyOutput("line_plot", height = "800px")
                                     )
                                 )
                          )
                        )
        ),
        
        bar = tabPanel("Bar Plot", value = "bar",
                       fluidRow(
                         column(4,
                                div(class = "card",
                                    div(class = "card-header", "Bar Plot Options"),
                                    div(class = "card-body",
                                        selectInput("bar_x", "X-axis", ""),
                                        selectInput("bar_y", "Y-axis", ""),
                                        textInput("bar_title", "Plot Title", value = "Bar Plot"),
                                        tags$script("$('#bar_title').attr('maxlength', 60);"),
                                        actionButton("draw_bar", "Draw Plot")
                                    )
                                )
                         ),
                         column(8,
                                div(class = "card",
                                    div(class = "card-header", "Bar Plot"),
                                    div(class = "card-body",
                                        plotlyOutput("bar_plot", height = "800px")
                                    )
                                )
                         )
                       )
        ),
        
        donut = tabPanel("Donut Chart", value = "donut",
                         fluidRow(
                           column(4,
                                  div(class = "card",
                                      div(class = "card-header", "Donut Chart Options"),
                                      div(class = "card-body",
                                          selectInput("pie_var", "Variable for Donut Chart", ""),
                                          textInput("pie_title", "Plot Title", value = "Donut Chart"),
                                          tags$script("$('#pie_title').attr('maxlength', 60);"),
                                          actionButton("draw_pie", "Draw Donut Chart")
                                      )
                                  )
                           ),
                           column(8,
                                  div(class = "card",
                                      div(class = "card-header", "Donut Chart"),
                                      div(class = "card-body",
                                          plotlyOutput("pie_plot", height = "800px")
                                      )
                                  )
                           )
                         )
        ),
        
        histo = tabPanel("Histogram", value = "histo",
                         fluidRow(
                           column(4,
                                  div(class = "card",
                                      div(class = "card-header", "Histogram Options"),
                                      div(class = "card-body",
                                          selectInput("hist_x", "Variable", ""),
                                          sliderInput("hist_bins", "Number of bins:", min = 1, max = 50, value = 30),
                                          textInput("histo_title", "Plot Title", value = "Histogram"),
                                          tags$script("$('#histo_title').attr('maxlength', 60);"),
                                          actionButton("draw_hist", "Draw Plot")
                                      )
                                  )
                           ),
                           column(8,
                                  div(class = "card",
                                      div(class = "card-header", "Histogram"),
                                      div(class = "card-body",
                                          plotlyOutput("hist_plot", height = "800px")
                                      )
                                  )
                           )
                         )
        ),
        
        violin = tabPanel("Violin Plot", value = "violin",
                          fluidRow(
                            column(4,
                                   div(class = "card",
                                       div(class = "card-header", "Violin Plot Options"),
                                       div(class = "card-body",
                                           selectInput("violin_x", "Categorical Variable (X-axis)", ""),
                                           selectInput("violin_y", "Numeric Variable (Y-axis)", ""),
                                           textInput("violin_title", "Plot Title", value = "Violin Plot"),
                                           tags$script("$('#violin_title').attr('maxlength', 60);"),
                                           actionButton("draw_violin", "Draw Plot")
                                       )
                                   )
                            ),
                            column(8,
                                   div(class = "card",
                                       div(class = "card-header", "Violin Plot"),
                                       div(class = "card-body",
                                           plotlyOutput("violin_plot", height = "800px")
                                       )
                                   )
                            )
                          )
        ),
        
        box = tabPanel("Box Plot", value = "box",
                       fluidRow(
                         column(4,
                                div(class = "card",
                                    div(class = "card-header", "Box Plot Options"),
                                    div(class = "card-body",
                                        selectInput("box_x", "X-axis (categorical)", ""),
                                        selectInput("box_y", "Y-axis (numerical)", ""),
                                        textInput("box_title", "Plot Title", value = "Box Plot"),
                                        tags$script("$('#box_title').attr('maxlength', 60);"),
                                        actionButton("draw_box", "Draw Plot")
                                    )
                                )
                         ),
                         column(8,
                                div(class = "card",
                                    div(class = "card-header", "Box Plot"),
                                    div(class = "card-body",
                                        plotlyOutput("box_plot", height = "800px")
                                    )
                                )
                         )
                       )
        ),
        
        heat = tabPanel("Heatmap", value = "heat",
                        fluidRow(
                          column(4,
                                 div(class = "card",
                                     div(class = "card-header", "Heatmap Options"),
                                     div(class = "card-body",
                                         selectInput("heat_x", "X-axis", ""),
                                         selectInput("heat_y", "Y-axis", ""),
                                         selectInput("heat_z", "Z-axis (numerical)", ""),
                                         textInput("heat_title", "Plot Title", value = "Heatmap Plot"),
                                         tags$script("$('#heat_title').attr('maxlength', 60);"),
                                         actionButton("draw_heat", "Draw Plot")
                                     )
                                 )
                          ),
                          column(8,
                                 div(class = "card",
                                     div(class = "card-header", "Heatmap"),
                                     div(class = "card-body",
                                         plotlyOutput("heat_plot", height = "800px")
                                     )
                                 )
                          )
                        )
        ),
        
        ai = tabPanel("AI Analysis", value = "ai",
                      fluidRow(
                        column(4,
                               div(class = "card",
                                   div(class = "card-header", "AI Model Options"),
                                   div(class = "card-body",
                                       selectInput("model_type", "Model Type",
                                                   choices = c("Binary Classification" = "binary_classification",
                                                               "Multi-Class Classification" = "multi_classification",
                                                               "Linear Regression" = "regression")),
                                       selectInput("target_variable", "Target Variable", ""),
                                       selectInput("predictor_variables", "Predictor Variables", "", multiple = TRUE),
                                       actionButton("run_model", "Run Model")
                                   )
                               )
                        ),
                        column(8,
                               div(class = "card",
                                   div(class = "card-header", "Model Results"),
                                   div(class = "card-body",
                                       verbatimTextOutput("model_summary"),
                                       br(),
                                       plotlyOutput("model_plot")
                                   )
                               )
                        )
                      ), br()
        )
      )
      
      # Conditionally add the About tab
      if (!is.null(chart)) {
        charts <- unlist(strsplit(chart, ",\\s*"))
        
        if (!("about" %in% charts)){
          tabs$about <- NULL
        }
        
        if (is.null(csv)) {
          charts <- c(charts, list("data"))
        }
        
        # Conditionally remove tabs not in charts
        tabs <- tabs[names(tabs) %in% charts] %>%
          unname(tabs)
        
        # Set the first chart as the selected tab
        updateNavbarPage(session, "navbar", selected = charts[1]) 
      } else {
        if (!is.null(csv)) {
          tabs$about <- NULL
        }
        tabs <- unname(tabs)
      }
      
      do.call(navbarPage, c(list("visR", id = "navbar", collapsible = TRUE), tabs))
    })
    
    output$data_tab_content <- renderUI({
      if (!is.null(csv)) {
        tagList(
          h3("Loaded CSV File:"),
          p(csv),
          br(),
          DTOutput("data_table"),
          br(), br(),
          uiOutput("column_type_ui")
        )
      } else {
        tagList(
          fileInput("file", "Load CSV File (up to 50MB)",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          br(), br(),
          uiOutput("file_name_display"),
          DTOutput("data_table"),
          br(), br(),
          uiOutput("column_type_ui")
        )
      }
    })
    
    # Load data if csv parameter is provided
    if (!is.null(csv)) {
      observeEvent(1, {
        df <- data_load(csv, session)
      })
    } else {
      
      observeEvent(input$file, {
        req(input$file)
        if (input$file$size > 50 * 1024^2) {
          showNotification("File size exceeds 50MB limit.", type = "error")
          return(NULL)
        }
        df <- data_load(input$file$datapath, session)
        showNotification("File uploaded successfully!", type = "message")
      })
    }
    
    output$file_name_display <- renderUI({
      req(input$file)
      div(
        style = "text-align: center; font-weight: bold; font-size: 18px;",
        paste("Loaded file:", input$file$name),
        br(), br()
      )
    })
    
    output$data_table <- renderDT({
      req(data())
      datatable(data(), options = list(scrollX = TRUE, scrollY = "300px"))
    })
    
    output$column_type_ui <- renderUI({
      req(data())
      column_types <- sapply(data(), class)
      
      tagList(
        h4("Column Type Summary"), br(),
        htmlOutput("type_summary"),
        br(), br(),
        h4("Column Type Changer"), br(),
        fluidRow(
          column(4, strong("Select column:")),
          column(3, strong("Actual type:")),
          column(3, strong("New type:")),
          column(2)
        ),
        fluidRow(
          column(4, selectInput("column_to_modify", NULL, choices = names(column_types), width = "100%")),
          column(3, textOutput("original_type")),
          column(3, selectInput("new_type", NULL,
                                choices = c("character", "numeric", "integer", "factor", "date"),
                                width = "100%")),
          column(2, actionButton("apply_type_change", "Apply", width = "100%"))
        ), br()
      )
    })
    
    output$original_type <- renderText({
      req(data(), input$column_to_modify)
      paste(class(data()[[input$column_to_modify]])[1])
    })
    
    output$type_summary <- renderUI({
      req(data())
      column_types <- sapply(data(), class)
      unique_types <- unique(column_types)
      
      summary_lines <- lapply(unique_types, function(type) {
        columns <- names(column_types)[column_types == type]
        p(
          strong(paste0(type, " (", length(columns), "):")),
          paste(columns, collapse = ", ")
        )
      })
      
      do.call(tagList, summary_lines)
    })
    
    observeEvent(input$apply_type_change, {
      req(data(), input$column_to_modify, input$new_type)
      
      new_data <- data()
      col <- input$column_to_modify
      new_type <- input$new_type
      current_type <- class(new_data[[col]])[1]
      
      # Improved function to check if conversion is possible
      can_convert <- function(x, to_type) {
        if (to_type %in% c("numeric", "integer") && is.character(x)) {
          # For character to numeric/integer, check if all non-NA values are numeric
          return(all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", na.omit(x))))
        }
        
        tryCatch({
          if(to_type == "factor") {
            as.factor(x)
          } else if(to_type == "date") {
            as.Date(x)
          } else {
            do.call(paste0("as.", to_type), list(x))
          }
          return(TRUE)
        }, error = function(e) {
          return(FALSE)
        })
      }
      
      # Check if conversion is possible
      if(!can_convert(new_data[[col]], new_type)) {
        showNotification(paste("Cannot convert", col, "from", current_type, "to", new_type), type = "error")
        return()
      }
      
      tryCatch({
        if(new_type == "factor") {
          new_data[[col]] <- as.factor(new_data[[col]])
        } else if(new_type == "date") {
          new_data[[col]] <- as.Date(new_data[[col]])
        } else {
          new_data[[col]] <- do.call(paste0("as.", new_type), list(new_data[[col]]))
        }
        
        # Check if the conversion was successful
        if (class(new_data[[col]])[1] != new_type) {
          stop(paste("Conversion to", new_type, "was not successful."))
        }
        
        data(new_data)
        showNotification(paste("Column", col, "converted to", new_type), type = "message")
      }, error = function(e) {
        error_message <- paste("Error converting", col, "to", new_type, ":", e$message)
        showNotification(error_message, type = "error")
      })
    })
    
    # Scatter Plot
    scatter_data <- reactiveVal(NULL)
    scatter_axes <- reactiveVal(list(x = "", y = "", color = "", size = ""))
    
    observeEvent(input$draw_scatter, {
      req(data())
      scatter_data(list(
        x = data()[[input$scatter_x]],
        y = data()[[input$scatter_y]],
        color = if(input$scatter_color != "") data()[[input$scatter_color]] else NULL,
        size = if(input$scatter_size != "") data()[[input$scatter_size]] else NULL
      ))
      scatter_axes(list(
        x = input$scatter_x,
        y = input$scatter_y,
        color = input$scatter_color,
        size = input$scatter_size
      ))
    })
    
    output$scatter_plot <- renderPlotly({
      req(scatter_data())
      df <- scatter_data()
      axes <- scatter_axes()
      
      # Initialize marker properties
      marker_props <- list(
        size = 10,  # Default size
        color = 'steelblue'  # Default color
      )
      
      # Initialize legend status
      show_legend <- FALSE
      legend_group <- NULL
      
      # Apply size if selected
      if(!is.null(df$size)) {
        marker_props$size <- df$size
        marker_props$sizeref <- 2 * max(df$size) / (50^2)
        marker_props$sizemode <- 'area'
      }
      
      # Apply color if selected
      if(!is.null(df$color)) {
        color_var <- df$color
        if(is.numeric(color_var)) {
          marker_props$color <- color_var
          marker_props$colorscale <- 'Oranges'
          marker_props$colorbar <- list(title = "")
          show_legend <- FALSE
        } else {
          color_levels <- unique(color_var)
          marker_props$color <- color_var
          marker_props$colorscale <- 'Oranges'
          show_legend <- TRUE
          legend_group <- color_var
        }
      }
      
      # Create the plot
      p <- plot_ly(x = df$x, y = df$y,
                   type = 'scatter', mode = 'markers',
                   marker = marker_props,
                   showlegend = show_legend,
                   legendgroup = legend_group,
                   name = input$scatter_color)
      
      # Add hover text
      hover_text <- paste("X:", df$x,
                          "\nY:", df$y)
      if(!is.null(df$color)) {
        hover_text <- paste(hover_text,
                            "\nColor:", df$color)
      }
      if(!is.null(df$size)) {
        hover_text <- paste(hover_text,
                            "\nSize:", df$size)
      }
      p <- p %>% add_trace(text = hover_text, hoverinfo = 'text', showlegend = FALSE)
      
      # Layout
      p <- p %>% layout(
        title = list(text = input$scatter_title, font = list(size = 18)),
        margin = list(t = 60, b = 80),
        xaxis = list(title = list(text = axes$x, standoff = 15)),
        yaxis = list(title = list(text = axes$y, standoff = 15), ticksuffix = "  "),
        legend = list(title = list(text = axes$color))
      )
      
      p
    })
    
    # Line Plot
    line_data <- reactiveVal(NULL)
    line_axes <- reactiveVal(list(x = "", y = ""))
    
    observeEvent(input$draw_line, {
      req(data())
      line_data(list(
        x = data()[[input$line_x]],
        y = data()[[input$line_y]]
      ))
      line_axes(list(
        x = input$line_x,
        y = input$line_y
      ))
    })
    
    output$line_plot <- renderPlotly({
      req(line_data())
      df <- line_data()
      axes <- line_axes()
      
      # Check if both x and y are present
      req(df$x, df$y)
      
      # Ensure x and y have the same length
      x_var <- df$x
      y_var <- df$y
      
      # Remove NA values
      valid_indices <- which(!is.na(x_var) & !is.na(y_var))
      x_var <- x_var[valid_indices]
      y_var <- y_var[valid_indices]
      
      # Check if there are any valid data points
      req(length(x_var) > 0, length(y_var) > 0)
      
      p <- plot_ly(x = x_var, y = y_var,
                   type = 'scatter', mode = 'lines',
                   line = list(color = 'steelblue'),
                   name = "Line") %>%
        layout(
          title = list(text = input$line_title, font = list(size = 18)),
          margin = list(t = 60, b = 80),
          xaxis = list(title = list(text = axes$x, standoff = 15)),
          yaxis = list(title = list(text = axes$y, standoff = 15), ticksuffix = "  ")
        )
      
      p
    })
    
    # Bar Plot
    # Reactive values to store bar plot data and axes
    bar_data <- reactiveVal(NULL)
    
    # Observe event to update bar plot data and axes
    observeEvent(input$draw_bar, {
      req(data())
      bar_data(list(
        x = data()[[input$bar_x]],
        y = data()[[input$bar_y]],
        x_title = input$bar_x,
        y_title = input$bar_y,
        title = input$bar_title
      ))
    })
    
    # Render the bar plot
    output$bar_plot <- renderPlotly({
      req(bar_data())
      df <- bar_data()
      
      # Create a data frame from the list
      plot_df <- data.frame(Item = df$x, Quantity = df$y)
      
      # Create the plotly bar chart
      p <- plot_ly(data = plot_df, x = ~Item, y = ~Quantity, type = 'bar',
                   color = ~Item, showlegend = FALSE) %>%
        layout(title = list(text = df$title, font = list(size = 18)),
               margin = list(t = 60, b = 80),
               xaxis = list(title = list(text = df$x_title, standoff = 15)),
               yaxis = list(title = list(text = df$y_title, standoff = 15), ticksuffix = "  "),
               barmode='stack')  # Group bars by Item
      
      p
    })
    
    # Donut chart
    donut_data <- reactiveVal(NULL)
    
    # Observer for the "Draw Chart" button click
    observeEvent(input$draw_pie, {
      req(data())
      req(input$pie_var)
      
      donut_data(data()[[input$pie_var]])
    })
    
    # Modify the renderPlotly function for the donut chart
    output$pie_plot <- renderPlotly({
      req(donut_data())
      df <- data.frame(table(donut_data()))
      colnames(df) <- c("Category", "Count")
      
      plot_ly(df, labels = ~Category, values = ~Count, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(Category, ": ", Count),
              marker = list(colors = 'Set1'),
              hole = 0.4) %>%
        layout(title = list(text = input$pie_title, font = list(size = 18)),
               margin = list(t = 60, b = 80),
               showlegend = TRUE,
               legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1))
    })
    
    # Update the observer for pie chart variable choices
    observe({
      req(data())
      updateSelectInput(session, "pie_var", choices = names(data()))
    })
    
    
    # Histogram
    hist_data <- reactiveVal(NULL)
    hist_axes <- reactiveVal(list(x = ""))
    
    observeEvent(input$draw_hist, {
      req(data())
      hist_data(list(
        x = data()[[input$hist_x]],
        bins = input$hist_bins
      ))
      hist_axes(list(
        x = input$hist_x
      ))
    })
    
    output$hist_plot <- renderPlotly({
      req(hist_data())
      df <- hist_data()
      axes <- hist_axes()
      
      hist_data <- hist(df$x, breaks = df$bins, plot = FALSE)
      
      plot_ly() %>%
        add_bars(x = hist_data$mids,
                 y = hist_data$counts,
                 marker = list(
                   color = 'steelblue',
                   line = list(color = "white", width = 0.5)
                 )) %>%
        layout(title = list(text = input$histo_title, font = list(size = 18)),
               margin = list(t = 60, b = 80),
               xaxis = list(title = list(text = axes$x, standoff = 15)),
               yaxis = list(title = list(text = "Count", standoff = 15), ticksuffix = "  "),
               bargap = 0.1)
    })
    
    # Violin Plot
    violin_data <- reactiveVal(NULL)
    violin_axes <- reactiveVal(list(x = "", y = ""))
    
    observeEvent(input$draw_violin, {
      req(data())
      violin_data(list(
        x = data()[[input$violin_x]],
        y = data()[[input$violin_y]]
      ))
      violin_axes(list(
        x = input$violin_x,
        y = input$violin_y
      ))
    })
    
    output$violin_plot <- renderPlotly({
      req(violin_data())
      df <- violin_data()
      axes <- violin_axes()
      
      p <- plot_ly(x = df$x, y = df$y, type = 'violin', box = list(visible = TRUE),
                   meanline = list(visible = TRUE), color = df$x) %>%
        layout(title = list(text = input$violin_title, font = list(size = 18)),
               margin = list(t = 60, b = 80),
               xaxis = list(title = list(text = axes$x, standoff = 15)),
               yaxis = list(title = list(text = axes$y, standoff = 15), ticksuffix = "  "))
      
      p
    })
    
    # Box Plot
    box_data <- reactiveVal(NULL)
    box_axes <- reactiveVal(list(x = "", y = ""))
    
    observeEvent(input$draw_box, {
      req(data())
      box_data(list(
        x = data()[[input$box_x]],
        y = data()[[input$box_y]]
      ))
      box_axes(list(
        x = input$box_x,
        y = input$box_y
      ))
    })
    
    output$box_plot <- renderPlotly({
      req(box_data())
      df <- box_data()
      axes <- box_axes()
      
      plot_ly(x = df$x, y = df$y, type = 'box',
              color = df$x) %>%
        layout(title = list(text = input$box_title, font = list(size = 18)),
               margin = list(t = 60, b = 80),
               xaxis = list(title = list(text = axes$x, standoff = 15)),
               yaxis = list(title = list(text = axes$y, standoff = 15), ticksuffix = "  "))
    })
    
    # Heatmap (Matrix View)
    heat_data <- reactiveVal(NULL)
    heat_axes <- reactiveVal(list(x = "", y = "", z = ""))
    
    observeEvent(input$draw_heat, {
      req(data())
      heat_data(list(
        x = data()[[input$heat_x]],
        y = data()[[input$heat_y]],
        z = if(input$heat_z != "") data()[[input$heat_z]] else NULL
      ))
      heat_axes(list(
        x = input$heat_x,
        y = input$heat_y,
        z = input$heat_z
      ))
    })
    
    output$heat_plot <- renderPlotly({
      req(heat_data())
      df <- heat_data()
      axes <- heat_axes()
      
      # Create a matrix from the selected columns
      matrix_data <- as.matrix(table(df$x, df$y))
      
      # If a z-axis is selected, use it to fill the matrix
      if (!is.null(df$z)) {
        z_values <- tapply(df$z, list(df$y, df$x), mean)
        matrix_data[!is.na(matrix_data)] <- z_values[!is.na(z_values)]
      }
      
      # Transpose the matrix to correct the orientation
      matrix_data <- t(matrix_data)
      
      plot_ly(
        x = colnames(matrix_data),
        y = rownames(matrix_data),
        z = matrix_data,
        type = "heatmap",
        colorscale = "Viridis"
      ) %>%
        layout(
          title = list(text = input$heat_title, font = list(size = 18)),
          margin = list(t = 60, b = 80),
          xaxis = list(title = list(text = axes$x, standoff = 15)),
          yaxis = list(title = list(text = axes$y, standoff = 15), ticksuffix = "  ")
        )
    })
    
    # AI Model
    model_data <- reactiveVal(NULL)
    
    observeEvent(input$run_model, {
      req(data(), input$target_variable, input$predictor_variables, input$model_type)
      model_data(list(
        df = data(),
        target = input$target_variable,
        predictors = input$predictor_variables,
        model_type = input$model_type
      ))
    })
    
    output$model_summary <- renderPrint({
      req(model_data())
      md <- model_data()
      
      # Prepare data
      df <- md$df
      
      # Ensure target variable is a factor for classification
      if(md$model_type %in% c("binary_classification", "multi_classification")) {
        df[[md$target]] <- as.factor(df[[md$target]])
      }
      
      # Split data into training and testing sets
      set.seed(123)
      trainIndex <- createDataPartition(df[[md$target]], p = .7,
                                        list = FALSE, times = 1)
      trainData <- df[trainIndex, ]
      testData <- df[-trainIndex, ]
      
      if(md$model_type == "binary_classification" || md$model_type == "multi_classification") {
        # Classification (Binary or Multi-class)
        
        # Convert target variable to factor
        trainData[[md$target]] <- as.factor(trainData[[md$target]])
        testData[[md$target]] <- as.factor(testData[[md$target]])
        
        # Train Random Forest model
        model <- randomForest(as.formula(paste(md$target, "~", paste(md$predictors, collapse = "+"))),
                              data = trainData, ntree = 100)
        
        # Make predictions
        predictions <- predict(model, newdata = testData)
        
        # Calculate accuracy
        accuracy <- mean(predictions == testData[[md$target]])
        
        # Confusion Matrix
        cm <- confusionMatrix(predictions, testData[[md$target]])
        
        # Display results
        cat("Random Forest Model Summary:\n")
        print(model)
        cat("\nAccuracy:", accuracy)
        cat("\n\nConfusion Matrix:\n")
        print(cm$table)
        cat("\nModel Performance:\n")
        print(cm$overall)
        
      } else {
        # Linear Regression
        
        # Check and convert target variable to numeric
        if (!is.numeric(trainData[[md$target]])) {
          cat("Target variable must be numeric for linear regression. Please select a numeric variable.")
          return()
        }
        trainData[[md$target]] <- as.numeric(trainData[[md$target]])
        testData[[md$target]] <- as.numeric(testData[[md$target]])
        
        tryCatch({
          # Train model
          model <- lm(as.formula(paste(md$target, "~", paste(md$predictors, collapse = "+"))),
                      data = trainData)
          
          # Make predictions
          predictions <- predict(model, newdata = testData)
          
          # Get actual values
          actual_values <- testData[[md$target]]
          
          # Calculate RMSE and R-squared
          rmse <- sqrt(mean((actual_values - predictions)^2))
          rsq <- summary(model)$r.squared
          
          # Display results
          cat("Linear Regression Model Summary:\n")
          print(summary(model))
          cat("\nRoot Mean Squared Error:", rmse)
          cat("\nR-squared:", rsq)
          
        }, error = function(e) {
          cat("Error in regression model:", e$message)
        })
      }
    })
    
    output$model_plot <- renderPlotly({
      req(model_data())
      md <- model_data()
      
      tryCatch({
        if(md$model_type %in% c("binary_classification", "multi_classification")) {
          # Classification plot (feature importance)
          df <- md$df
          df[[md$target]] <- as.factor(df[[md$target]])  # Ensure target is a factor
          
          model <- randomForest(as.formula(paste(md$target, "~", paste(md$predictors, collapse = "+"))),
                                data = df, ntree = 100)
          
          # Get feature importance
          imp <- importance(model)
          imp_df <- data.frame(feature = rownames(imp), importance = imp[,1])
          imp_df <- imp_df[order(imp_df$importance),]
          
          # Convert the 'feature' column to a factor with levels in the desired order
          imp_df$feature <- factor(imp_df$feature, levels = imp_df$feature)
          
          # Create Plotly bar plot
          p <- plot_ly(imp_df, x = ~importance, y = ~feature, type = 'bar', orientation = 'h',
                       marker = list(color = 'steelblue')) %>%
            layout(title = list(text = "Predicted vs Actual"),
                   xaxis = list(title = list(text = "Importance"), standoff = 15),
                   yaxis = list(title = list(text = "Features", standoff = 15), # Adjust standoff value as needed
                                autorange = "reversed", automargin = TRUE, ticksuffix = "  "),
                   margin = list(t = 60))
          
          return(p)
        } else {
          # Regression plot (predicted vs actual)
          model <- lm(as.formula(paste(md$target, "~", paste(md$predictors, collapse = "+"))),
                      data = md$df)
          predictions <- predict(model)
          actual_values <- md$df[[md$target]]
          
          plot_data <- data.frame(actual = actual_values, predicted = predictions)
          
          p <- plot_ly(data = plot_data, x = ~actual, y = ~predicted,
                       type = "scatter", mode = "markers",  # Explicitly specify mode as "markers"
                       marker = list(color = 'blue', size = 8, opacity = 0.6)) %>%
            add_trace(x = range(actual_values), y = range(actual_values),
                      type = "scatter", mode = "lines", line = list(color = 'red'),
                      showlegend = FALSE) %>%
            layout(title = "Predicted vs Actual",
                   xaxis = list(title = list(text ="Actual Values"), standoff = 15),
                   yaxis = list(title = list(text = "Predicted Values", standoff = 15), ticksuffix = "  "),
                   margin = list(t = 60))
          
          return(p)
        }
      }, error = function(e) {
        # If an error occurs, print it and return a blank plot
        print(paste("Error in model_plot:", e$message))
        return(plot_ly() %>% add_annotations(
          text = paste("Error:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        ))
      })
    })
    
    # If csv parameter is provided, load the data
    if (!is.null(csv)) {
      observeEvent(1, {
        df <- data_load(csv, session)
      })
    }
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}

# Run the full app if no parameters are provided
if (sys.nframe() == 0) {
  visR()
}
