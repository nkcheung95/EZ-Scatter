library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("GGPlot2 Visualizer with Grouping"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(".csv", ".txt"),
                multiple = FALSE),
      tags$hr(),
      uiOutput("x_var_ui"),
      uiOutput("y_var_ui"),
      uiOutput("group_var_ui"),
      checkboxInput("use_group", "Color by group", value = FALSE),
      selectInput("point_color", "Default point color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900"),
                  selected = "#003366"),
      selectInput("regression_color", "Regression line color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900"),
                  selected = "#CC0000"),
      selectInput("theme", "Select theme", 
                  choices = c("Default" = "theme_bw", 
                             "Classic" = "theme_classic", 
                             "Minimal" = "theme_minimal", 
                             "Dark" = "theme_dark")),
      checkboxInput("regression", "Add regression line", value = FALSE),
      textInput("title", "Plot title", ""),
      textInput("x_axis_label", "X-axis label", ""),
      textInput("y_axis_label", "Y-axis label", ""),
      checkboxInput("show_ticks", "Show axis ticks", value = TRUE),
      sliderInput("plot_width", "Plot width (inches)", min = 5, max = 20, value = 10),
      sliderInput("plot_height", "Plot height (inches)", min = 5, max = 20, value = 8)
    ),
    mainPanel(
      plotOutput("plot", width = "100%")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data frame
  data <- reactive({
    req(input$file)
    
    tryCatch({
      df <- read.csv(input$file$datapath)
      # Convert character columns to factors if they have <= 10 unique values
      char_cols <- sapply(df, is.character)
      df[char_cols] <- lapply(df[char_cols], function(x) {
        if (length(unique(x)) <= 10) as.factor(x) else x
      })
      df
    }, error = function(e) {
      showNotification("Error reading file. Please check the file format.", type = "error")
      NULL
    })
  })
  
  # Dynamic UI for variable selection
  output$x_var_ui <- renderUI({
    req(data())
    selectInput("x_var", "X variable", choices = names(data()))
  })
  
  output$y_var_ui <- renderUI({
    req(data())
    selectInput("y_var", "Y variable", choices = names(data()))
  })
  
  output$group_var_ui <- renderUI({
    req(data())
    selectInput("group_var", "Group variable", 
               choices = c("None", names(data())[sapply(data(), function(x) is.factor(x) | is.character(x))]))
  })
  
  # Update axis labels when variables change
  observeEvent(input$x_var, {
    updateTextInput(session, "x_axis_label", value = input$x_var)
  })
  
  observeEvent(input$y_var, {
    updateTextInput(session, "y_axis_label", value = input$y_var)
  })
  
  # Main plot
  output$plot <- renderPlot({
    req(data(), input$x_var, input$y_var)
    df <- data()
    
    # Validate selected variables exist in data
    validate(
      need(input$x_var %in% names(df), "X variable not found in data"),
      need(input$y_var %in% names(df), "Y variable not found in data")
    )
    
    # Create base plot
    p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var))
    
    # Apply selected theme
    if (input$theme != "theme_bw") {
      p <- p + do.call(input$theme, list())
    } else {
      p <- p + theme_bw()
    }
    
    # Add points with or without grouping
    if (input$use_group && !is.null(input$group_var) && input$group_var != "None") {
      p <- p + geom_point(aes_string(color = input$group_var), size = 3, alpha = 0.7) +
        labs(color = input$group_var)
    } else {
      p <- p + geom_point(color = input$point_color, size = 3, alpha = 0.7)
    }
    
    # Add regression line if requested
    if (input$regression) {
      # Calculate correlation and regression
      cor_test <- try(cor.test(df[[input$x_var]], df[[input$y_var]], method = "pearson"))
      
      if (!inherits(cor_test, "try-error")) {
        r_value <- round(cor_test$estimate, 3)
        p_value <- round(cor_test$p.value, 5)
        
        p <- p + 
          geom_smooth(method = "lm", se = FALSE, color = input$regression_color) +
          annotate("text", 
                   x = Inf, y = Inf,
                   hjust = 1.1, vjust = 1.5,
                   label = paste0("Pearson r = ", r_value, "\n",
                                 "p-value = ", ifelse(p_value < 0.001, "< 0.001", p_value)),
                   size = 5)
      }
    }
    
    # Add labels and titles
    p <- p + 
      labs(title = input$title,
           x = ifelse(input$x_axis_label == "", input$x_var, input$x_axis_label),
           y = ifelse(input$y_axis_label == "", input$y_var, input$y_axis_label)) +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
    # Hide ticks if requested
    if (!input$show_ticks) {
      p <- p + theme(axis.ticks = element_blank(),
                     axis.text = element_blank())
    }
    
    p
  }, 
  width = function() input$plot_width * 72,  # Convert inches to pixels
  height = function() input$plot_height * 72)
}

shinyApp(ui = ui, server = server)
