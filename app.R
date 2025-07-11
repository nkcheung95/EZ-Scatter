# Define the packages you want to use
packages <- c("shiny", "ggplot2")

# Function to install and load packages
install_load_packages <- function(packages) {
  # Check which packages are not installed
  not_installed <- setdiff(packages, rownames(installed.packages()))
  
  # Install the missing packages
  if (length(not_installed) > 0) {
    install.packages(not_installed)
  }
  
  # Load all the packages
  invisible(sapply(packages, library, character.only = TRUE))
}

# Call the function to install and load packages
install_load_packages(packages)

# Define UI
ui <- fluidPage(
  titlePanel("GGPlot2 Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      selectInput("x_var", "X Variable", NULL),
      selectInput("y_var", "Y Variable", NULL),
      selectInput("group_var", "Group Variable (for point colors)", NULL),
      selectInput("point_color", "Select Default Point Color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900")),
      selectInput("regression_color", "Select Regression Line Color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900")),
      selectInput("theme", "Select Theme", 
                  choices = c("Default", "Classic", "Minimal", "Dark")),
      checkboxInput("regression", "Add Regression Line", value = FALSE),
      checkboxInput("color_by_group", "Color points by group", value = FALSE),
      textInput("title", "Title", ""),
      textInput("x_axis_label", "X-Axis Label", ""),
      textInput("y_axis_label", "Y-Axis Label", ""),
      checkboxInput("show_ticks", "Show Ticks on Axes", value = TRUE),
      sliderInput("plot_width", "Plot Width", min = 5, max = 20, value = 10),
      sliderInput("plot_height", "Plot Height", min = 5, max = 20, value = 8)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read CSV file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    updateSelectInput(session, "x_var", choices = colnames(df))
    updateSelectInput(session, "y_var", choices = colnames(df))
    updateSelectInput(session, "group_var", choices = c("None", colnames(df)))
    return(df)
  })
  
  # Update axis labels when variables change
  observeEvent(input$x_var, {
    updateTextInput(session, "x_axis_label", value = input$x_var)
  })
  
  observeEvent(input$y_var, {
    updateTextInput(session, "y_axis_label", value = input$y_var)
  })
  
  # Generate plot
  output$plot <- renderPlot({
    req(input$x_var, input$y_var)
    df <- data()
    
    # Create base plot
    p <- ggplot(df, aes_string(x = input$x_var, y = input$y_var))
    
    # Apply selected theme
    if (input$theme == "Classic") {
      p <- p + theme_classic()
    } else if (input$theme == "Minimal") {
      p <- p + theme_minimal()
    } else if (input$theme == "Dark") {
      p <- p + theme_dark()
    } else {
      p <- p + theme_bw() # default theme
    }
    
    # Add points with or without grouping
    if (input$color_by_group && input$group_var != "None") {
      p <- p + geom_point(aes_string(color = input$group_var)) +
        labs(color = input$group_var)
    } else {
      p <- p + geom_point(color = input$point_color)
    }
    
    # Add regression line with R and R-squared if selected
    if (input$regression) {
      formula <- as.formula(paste(input$y_var, "~", input$x_var))
      fit <- lm(formula, data = df)
      r_value <- cor(df[[input$x_var]], df[[input$y_var]], use = "complete.obs")
      
      p <- p + geom_smooth(method = "lm", se = FALSE, color = input$regression_color) +
        annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
                 label = paste0("R = ", round(r_value, 3), 
                               "\nR² = ", round(summary(fit)$r.squared, 3)))
    }
    
    # Set plot title and axis labels
    p <- p + labs(title = input$title, x = input$x_axis_label, y = input$y_axis_label)
    
    # Show ticks on axes if selected
    if (!input$show_ticks) {
      p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank())
    }
    
    return(p)
  }, width = reactive(input$plot_width * 100), height = reactive(input$plot_height * 100))
}

# Run the application
shinyApp(ui = ui, server = server)
