# Load necessary libraries
library(shiny)
library(ggplot2)

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
      selectInput("point_color", "Select Point Color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900")),
      selectInput("regression_color", "Select Regression Line Color",
                  choices = c("Blue" = "#003366", "Red" = "#CC0000", "Green" = "#009900")),
      selectInput("theme", "Select Theme", 
                  choices = c("Default", "Classic", "Minimal", "Dark")),
      checkboxInput("regression", "Add Regression Line", value = FALSE),
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
    p <- ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) +
      theme_bw() # default theme
    
    # Apply selected theme
    if (input$theme == "Classic") {
      p <- p + theme_classic()
    } else if (input$theme == "Minimal") {
      p <- p + theme_minimal()
    } else if (input$theme == "Dark") {
      p <- p + theme_dark()
    }
    
    # Add chosen geom layer (point)
    p <- p + geom_point(color = input$point_color)
    
    # Add regression line with R-squared if selected
    if (input$regression) {
      fit <- lm(data = data(), formula = as.formula(paste(input$y_var, "~", input$x_var)))
      p <- p + geom_smooth(method = "lm", se = FALSE, color = input$regression_color) +
        annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
                 label = paste("R-squared =", round(summary(fit)$r.squared, 3)))
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
