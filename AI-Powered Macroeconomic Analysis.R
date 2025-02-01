# -------------------------------
# 📦 Load Required Libraries
# -------------------------------
library(shiny)       # RShiny for interactive UI
library(ggplot2)     # Data visualization
library(wbstats)     # World Bank API for economic data
library(dplyr)       # Data manipulation
library(httr)        # OpenAI API calls
library(jsonlite)    # Handling JSON responses
library(readr)       # Exporting data for EViews
library(promises)    # For async API calls
library(future)      # For parallel processing in async tasks

# Set up future plan to run async tasks in background
plan(multisession)

# -------------------------------
# 📊 Load Macroeconomic Data from World Bank API
# -------------------------------
fetch_macro_data <- function() {
  indicators <- c("NY.GDP.MKTP.CD",  # GDP (Current US$)
                  "FP.CPI.TOTL",     # Inflation (CPI)
                  "NE.EXP.GNFS.CD")  # Exports of Goods & Services
  
  # Fetch data for all available years
  data <- wb(indicator = indicators, country = "USA", startdate = 2000, enddate = 2023)
  
  # Process data for visualization
  data <- data %>%
    select(date, indicatorID, value) %>%
    mutate(date = as.integer(date))
  
  return(data)
}

# Load the dataset
macro_data <- fetch_macro_data()

# -------------------------------
# 📤 Export Data to EViews-Compatible Format
# -------------------------------
export_to_eviews <- function(data) {
  # Save as CSV (EViews can import CSV files)
  write_csv(data, "macro_data_eviews.csv")
  
  # Alternatively, export as PRN (plain text format)
  write_delim(data, "macro_data_eviews.prn", delim = "\t")
}

# Call export function to generate files for EViews
export_to_eviews(macro_data)

# -------------------------------
# 🤖 AI Integration: OpenAI API for Economic Insights (Async Version)
# -------------------------------
generate_ai_insights <- function(text_input) {
  openai_api_key <- "XXX"  # Replace with your OpenAI API key
  
  response <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", openai_api_key)),
      content_type_json(),
      body = toJSON(list(
        model = "gpt-4o-mini",
        messages = list(
          list(role = "system", content = "You are an economist providing macroeconomic insights."),
          list(role = "user", content = text_input)
        )
      ), auto_unbox = TRUE)
    )
  }, error = function(e) {
    return(NULL)  # Return NULL if API request fails
  })
  
  if (is.null(response)) {
    return("⚠️ AI service unavailable. Please try again later.")  # Error handling
  }
  
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  print(result)
  return(result$choices[[1]]$message$content)
}

# -------------------------------
# 📺 RShiny UI: Interactive Dashboard
# -------------------------------
ui <- fluidPage(
  titlePanel("📊 AI-Powered Macroeconomic Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select Economic Indicator:", 
                  choices = c("GDP" = "NY.GDP.MKTP.CD", 
                              "Inflation (CPI)" = "FP.CPI.TOTL", 
                              "Exports" = "NE.EXP.GNFS.CD")),
      textInput("ai_query", "Ask AI for Insights:", "What are the trends in GDP?"),
      actionButton("generate", "Generate AI Insights"),
      downloadButton("download_data", "Download Data for EViews")  # Button to download data
    ),
    
    mainPanel(
      plotOutput("macro_plot"),
      verbatimTextOutput("ai_output")
    )
  )
)

# -------------------------------
# Server Logic for Data Processing & AI Integration
# -------------------------------
server <- function(input, output, session) {
  # Filter data based on selected indicator
  filtered_data <- reactive({
    macro_data %>% filter(indicatorID == input$indicator)
  })
  
  # Plot economic indicator trends
  output$macro_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = value)) +
      geom_line(color = "blue", size = 1.2) +
      ggtitle(paste("Macroeconomic Indicator:", input$indicator)) +
      xlab("Year") + ylab("Value") + theme_minimal()
  })
  
  # AI-generated macroeconomic insights with asynchronous handling
  observeEvent(input$generate, {
    query_text <- isolate(input$ai_query)  # Ensure proper reactive handling
    
    # Show loading message before API response arrives
    output$ai_output <- renderText("⏳ Generating insights... Please wait.")
    
    # Async task for generating AI insights
    future({
      generate_ai_insights(query_text)  # Call AI function in the background
    }) %...>% {  # When the future is completed, update the UI
      output$ai_output <- renderText(.)
    }
  })
  
  # Allow users to download data for EViews
  output$download_data <- downloadHandler(
    filename = function() { "macro_data_eviews.csv" },
    content = function(file) {
      write_csv(macro_data, file)
    }
  )
}

# -------------------------------
# 🚀 Run the Shiny App
# -------------------------------
shinyApp(ui = ui, server = server)
