# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(maps)

# Define UI
ui <- fluidPage(
  titlePanel("Automatic Geographical Mapping App"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept = c('.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE)
    ),
    mainPanel(
      uiOutput("map_ui")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to read the uploaded data
  dataInput <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header, stringsAsFactors = FALSE)
    return(df)
  })
  
  # Reactive expression to detect geographical columns
  geo_info <- reactive({
    df <- dataInput()
    # Define possible geographical column names
    geo_columns <- c('country', 'state', 'county', 'region', 'city', 'area', 'province', 'department')
    col_names <- tolower(names(df))
    matched_cols <- intersect(col_names, geo_columns)
    # Create a list of detected geographical levels
    levels <- list()
    if('country' %in% matched_cols) levels$country <- names(df)[which(col_names == 'country')]
    if('state' %in% matched_cols || 'province' %in% matched_cols) levels$state <- names(df)[which(col_names %in% c('state', 'province'))[1]]
    if('county' %in% matched_cols) levels$county <- names(df)[which(col_names == 'county')]
    if(length(levels) > 0){
      return(levels)
    } else {
      return(NULL)
    }
  })
  
  # UI output to display select inputs and map
  output$map_ui <- renderUI({
    req(geo_info())
    df <- dataInput()
    # Exclude geographical columns to find numeric columns for mapping
    value_columns <- setdiff(names(df), unlist(geo_info()))
    value_columns <- value_columns[sapply(df[value_columns], is.numeric)]  # Only numeric columns
    if(length(value_columns) == 0){
      h4("No numeric columns to map.")
    } else {
      tagList(
        selectInput("geo_level", "Select Geographical Level", choices = names(geo_info())),
        selectInput("value_col", "Select Value Column", choices = value_columns),
        plotOutput("mapPlot")
      )
    }
  })
  
  # Render the map based on user selections
  output$mapPlot <- renderPlot({
    req(geo_info())
    req(input$value_col)
    req(input$geo_level)
    df <- dataInput()
    geo_column <- geo_info()[[input$geo_level]]
    value_column <- input$value_col
    
    if(input$geo_level == 'country') {
      # World map
      world <- map_data("world")
      df_map <- df %>%
        mutate(region = tolower(get(geo_column)))
      world$region <- tolower(world$region)
      merged_data <- left_join(world, df_map, by = "region")
      ggplot(merged_data, aes(long, lat, group = group)) +
        geom_polygon(aes_string(fill = value_column), color = "white") +
        coord_fixed(1.3) +
        theme_minimal()
    } else if(input$geo_level == 'state') {
      # US States map
      states <- map_data("state")
      df_map <- df %>%
        mutate(region = tolower(get(geo_column)))
      states$region <- tolower(states$region)
      merged_data <- left_join(states, df_map, by = "region")
      ggplot(merged_data, aes(long, lat, group = group)) +
        geom_polygon(aes_string(fill = value_column), color = "white") +
        coord_fixed(1.3) +
        theme_minimal()
    } else if(input$geo_level == 'county') {
      # US Counties map (requires state information)
      if('state' %in% names(geo_info())) {
        state_column <- geo_info()[['state']]
        df_map <- df %>%
          mutate(region = tolower(get(state_column)),
                 subregion = tolower(get(geo_column)))
        counties <- map_data("county")
        merged_data <- left_join(counties, df_map, by = c("region", "subregion"))
        ggplot(merged_data, aes(long, lat, group = group)) +
          geom_polygon(aes_string(fill = value_column), color = "white") +
          coord_fixed(1.3) +
          theme_minimal()
      } else {
        plot.new()
        text(0.5, 0.5, "County mapping requires 'state' information.")
      }
    } else {
      # No mappable geographical data found
      plot.new()
      text(0.5, 0.5, "No mappable geographical data found.")
    }
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
