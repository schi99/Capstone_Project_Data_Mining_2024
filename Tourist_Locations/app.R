# Tourism_Map ------------------------------------------------------------

# Preparation
library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(geosphere)
library(magrittr)


destination_df <- as.data.frame(readRDS(here::here("data", "destination_df.rds")))
attraction_df <- as.data.frame(readRDS(here::here("data", "attractions_df.rds")))
clean_destination_df <- as.data.frame(readRDS(here::here("data", "clean_destination_df.rds")))
gemeinde_loc <- as.data.frame(readRDS(here::here("data", "gemeinde_loc.rds")))

# User_interface ------------------------------------------------------------------

# Define UI for the app
if (!exists("gemeinde_loc")) {
  stop("The 'gemeinde_loc' dataset is not loaded.")
}

if (!"postal_code" %in% colnames(gemeinde_loc)) {
  stop("The 'gemeinde_loc' dataset does not contain a 'postal_code' column.")
}

# Define UI for the app
ui <- fluidPage(
  titlePanel("Tourism Map of Switzerland"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset", choices = c("Attractions", "Destinations")),
      selectInput("gemeinde", "Select Municipality", choices = gemeinde_loc$name, multiple = FALSE),
      selectInput("postal_code", "Select Postal Code", choices = unique(gemeinde_loc$postal_code)),
      numericInput("radius", "Select Radius (km)", value = 20, min = 1)
    ),
    mainPanel(
      leafletOutput("mymap"),
      br(),
      h3(textOutput("list_title")),  # Dynamic title for the list of attractions
      htmlOutput("attraction_list")  # Output for the list of destinations
    )
  )
)


# Server ------------------------------------------------------------------


# Define server logic for the app
server <- function(input, output, session) {
  observe({
    selected_postal_code <- input$postal_code
    selected_gemeinde <- gemeinde_loc$name[gemeinde_loc$postal_code == selected_postal_code]
    
    if (!is.null(selected_gemeinde) && length(selected_gemeinde) > 0) {
      updateSelectInput(session, "gemeinde", selected = selected_gemeinde)
    }
  })
  
  filtered_data <- reactive({
    dataset <- switch(input$dataset,
                      "Attractions" = attraction_df,
                      "Destinations" = clean_destination_df)
    
    selected_gemeinde <- gemeinde_loc %>%
      filter(name == input$gemeinde)
    if (nrow(selected_gemeinde) == 0) { return(NULL) }
    
    distances <- distm(
      c(selected_gemeinde$longitude, selected_gemeinde$latitude), 
      dataset[, c("longitude", "latitude")], 
      fun = distHaversine
    )
    
    radius_meters <- input$radius * 1000
    
    dataset_within_radius <- dataset[distances <= radius_meters, ]
    
    list(dataset_within_radius = dataset_within_radius, selected_gemeinde = selected_gemeinde)
  })
  
  output$mymap <- renderLeaflet({
    data <- filtered_data()
    if (is.null(data)) return(NULL)
    
    dataset <- data$dataset_within_radius
    selected_gemeinde <- data$selected_gemeinde
    
    leaflet(dataset) %>%
      addTiles() %>%
      setView(lng = 8.2275, lat = 46.8182, zoom = 7) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       radius = 5,
                       popup = ~paste("<b>", name, "</b><br><a href='", url, "'>", url, "</a>", 
                                      ifelse(abstract != "", paste("<br>", abstract), ""), sep = ""),
                       color = "darkred",
                       fillOpacity = 0.1) %>%
      addCircleMarkers(lng = selected_gemeinde$longitude, 
                       lat = selected_gemeinde$latitude, 
                       radius = 5, 
                       color = "black", 
                       popup = ~paste("<b>", selected_gemeinde$name, "</b>"))
  })
  
  # Generate a dynamic title based on the selected dataset
  output$list_title <- renderText({
    switch(input$dataset,
           "Attractions" = "List of Attractions",
           "Destinations" = "List of Destinations")
  })
  
  output$attraction_list <- renderUI({
    data <- filtered_data()
    if (is.null(data)) return(NULL)
    
    dataset <- data$dataset_within_radius
    
    # Filter out rows with null name or url
    dataset <- dataset %>% filter(!is.na(name) & !is.na(url) & name != "" & url != "")
    
    # Create a list of HTML elements for each attraction
    attraction_list <- lapply(seq_len(nrow(dataset)), function(i) {
      tags$div(
        tags$h4(dataset$name[i]),
        tags$a(href = dataset$url[i], dataset$url[i]),
        tags$br()
      )
    })
    
    # Return the list as HTML
    do.call(tagList, attraction_list)
  })
}

# Run the Shiny app
shinyApp(ui, server)
