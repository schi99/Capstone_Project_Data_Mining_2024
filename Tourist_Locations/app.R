# Tourism_Map ------------------------------------------------------------

# Preparation
library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(geosphere)
library(magrittr)


# Load Data


attractions <- readRDS("attraction_data.rds")
offers <- readRDS("offers_data.rds")
destinations <- readRDS("destination_data.rds")
gemeinde_df <- readRDS("gemeinde.rds")


# Attractions_df --------------------------------------------------


attraction_df <- data.frame(
  name = character(),
  abstract = character(),
  latitude = numeric(),
  longitude = numeric(),
  url = character(),
  stringsAsFactors = FALSE
)


j <- 1
i <- 1

repeat {
  # Check if we are within bounds of the list
  if (j == (length(attractions)-1)) {
    
    # Dirty_Fix_Because_no_more_Api_calls -------------------------------------
    
    break
  }
  
  
  
  if (i == length(attractions[[j]]$data)) {
    j <- j + 1
    i <- 1
  }
  
  
  
  # Extract values, ensuring they exist
  current_attraction <- attractions[[j]]$data[[i]]
  
  if (!is.null(current_attraction$name) && !is.null(current_attraction$geo$latitude) &&
      !is.null(current_attraction$geo$longitude)) {
    
    name <- current_attraction$name
    abstract <- ifelse(is.null(current_attraction$abstract), "", current_attraction$abstract)
    latitude <- current_attraction$geo$latitude
    longitude <- current_attraction$geo$longitude
    url <- ifelse(is.null(current_attraction$url), "", current_attraction$url)  # Assuming 'url' field exists
    
    # Create a single-row data frame
    row_df <- data.frame(
      name = name, 
      abstract = abstract, 
      latitude = latitude, 
      longitude = longitude, 
      url = url, 
      stringsAsFactors = FALSE
    )
    
    # Append the row to the main data frame
    attraction_df <- rbind(attraction_df, row_df)
  } else {
    # Print error message for debugging
    print(paste("Missing data for destination at index", i))
  }
  
  i <- i + 1
  print(i)
}


# Offers_df ---------------------------------------------------------------

offers_df <- data.frame(
  name = character(),
  abstract = character(),
  latitude = numeric(),
  longitude = numeric(),
  url = character(),
  stringsAsFactors = FALSE
)


j <- 1
i <- 1

repeat {
  # Check if we are within bounds of the list
  if (j == length(offers)) {
    break
  }
  
  if (i == length(offers[[j]]$data)) {
    j <- j + 1
    i <- 1
  }
  
  # Extract values, ensuring they exist
  current_offer <- offers[[j]]$data[[i]]
  
  if (!is.null(current_offer$name) && !is.null(current_offer$geo$latitude) &&
      !is.null(current_offer$geo$longitude)) {
    
    name <- current_offer$name
    abstract <- ifelse(is.null(current_offer$abstract), "", current_offer$abstract)
    latitude <- current_offer$geo$latitude
    longitude <- current_offer$geo$longitude
    url <- ifelse(is.null(current_offer$url), "", current_offer$url)  # Assuming 'url' field exists
    
    # Create a single-row data frame
    row_df <- data.frame(
      name = name, 
      abstract = abstract, 
      latitude = latitude, 
      longitude = longitude, 
      url = url, 
      stringsAsFactors = FALSE
    )
    
    # Append the row to the main data frame
    offers_df <- rbind(offers_df, row_df)
  } else {
    # Print error message for debugging
    print(paste("Missing data for destination at index", i))
  }
  
  i <- i + 1
  print(i)
}



# Destination_df --------------------------------------------------------

destinations_df <- data.frame(
  name = character(),
  abstract = character(),
  latitude = numeric(),
  longitude = numeric(),
  url = character(),
  stringsAsFactors = FALSE
)

# Initialize an empty data frame
destinations_df <- data.frame(
  name = character(),
  abstract = character(),
  latitude = numeric(),
  longitude = numeric(),
  url = character(),
  stringsAsFactors = FALSE
)


j <- 1
i <- 1

repeat {
  # Check if we are within bounds of the list
  if (j == length(destinations)) {
    break
  }
  
  if (i == length(destinations[[j]]$data)) {
    j <- j + 1
    i <- 1
  }
  
  # Extract values, ensuring they exist
  current_destination <- destinations[[j]]$data[[i]]
  
  if (!is.null(current_destination$name) && !is.null(current_destination$geo$latitude) &&
      !is.null(current_destination$geo$longitude)) {
    
    name <- current_destination$name
    abstract <- ifelse(is.null(current_destination$abstract), "", current_destination$abstract)
    latitude <- current_destination$geo$latitude
    longitude <- current_destination$geo$longitude
    url <- ifelse(is.null(current_destination$url), "", current_destination$url)  # Assuming 'url' field exists
    
    # Create a single-row data frame
    row_df <- data.frame(
      name = name, 
      abstract = abstract, 
      latitude = latitude, 
      longitude = longitude, 
      url = url, 
      stringsAsFactors = FALSE
    )
    
    # Append the row to the main data frame
    destinations_df <- rbind(destinations_df, row_df)
  } else {
    # Print error message for debugging
    print(paste("Missing data for destination at index", i))
  }
  
  i <- i + 1
  print(i)
}


clean_destination_df <- destinations_df %>%
  filter(destinations_df[2] != "")


gemeinde_df <- gemeinde_df %>%
  mutate(name = str_remove_all(name, "[\\(\\)]"))




gemeinde_loc <- inner_join(gemeinde_df, destinations_df, by = "name")
gemeinde_loc_2 <- gemeinde_loc


# APP_UI ------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Tourism Map of Switzerland"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset", choices = c("Attractions", "Destinations", "Offers")),
      selectInput("gemeinde", "Select Gemeinde", choices = gemeinde_loc$name, multiple = FALSE),
      selectInput("postal_code", "Select Postal Code", choices = gemeinde_loc$postal_code),
      numericInput("radius", "Select Radius (km)", value = 20, min = 1)
    ),
    mainPanel(
      leafletOutput("mymap"),
      br(),
      h3("List of Attractions"),
      verbatimTextOutput("attraction_list")  # Output for the list of destinations
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (!exists("gemeinde_loc")) return(NULL)  # Check if gemeinde_loc exists
    
    selected_postal_code <- input$postal_code
    selected_gemeinde <- gemeinde_loc$name[gemeinde_loc$postal_code == selected_postal_code]
    
    if (!is.null(selected_gemeinde) && length(selected_gemeinde) > 0) {
      updateSelectInput(session, "gemeinde", selected = selected_gemeinde)
    }
  })
  
  filtered_data <- reactive({
    dataset <- switch(input$dataset,
                      "Attractions" = attraction_df,
                      "Destinations" = clean_destination_df,
                      "Offers" = offers_df)
    
    if (!exists("gemeinde_loc")) return(NULL)  # Check if gemeinde_loc exists
    
    selected_gemeinde <- gemeinde_loc %>%
      dplyr::filter(name == input$gemeinde)
    if (nrow(selected_gemeinde) == 0) {return(NULL)}
    
    
    distances <- distm(
      c(selected_gemeinde$longitude, selected_gemeinde$latitude), 
      dataset[, c("longitude", "latitude")], 
      fun = distHaversine
    )
    
    # Convert the radius from kilometers to meters
    radius_meters <- input$radius * 1000
    
    # Filter the dataset based on the selected radius
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
  output$attraction_list <- renderPrint({
    data <- filtered_data()
    if (is.null(data)) return(NULL)
    
    dataset <- data$dataset_within_radius
    # Print the list of titles and URLs of attractions
    lapply(seq_len(nrow(dataset)), function(i) {
      cat("Title:", dataset$name[i], "\n")
      cat("URL:", dataset$url[i], "\n\n")
    })
  })
}



shinyApp(ui, server)








