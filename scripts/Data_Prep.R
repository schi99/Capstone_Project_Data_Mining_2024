
# Data Preparation --------------------------------------------------------

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(geosphere)
library(magrittr)


# Load Data

attractions <- readRDS("data/attraction_data.rds")
offers <- readRDS("data/offers_data.rds")
destinations <- readRDS("data/destination_data.rds")
gemeinde_df <- readRDS("data/gemeinde.rds")


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
  if (j == (length(attractions))) {
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




gemeinde_loc <- as.data.frame(inner_join(gemeinde_df, destinations_df, by = "name"))
gemeinde_loc_2 <- gemeinde_loc


saveRDS(object = destinations_df,file = here::here("data", "destination_df.rds"))
saveRDS(object = gemeinde_loc, file = here::here("data", "gemeinde_loc.rds"))
saveRDS(object = clean_destination_df, file = here::here("data", "clean_destination_df.rds"))
saveRDS(object = offers_df, file = here::here("data", "offers_df.rds"))
saveRDS(object = attraction_df, file = here::here("data", "attractions_df.rds"))
