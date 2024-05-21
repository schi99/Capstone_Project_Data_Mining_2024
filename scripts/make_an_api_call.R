# Call_the_API -----------------------------------------------------------

library(here)
library(tidyverse)
library(httr)
library(rvest)
library(httr2)

# General_information_and_setup -------------------------------------------

# The data gathered by this script is from MySwitzerland.com. 
# Add your E-Mail to be polite (You can use the .Renviron-file) or add it in the
# code

api_key <- Sys.getenv(x="Key")
my_email <- Sys.getenv(x= "Mail")

library(httr)
library(jsonlite)
library(aws.signature)

library(httr)
library(jsonlite)

endpoint_link <- "https://opendata.myswitzerland.io/v1/destination"

get_destination_data <- function(api_key, my_email) {
  # Initialize an empty list to store results
  all_results <- list()
  page <- 1
  
  repeat {
    # Define the parameters for the API call, including the page number
    params <- list(page = page)
    
    # Make the API call with the API key in the Authorization header
    response <- GET(url = endpoint_link, 
                    add_headers(`From` = my_email, 
                                `User-Agent` = R.Version()$version.string,
                                `Authorization` = paste("Bearer", api_key)),
                    query = params)
    
    # Check if the API call was successful
    if (response$status_code == 200) {
      # Parse the response
      data <- content(response, as = "parsed", type = "application/json")
      
      # Extract the relevant information
      results <- data$response$results
      
      # Add the results to the list
      all_results <- c(all_results, results)
      
      # Check if there are more pages
      if (length(results) == 0) {
        break
      } else {
        page <- page + 1
      }
    } else if (response$status_code == 403) {
      # Print an error message for 403 status code
      message("Error: Forbidden. API call failed with status code 403. Please check your API key and permissions.")
      
      # Print the response for debugging
      print(content(response, as = "text"))
      
      # Return NULL
      return(NULL)
    } else {
      # Print an error message for other status codes
      message("Error: API call failed with status code ", response$status_code)
      
      # Print the response for debugging
      print(content(response, as = "text"))
      
      # Return NULL
      return(NULL)
    }
  }
  
  # Return the collected results
  return(all_results)
}

all_results <- get_destination_data(api_key, my_email)
print(all_results)


