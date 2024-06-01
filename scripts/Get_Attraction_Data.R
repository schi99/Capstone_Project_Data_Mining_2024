
library(httr2)
library(httr)

# Note: Before runing this code make sure to haven an .Renviron-file with your
# API-Key and your E-Mail Address.



# Define the GET-Function -------------------------------------------------

get_attraction_data <- function(api_key, 
                                 my_email,
                                 language = "en",
                                 page = 0,
                                 hitsPerPage = 30,
                                 translateFacets = TRUE,
                                 expandData = TRUE)
{
  # Construct the URL based on the provided parameters
  
  url <- paste0("https://opendata.myswitzerland.io/v1/attractions?lang=", language, "&page=", page, "&hitsPerPage=", hitsPerPage)
  if (translateFacets) {
    url <- paste0(url, "&facets.translate=true")
  }
  if (expandData) {
    url <- paste0(url, "&expand=true")
  }
  
  # Make the API call with the API key in the headers
  response <- httr::GET(
    url,
    httr::add_headers(
      accept = "application/json",
      `x-api-key` = api_key,
      `From` = my_email,
      `User-Agent` = R.Version()$version.string
    )
    
  )
  
  # Check if the API call was successful
  if (response$status_code == 200) {
    # Parse the response
    data <- httr::content(response, as = "parsed", type = "application/json")
    print("All good :)")
    return(data)
  } else {
    # Print an error message for other status codes
    message("Error: API call failed with status code ", response$status_code)
    print(httr::content(response, as = "text"))
    return(NULL)
  }
}



# Fetch attraction data  -------------------------------------------------



api_key <- Sys.getenv(x="Key")
my_email <- Sys.getenv(x="Mail")


# Test

attraction_data <- get_attraction_data(api_key, 
                                         my_email,
                                         language = "en",
                                         page = 0,
                                         hitsPerPage = 20,
                                         translateFacets = TRUE)
print(attraction_data)

# Perparation

page <- 0
all_data <- list()


#Get all data

repeat {
  
  Sys.sleep(rnorm(1 ,mean = 1, sd = 0.2))
  
  attraction_data <- get_attraction_data(api_key, 
                                           my_email,
                                           language = "en",
                                           page = page,
                                           hitsPerPage = 20,
                                           translateFacets = TRUE)
  
  if (page == attraction_data$meta$page$totalPages) {
    print("done")
    break
  }
  
  all_data <- append(all_data, list(attraction_data))
  
  
  page <- page + 1
  
  print(page)
}


saveRDS(object = all_data, file = here("data/attraction.rds"))

