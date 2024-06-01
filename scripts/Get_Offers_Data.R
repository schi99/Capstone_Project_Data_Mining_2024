
# Get_Offers_Data ---------------------------------------------------------

get_offers_data <- function(api_key, 
                                my_email,
                                language = "en",
                                page = 0,
                                hitsPerPage = 30,
                                translateFacets = TRUE,
                                expandData = TRUE)
{
  # Construct the URL based on the provided parameters
  
  url <- paste0("https://opendata.myswitzerland.io/v1/offers?lang=", language, "&page=", page, "&hitsPerPage=", hitsPerPage)
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



# Fetch offers data  -------------------------------------------------



api_key <- Sys.getenv(x="Key")
my_email <- Sys.getenv(x="Mail")


# Test

offers_data <- get_offers_data(api_key, 
                                       my_email,
                                       language = "en",
                                       page = 0,
                                       hitsPerPage = 20,
                                       translateFacets = TRUE)
print(offers_data)

# Perparation

page <- 0
all_data <- list()


#Get all data

repeat {
  
  Sys.sleep(rnorm(1 ,mean = 1, sd = 0.2))
  
  offers_data <- get_offers_data(api_key, 
                                          my_email,
                                          language = "en",
                                          page = page,
                                          hitsPerPage = 20,
                                          translateFacets = TRUE)
  
  if (page == offers_data$meta$page$totalPages) {
    print("done")
    break
  }
  
  all_data <- append(all_data, list(offers_data))
  
  
  page <- page + 1
  
  print(page)
}


saveRDS(object = all_data, file = here::here("raw_data","offers_data.rds"))



