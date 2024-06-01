
# Df_Gemeindenamen_erstellen ----------------------------------------------

library(rvest)
library(tidyverse)
library(janitor)


my_email <- Sys.getenv("Mail")

require(httr)

# Create a function `download_politely(from_url, to_html)` to download politely
# the web page:
download_politely <- function(from_url, to_html, my_email, my_agent = R.Version()$version.string) {
  
  require(httr)
  
  # Check that arguments are of the expected type:
  stopifnot(is.character(from_url))
  stopifnot(is.character(to_html))
  stopifnot(is.character(my_email))
  
  # GET politely
  simps_req <- httr::GET(url = from_url, 
                         add_headers(
                           From = my_email,   # Provides email to let webmaster get in touch
                           `User-Agent` = R.Version()$version.string     # Adds infos about our software
                         )
  )
  # If status == 200, extract content and save to a file:
  if (httr::http_status(simps_req)$message == "Success: (200) OK") {
    bin <- content(simps_req, as = "raw")
    writeBin(object = bin, con = to_html)
  } else {
    # Else, print a message to understand that we have a problem
    cat(":(")
  }
}

# Call the customized function:

download_politely(from_url = "https://de.wikipedia.org/wiki/Liste_Schweizer_Gemeinden", 
                  to_html = here::here("raw_data/gemeinden.html"), 
                  my_email = "my@email.com")


# Extract the titles:
page <- read_html(here::here("raw_data/gemeinden.html")) 

gemeinden_l <- page %>%
  rvest::html_nodes("table.wikitable") %>%
  rvest::html_table(fill = TRUE) 

gemeinde_df <- gemeinden_l[[1]] %>% 
  clean_names() 

gemeinde_df <- gemeinde_df %>%
  select(offizieller_gemeindename, bfs_nr) %>%
  rename(name = offizieller_gemeindename,
         "postal_code" = bfs_nr)
  

saveRDS(object = gemeinde_df, file = here::here("raw_data" , "gemeinde.rds"))
  
