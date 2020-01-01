# this script *must* be run from a call by index.Rmd, where the important
#    variable "address" is declared

# load necessary libraries/files
library("httr")
library("jsonlite")
library("dplyr")
library("tidyr")
source("api-keys.R")

# create GET request for representatives of a specified address
base_uri <- "https://www.googleapis.com/civicinfo/v2"
endpoint <- "/representatives"
resource_uri <- paste0(base_uri, endpoint)
query_params <- list("key" = google_key, address = address)
response <- GET(resource_uri, query = query_params)

# format received content
parsed_data <- fromJSON(content(response, "text"))
offices <- parsed_data$offices
officials <- parsed_data$officials

# join offices to officials by index
num_to_rep <- unlist(lapply(parsed_data$offices$officialIndices, length))
expanded <- offices[rep(row.names(offices), num_to_rep), ]
officials <- officials %>% mutate(index = row_number() - 1)
expanded <- expanded %>%
  mutate(index = row_number() - 1) %>%
  rename(position = name)
civic_info <- merge(expanded, officials, by = "index")

# format civic_info with markdown to make a nice-looking table
civic_info <- civic_info %>%
  mutate(Name = paste0("[", name, "](", urls, ")")) %>%
  mutate(Email = paste0("[", emails, "](mailto:", emails, ")")) %>%
  mutate(Phone = phones) %>%
  mutate(Photo = paste0("![photo](", photoUrl, ")")) %>%
  select(Name, Position = position, Party = party, Email, Phone, Photo)
civic_info$Email[civic_info$Email == "[NULL](mailto:NULL)"] <- "Not available"
civic_info$Photo[civic_info$Photo == "![photo](NA)"] <- "Not available"