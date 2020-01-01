# this script *must* be run from a call by index.Rmd, where the important
#    variable "address" is declared

# load necessary libraries/files
library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")
library("stringr")
source("api-keys.R")

# extract state from address (declared in index.Rmd)
#   will be successful for addresses where 2-letter state abbreviation is given
#   following a comma
state <- str_extract(
  str_extract(toupper(address), ", [A-Z][A-Z],?"), "[A-Z][A-Z]"
)

# this section creates values for the charts on house reps by address
# create GET request for house reps for a given address
base_uri <- "https://api.propublica.org/congress/v1"
endpoint <- "/members"
query <- paste0("/house/", state, "/current.json")
resource_uri <- paste0(base_uri, endpoint, query)
response <- GET(resource_uri, add_headers("X-API-Key" = propublica_key))

# format received content
parsed_data <- fromJSON(content(response, "text"))
congress_info <- flatten(parsed_data$results)

# make reps by gender chart
congress_info_by_gender <- congress_info %>%
  group_by(gender) %>%
  summarise(num = n())
gender_chart <- ggplot(congress_info_by_gender) +
  geom_col(mapping = aes(x = gender, y = num)) +
  labs(
    title = "Representatives by Gender",
    x = "Gender",
    y = "# of Representatives"
  ) +
  coord_flip()

# make reps by party chart
congress_info_by_party <- congress_info %>%
  group_by(party) %>%
  summarise(num = n())
party_chart <- ggplot(congress_info_by_party) +
  geom_col(mapping = aes(x = party, y = num)) +
  labs(
    title = "Representatives by Party",
    x = "Party",
    y = "# of Representatives"
  ) +
  coord_flip()

# this section creates values for the specific House member report
# create GET request for specific House member
member <- congress_info$id[1]

base_uri <- "https://api.propublica.org/congress/v1"
endpoint <- "/members"
query <- paste0("/", member, ".json")
resource_uri <- paste0(base_uri, endpoint, query)
response <- GET(resource_uri, add_headers("X-API-Key" = propublica_key))
parsed_data <- fromJSON(content(response, "text"))
rep_details <- flatten(parsed_data$results)

# create request for that member's voting patterns
base_uri <- "https://api.propublica.org/congress/v1"
endpoint <- "/members"
query <- paste0("/", member, "/votes.json")
resource_uri <- paste0(base_uri, endpoint, query)
response <- GET(resource_uri, add_headers("X-API-Key" = propublica_key))
parsed_data <- fromJSON(content(response, "text"))
rep_votes <- flatten(parsed_data$results$votes[[1]])

# calculate values for report
# store representative's name
rep_name <- congress_info %>%
  filter(id == member) %>%
  pull(name)

# calculate representative's age
rep_DOB <- as.POSIXlt(rep_details$date_of_birth)
current_date <- as.POSIXlt(Sys.Date())
rep_age <- current_date$year - rep_DOB$year
if (current_date$mon < rep_DOB$mon | (current_date$mon < rep_DOB$mon &
                                      current_date$mday < rep_DOB$mday)) {
  rep_age <- rep_age - 1
}

# store link to representative's twitter
twitter <- paste0(
  "[", rep_details$twitter_account, "](https://twitter.com/",
  rep_details$twitter_account, ")"
)

# calculate frequency of agreement with vote (as a percent)
rep_votes <- mutate(rep_votes, agreed = F)
rep_votes$agreed[(rep_votes$result == c("Passed", "Agreed to")) &
                   (rep_votes$position == "Yes")] <- T
rep_votes$agreed[(rep_votes$result == "Failed") &
                   (rep_votes$position == "No")] <- T
agree_percent <- sum(rep_votes$agreed) / length(rep_votes$agreed) * 100