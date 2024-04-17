

# Setup -------------------------------------------------------------------

# Load the newscatcheR package
library(newscatcheR)

# Set your API key
api_key <- "" # add key here


# Extract Sites of interest -------------------------------------------------------

news_sites <- c("bbc.co.uk", "dailymail.co.uk",
                "express.co.uk", "mirror.co.uk")

top_sites <- filter_urls(language = "en") %>%
  filter(clean_country == "GB" | clean_country == "None") %>%
  filter(clean_url %in% news_sites) %>% 
  filter(topic_unified != "entertainment")



# Extract all headlines today ---------------------------------------------

all_headlines <- news_sites %>% 
  map(~ get_headlines(.)) %>%
  bind_rows() %>% 
  rename(headline = 1)

