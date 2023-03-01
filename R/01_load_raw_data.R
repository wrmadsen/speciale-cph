# Load raw data

# Radio data -----
read_bind_radio <- function(files){
  
  name_of_radio <- gsub("data\\-raw\\/Radio\\/radio_articles\\_|\\.csv|_.+|\\d+", "", files)
  
  read_csv(files) %>%
    clean_names() %>%
    mutate(sub_group = name_of_radio,
           sub_group = recode(sub_group,
                              "radiondekeluka" = "Radio Ndeke Luka",
                              "radiolengo" = "Radio Lengo Songo",
                              "radioreseau" = "Reseau des journalistes",
           ))
  
}

radio_raw <- list.files(pattern = "radio_articles.+\\.csv", recursive = TRUE) %>%
  map_df(read_bind_radio)

# Spike periods data
spike_periods <- read_csv("output/spike_periods.csv")

# Twitter data ----
# By keywords
read_bind_twitter <- function(files){
  
  id <- gsub("data\\-raw\\/Twitter\\/twitter_keyword\\_|\\.csv|_.+", "", files)
  
  read_csv(files) %>%
    clean_names() %>%
    mutate(id = id)
  
}

twitter_keywords_raw <- list.files(pattern = "twitter_keyword_.+\\.csv", recursive = TRUE) %>%
  map_df(read_bind_twitter)

# By user

# GADM data ----
# https://gadm.org/download_country.html
gadm_raw <- list.files(pattern = "1.shp", recursive = TRUE) %>%
  map_df(~read_sf(.))


# FEEL sentiment dictionary ----
feel_raw <- read_delim("data-raw/Sentiment/FEEL.csv", delim = ";")


