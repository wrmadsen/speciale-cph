# Load raw data

# Radio data -----
read_bind_radio <- function(files){
  
  name_of_radio <- gsub("data\\-raw\\/Radio\\/radio_articles\\_|\\.csv|_.+|\\d+", "", files)
  
  read_csv(files) %>%
    clean_names() %>%
    mutate(name = name_of_radio,
           name = recode(name,
                       "radiondekeluka" = "Radio Ndeke Luka"))
  
}

radio_raw <- list.files(pattern = "radio_articles.+\\.csv", recursive = TRUE) %>%
  map_df(read_bind_radio)

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
gadm_sub_raw <- list.files(pattern = "1.shp", recursive = TRUE) %>%
  map_df(~read_sf(.))

