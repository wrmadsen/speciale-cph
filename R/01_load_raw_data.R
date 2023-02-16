# Load raw data

# Radio data -----
radio_raw <- read_csv("data-raw/Radio/articles_radiondekeluka.csv")

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

