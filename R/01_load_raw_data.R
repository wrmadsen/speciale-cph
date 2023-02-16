# Load raw data

# Twitter data ----
# By keywords
twitter_keywords_raw <- list.files(pattern = "twitter_keyword", recursive = TRUE) %>%
  map_df(~read_csv(.))

# By user

# GADM data ----
gadm_sub_raw <- list.files(pattern = "1.shp", recursive = TRUE) %>%
  map_df(~read_sf(.))

