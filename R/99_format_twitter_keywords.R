# Format Twitter keywords data

# Tidy ----
twitter_keywords <- twitter_keywords_raw %>%
  clean_names() %>%
  # Select batch of downloads
  filter(id == "lang:fr centrafrique") %>%
  # format coordinates
  mutate(coordinates = gsub("Coordinates\\(longitude\\=| latitude\\=|\\)", "", coordinates),
         longitude = gsub("\\,.+", "", coordinates) %>% as.double(),
         latitude = gsub(".+\\,", "", coordinates) %>% as.double()) %>%
  transmute(user,
            group = "Twitter",
            sub_group = id,
            date = as.Date(date_created),
            week = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
            month = floor_date(date, unit = "month"),
            year = year(date),
            text = tweet,
            text_nchar = nchar(text),
            likes = number_of_likes,
            longitude, latitude
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on text
  distinct(text, .keep_all = TRUE)

twitter_keywords #%>% filter(grepl("#", text)) %>% view()

twitter_master <- twitter_keywords








