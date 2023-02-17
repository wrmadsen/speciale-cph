# Find Twitter users

# Identify users that are within Mali ----
# Mali max lat = 25
# Mali min lat = 10
# Mali max long = 5
# Mali min long = -13
# users_mali <- twitter_keywords %>%
#   filter(!is.na(longitude)) %>%
#   filter(longitude > -13 & longitude < 5 & latitude > 10 & latitude < 25) %>%
#   distinct(user)

## Convert tweets with points to sf
twitter_keywords_points <- twitter_keywords %>%
  filter(!is.na(longitude)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  mutate(has_point = TRUE)

twitter_keywords_no_points <- twitter_keywords %>%
  filter(is.na(longitude)) %>%
  select(-c(longitude, latitude)) %>%
  mutate(has_point = FALSE)

# Find intersections between points and polygons
intersections <- st_intersects(twitter_keywords_points, boundaries_subnational_simp)

# Create region-row-number look-up
region_look_up <- boundaries_subnational_simp %>%
  as.data.frame() %>%
  transmute(region_id = row_number(),
            country,
            region_1,
            engtype_1,
            #region_2,
            #engtype_2
  )

# Add regions to tweets with points
twitter_keywords_points_w_regions <- twitter_keywords_points %>%
  mutate(region_id = as.integer(intersections)
  ) %>%
  left_join(., region_look_up, by = "region_id")

# Filter users within Mali
get_tweets_users <- twitter_keywords_points_w_regions %>%
  filter(country == "Mali") %>%
  distinct(user)

# Identify bots ----


# Create data to get Tweets by usernames -----
get_tweets_users <- get_tweets_users %>%
  mutate(start = as.Date("2018-01-01"),
         end = as.Date("2023-02-01"))

# For each user, create a number of periods to scrape
get_tweets_users <- get_tweets_users %>%
  rowwise() %>%
  mutate(since = list(seq.Date(start,
                               end,
                               by = "6 month"))
  ) %>%
  tidyr::unnest(since) %>%
  transmute(user,
            limit = 10,
            since , # get start time (since)
            until = since + months(6) - days(1) # get end time (to)
  )


# Save users in a csv to scrape Tweets per user ----
#write_csv(get_tweets_users, "data-raw/Twitter/get_tweets_users.csv")


