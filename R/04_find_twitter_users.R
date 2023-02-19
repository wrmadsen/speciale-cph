# Find Twitter users that are either bots or live in CAR
# Then create dataset to scrape their Tweets

# Find Twitter users ----

## Identify users that are within CAR ----

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
intersections <- st_intersects(twitter_keywords_points, gadm_simp)

# Create region-row-number look-up
region_look_up <- gadm_simp %>%
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

# Filter users within CAR
get_tweets_users <- twitter_keywords_points_w_regions %>%
  filter(country == "Central African Republic") %>%
  distinct(user)

get_tweets_users


## Identify bots ----
# Need followers features to put into https://github.com/mkearney/tweetbotornot fast model ??
# Name
# Screen name
# Location
# Description (bio)
twitter_keywords %>% distinct(user) %>% nrow()*200

library(devtools)
library(botcheck)

# Rapid API key
rapid_api_key = "4fc6142eb2msh41f020494846596p1655bbjsnf0b666eb6bd4"

# Twitter app info
consumer_key = "xxxxxxxxxxxxxxx"
consumer_secret = "xxxxxxxxxxxxxxx"
access_token = "xxxxxxxxxxxxxxx"
access_secret = "xxxxxxxxxxxxxxx"

# Create dataset to get Tweets by usernames -----
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


