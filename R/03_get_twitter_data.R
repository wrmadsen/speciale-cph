# Scrape Tweets from Twitter

# Create data to get Tweets by keywords -----
# Keysword to scrape
key_words_to_scrape <- c("poutine touadera")

# Create tibbl
get_tweets_keywords <- key_words_to_scrape %>%
  tibble("keyword" = .) %>%
  mutate(start = as.Date("2018-01-01"),
         end = as.Date("2023-02-01"))

# For each keyword, create a number of periods to scrape
get_tweets_keywords <- get_tweets_keywords %>%
  rowwise() %>%
  mutate(since = list(seq.Date(start,
                              end,
                              by = "12 month"))
  ) %>%
  tidyr::unnest(since) %>%
  transmute(keyword,
            limit = 10,
            since , # get start time (since)
            until = since + months(6) - days(1) # get end time (to)
  )

# Save get data as csv to use in Python
write_csv(get_tweets_keywords, "data-raw/Twitter/get_tweets_keywords.csv")

