# Scrape Tweets from Twitter

# Create data to get Tweets by keywords -----
# Keysword to scrape
search_generic <- c(#"poutine touadera",
  #"centrafrique poutine",
  #"centrafrique wagner",
  #"centra"
  #"wagner touadera",
  "centrafrique",
  "centrafricaine")

"(centrafrique OR centrafricaine) AND poutine"

# Phrases pro-Russian or pro-Touadera
search_disinfo_pro_russ <- c("russie fournit",
                             "remercier touadera",
                             "instructeurs")

# Phrases anti-Russian, pro-France
search_disinfo_anti_russ <- c("mercenaires",
)

# Add lang:fr and centrafrique
search_generic <- paste0("lang:fr ", search_generic)

# Create tibble
get_tweets_keywords <- search_generic %>%
  tibble("keyword" = .) %>%
  mutate(start = as.Date("2014-01-01"),
         end = as.Date("2019-02-01"))

# For each keyword, create a number of periods to scrape
get_tweets_keywords <- get_tweets_keywords %>%
  rowwise() %>%
  mutate(since = list(seq.Date(start,
                               end,
                               by = "1 month"))
  ) %>%
  tidyr::unnest(since) %>%
  transmute(keyword,
            limit = 100,
            since , # get start time (since)
            until = since + months(6) - days(1) # get end time (to)
  )

# Save get data as csv to use in Python
write_csv(get_tweets_keywords, "data-raw/Twitter/get_tweets_keywords.csv")

