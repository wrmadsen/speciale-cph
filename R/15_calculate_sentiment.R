# Calculate sentiment

# Join tokens with sentiment dictionary -----
master_sentiment <- left_join(master_tokens_tbl, afinn, by = c("token" = "token")) %>%
  select(document, orient, group, sub_group, text_nchar, text, token, date, week, year, month, afinn)

test <- master_sentiment %>%
  filter(text_nchar < 400) %>%
  slice_sample(n = 1000) %>%
  select(sub_group, token, afinn, text)

# Share of tokens with a sentiment score
master_sentiment %>%
  group_by(category = if_else(is.na(afinn), "No match", "Matched")) %>%
  summarise(n = n()) %>%
  mutate(share = (n/sum(n)*100) %>% round)

# Check some without
#master_sentiment %>% filter(is.na(score)) %>% select(token)

# Check most popular tokens and their sentiment per media
master_sentiment %>%
  filter(!is.na(afinn)) %>%
  group_by(sub_group, token, afinn) %>%
  summarise(n = n()) %>%
  group_by(sub_group) %>%
  slice_min(order_by = n, n = 20) %>%
  left_join(., popular_tokens) %>%
  mutate(order = row_number()) %>%
  select(sub_group, non_stemmed, afinn)

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  group_by(document, text, text_nchar) %>%
  summarise(afinn_document = mean(afinn, na.rm = TRUE)) %>%
  # ungroup() %>%
  # mutate(binary_document = if_else(afinn_document > 0.5, "Positive", "Negative")) %>%
  ungroup() %>%
  select(document, afinn_document, text, text_nchar)

# See some
sentiment_per_document %>%
  filter(text_nchar < 400) #%>% view

# Add thetas (topics) ----
thetas_that_will_be_joined <- master_dt_thetas_long %>%
  select(sub_group, document, date, month, year, topic_no, topic_name, topic_proportion, text_nchar, text, url) %>%
  filter(year(date) >= 2020)

master_sentiment_joined <- left_join(thetas_that_will_be_joined,
                                     sentiment_per_document %>% select(-c(text, text_nchar)),
                                     by = "document") %>%
  relocate(text_nchar, text, url, .after = last_col())

# Subset topics
# top_topics_no is created in the 14th script
# topic_to_filter is created in the 14th script
# master_sentiment_joined <- master_sentiment_joined %>%
#   filter(topic_name %in% top_topics_name)

# Calculate sentiment ----

## Calculate raw score per document ----
master_sentiment_raw <- master_sentiment_joined

summary(master_sentiment_raw$topic_proportion)

master_sentiment_raw_sub <- master_sentiment_raw %>%
  group_by(document) %>%
  slice_max(order_by = topic_proportion, n = 10) #%>%
  # ungroup() %>%
  # filter(topic_proportion > 0.174)

# Summary after slice_max -----

# Summary
summary(master_sentiment_raw_sub$topic_proportion)

# Number of observations
nrow(master_sentiment_raw) # pre
nrow(master_sentiment_raw_sub) # post
nrow(master_sentiment_raw_sub)/4/20 # per media and topic



