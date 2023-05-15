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

# Add sentiment to thetas (topics) object ----
#load("output/master_dt_thetas_long.Rdata")

thetas_that_will_be_joined <- master_dt_thetas_long %>%
  select(sub_group, document, date, month, topic_no, topic_name, topic_proportion) %>%
  filter(year(date) >= 2020)

master_sentiment_joined <- left_join(thetas_that_will_be_joined,
                                     sentiment_per_document %>% select(-c(text, text_nchar)),
                                     by = "document")

# # Pivot longer
# master_sentiment_long <- master_sentiment_joined %>%
#   pivot_longer(cols = c(15:44), names_to = "topic", values_to = "topic_share") %>%
#   select(-c(text_nchar, url, spike_no, spike_text, spike_binary))

# Subset topics
# top_topics_no is created in the 14th script
# topic_to_filter is created in the 14th script
master_sentiment_joined <- master_sentiment_joined %>%
  filter(topic_name %in% topic_to_filter)

# Calculate sentiment ----

## Calculate share of binary per month -----
# master_sentiment_share <- master_sentiment_joined %>%
#   # First choose topic per document
#   group_by(document) %>%
#   filter(topic_proportion == max(topic_proportion)) %>%
#   # Calculate monthly share
#   group_by(sub_group, month, topic_name) %>%
#   summarise(n = n()) %>%
#   arrange(sub_group, month, topic_name) %>%
#   group_by(sub_group, month, topic_name) %>%
#   mutate(total = sum(n),
#          share = n/total)

## Calculate raw score per document ----
master_sentiment_raw <- master_sentiment_joined

summary(master_sentiment_raw$topic_proportion)

master_sentiment_raw_sub <- master_sentiment_raw %>%
  # Choose only very majority topics
  #filter(topic_proportion > 0.01) #%>%
  # Or, for each document, choose largest topic by share
  group_by(document) %>%
  slice_max(order_by = topic_proportion, n = 1)

## Calculate relative sentiment -----





