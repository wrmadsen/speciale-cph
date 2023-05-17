# Calculate sentiment

# Join tokens with sentiment dictionary -----
master_tokens_sentiment <- left_join(master_tokens_tbl, afinn, by = c("token" = "token")) %>%
  select(document, orient, group, sub_group, text_nchar, text, token, date, week, year, month, afinn_mean, afinn_median)

# Deal with outliers, bias?
master_tokens_sentiment <- master_tokens_sentiment %>%
  mutate(across(c(afinn_mean, afinn_median), ~if_else(. > 2 | . < -2, NA_integer_, .)))

# test <- master_tokens_sentiment %>%
#   filter(text_nchar < 400) %>%
#   slice_sample(n = 1000) %>%
#   select(sub_group, token, afinn_median, text)

# Share of tokens with a sentiment score
master_tokens_sentiment %>%
  group_by(category = if_else(is.na(afinn_median), "No match", "Matched")) %>%
  summarise(n = n()) %>%
  mutate(share = (n/sum(n)*100) %>% round)

# Check some without
#master_tokens_sentiment %>% filter(is.na(score)) %>% select(token)

# Check most popular tokens and their sentiment per media
master_tokens_sentiment %>%
  filter(!is.na(afinn_median)) %>%
  group_by(sub_group, token, afinn_median) %>%
  summarise(n = n()) %>%
  group_by(sub_group) %>%
  slice_min(order_by = n, n = 20) %>%
  left_join(., popular_tokens) %>%
  mutate(order = row_number()) %>%
  select(sub_group, non_stemmed, afinn_median)

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_doc <- master_tokens_sentiment %>%
  filter(!is.na(afinn_median)) %>%
  group_by(document, text, text_nchar) %>%
  summarise(afinn_document = mean(afinn_median, na.rm = TRUE),
            n_tokens = n()) %>%
  # ungroup() %>%
  # mutate(binary_document = if_else(afinn_document > 0.5, "Positive", "Negative")) %>%
  ungroup() %>%
  select(document, afinn_document, n_tokens, text, text_nchar)

# See some
sentiment_per_doc %>%
  filter(is.na(n_tokens))

sentiment_per_doc %>%
  filter(text_nchar < 400) #%>% view

# Add thetas (topics) ----
thetas_that_will_be_joined <- master_dt_thetas_long %>%
  select(sub_group, document, date, month, year, topic_no, topic_name, topic_proportion, text_nchar, text, url) %>%
  filter(year(date) >= 2020)

sentiment_per_doc_thetas <- left_join(thetas_that_will_be_joined,
                                      sentiment_per_doc %>% select(-c(text, text_nchar)),
                                      by = "document") %>%
  relocate(text_nchar, text, url, .after = last_col())

# Subset data ----
sentiment_per_doc_thetas %>% filter(is.na(n_tokens))

# Remove NAs
# And select topics for document
sentiment_per_doc_thetas_sub <- sentiment_per_doc_thetas %>%
  filter(!is.na(n_tokens) & !is.na(afinn_document)) %>%
  group_by(document) %>%
  slice_max(order_by = topic_proportion, n = 10) %>%
  ungroup()

# Summary after slice_max -----

# Summary
summary(sentiment_per_doc_thetas_sub$topic_proportion)

# Number of observations
nrow(sentiment_per_doc_thetas_sub) # pre
nrow(sentiment_per_doc_thetas_sub) # post
nrow(sentiment_per_doc_thetas_sub)/4/20 # per media and topic



