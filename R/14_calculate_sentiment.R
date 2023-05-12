# Calculate sentiment

# Join tokens with FEEL dictionary -----
master_sentiment <- left_join(master_tokens_tbl, feel, by = c("token" = "text")) %>%
  select(document, orient, group, sub_group, text, token, date, week, year, month, score)

# Share of tokens with a sentiment score
master_sentiment %>%
  group_by(has_score = !is.na(score)) %>%
  summarise(n = n()) %>%
  mutate(share = n/sum(n)) # 84% don't

# Check some without
#master_sentiment %>% filter(is.na(score)) %>% select(token)

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  group_by(document) %>%
  summarise(score_document = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Second add sentiment to thetas (topics) object ----
master_sentiment <- full_join(master_dt_thetas, sentiment_per_document, by = "document")

# Plot sentiment by topic -----
