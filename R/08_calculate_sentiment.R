# Calculate sentiment

# Join tokens with FEEL dictionary
master_sentiment <- left_join(master_tokens_tbl, feel, by = c("token" = "word")) %>%
  select(document, group, sub_group, text, token, date, week, year, month, score)


# Calculate sentiment ----

## First calculate sentiment per document ----
# Calculate
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  mutate(to_subset = if_else(str_detect(token, "russ|poutin"), 1, 0)) %>%
  group_by(document) %>%
  mutate(to_subset = max(to_subset)) %>%
  group_by(group, sub_group, date, week, month, year, to_subset, document, text) %>%
  summarise(score_document = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Check documents that are very critical of Russia
# And those very positive of Russia
# Supposedly
sentiment_per_document %>%
  filter(to_subset == 1) %>%
  slice_max(score_document, n = 10)

## General sentiment per group per week ----
senti_general <- sentiment_per_document %>%
  group_by(group, sub_group, year, month) %>%
  summarise(score_general = mean(score_document, na.rm = TRUE)) %>%
  ungroup()

## Sentiment per week for documents containing 'russ'
senti_russia <- sentiment_per_document %>%
  filter(to_subset == 1) %>%
  group_by(group, sub_group, year, month) %>%
  summarise(score_russia = mean(score_document, na.rm = TRUE)) %>%
  ungroup()

# Join to see relative sentiment
master_senti_scores <- full_join(senti_general, senti_russia) %>%
  mutate(difference = score_russia - score_general)

## Create wide-dataset with Twitter in separate column for modelling ----
# General sentiment
# Subset and then join
twitter_general <- senti_general %>%
  filter(group == "Twitter") %>%
  select(year, month, score_twitter = score_general)

senti_for_model_general <- senti_general %>%
  filter(group == "Radio") %>%
  # Create lag per subgroup
  group_by(sub_group) %>%
  arrange(sub_group, month) %>%
  mutate(score_general = lead(score_general, 1)) %>%
  ungroup() %>%
  left_join(twitter_general) %>%
  mutate(difference = score_twitter - score_general)


# Russia-specific sentiment
# Subset and join
twitter_russia <- senti_russia %>%
  filter(group == "Twitter") %>%
  select(year, month, score_twitter = score_russia)

senti_for_model_russia <- senti_russia %>%
  filter(group == "Radio") %>%
  # Create lag per subgroup
  group_by(sub_group) %>%
  arrange(sub_group, month) %>%
  mutate(score_russia = lead(score_russia, 1)) %>%
  ungroup() %>%
  left_join(twitter_russia) %>%
  mutate(difference = score_twitter - score_russia)








