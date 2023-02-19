# Calculate sentiment

# Format sentiment dictionary
feel_raw <- read_delim("data-raw/Sentiment/FEEL.csv", delim = ";")

# Remove accents and stem
feel <- feel_raw %>%
  transmute(word = remove_accents(word),
            score = case_when(polarity == "positive" ~ 1,
                              polarity == "negative" ~ -1)
  )

# Join tokens with FEEL dictionary

master_sentiment <- left_join(master_tokens_tbl, feel, by = c("token" = "word")) %>%
  select(document, group, sub_group, text, token, date, week, score)


# Calculate sentiment ----

## First calculate sentiment per document ----
# Calculate
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  # Drop non-Radio
  filter(group == "Radio") %>%
  mutate(to_subset = if_else(str_detect(token, "russ|poutin"), 1, 0)) %>%
  group_by(document) %>%
  mutate(to_subset = max(to_subset)) %>%
  group_by(group, sub_group, week, date, to_subset, document, text) %>%
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
  group_by(group, sub_group, week) %>%
  summarise(score_general = mean(score_document, na.rm = TRUE)) %>%
  ungroup()

## Sentiment per week for documents containing 'russ'
senti_russia <- sentiment_per_document %>%
  filter(to_subset == 1) %>%
  group_by(group, sub_group, week) %>%
  summarise(score_russia = mean(score_document, na.rm = TRUE)) %>%
  ungroup()

# Join to see relative sentiment
master_senti_scores <- full_join(senti_general, senti_russia) %>%
  mutate(difference = score_russia-score_general)


master_senti_scores








