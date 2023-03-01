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


# Plot ----
# Plot cosine similarity score over time ----
# Box plot
master_cosine %>%
  mutate(date = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
         date_factor = as.factor(date),
         date_factor = fct_reorder(date_factor, date)) %>%
  filter(cosine_sim > 0) %>%
  group_by(comparison, date_factor) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  filter(cosine_sim < 0.05) %>%
  ggplot(.,
         aes(x = date_factor,
             y = cosine_sim)) +
  geom_boxplot(aes(colour = comparison)) +
  facet_wrap(~comparison) +
  labs(title = "Cosine similarity score between Tweets and radio articles over time") +
  theme_speciale

#save_plot_speciale("output/cosine_sim_boxplot.png")

master_cosine %>%
  filter(cosine_sim > 0) %>%
  #filter(date > as.Date("2020-01-01")) %>%
  group_by(comparison, date) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  ggplot(.,
         aes(x = date,
             y = cosine_sim)) +
  geom_point(aes(colour = comparison)) +
  geom_smooth(aes(colour = comparison)) +
  facet_wrap(~comparison) +
  geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
  geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
  geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
  scale_color_manual(values = colours_groups) +
  labs(title = "Cosine similarity score between Tweets and radio articles over time") +
  theme_speciale

#save_plot_speciale("output/cosine_sim_points_2.png")

# Plot difference in similarity ----
data_for_model %>%
  filter(week > as.Date("2019-06-01")) %>%
  ggplot(.,
         aes(x = week,
             y = difference)) +
  geom_point(aes(colour = name)) +
  geom_smooth(aes(colour = name), size = 2, se = FALSE) +
  #facet_wrap(~name) +
  geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
  geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
  geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
  scale_color_manual(values = colours_groups) +
  labs(title = "Difference to Radio Ndeke Luka in cosine similarity score over time") +
  theme_speciale

save_plot_speciale("output/diff_in_cosine_sim.png")


# Plot sentiment over time -----
# Regarding Russia (Poutine, Russia)
master_senti_scores %>%
  filter(group == "Radio") %>%
  ggplot(.,
         aes(x = week,
             y = difference)) +
  geom_point(aes(colour = sub_group)) +
  geom_smooth(aes(colour = sub_group), size = 2, se = FALSE) +
  facet_wrap(~sub_group) +
  geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
  geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
  geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
  scale_color_manual(values = colours_groups) +
  labs(title = "Relative sentiment regarding Russia over time",
       subtitle = "Greater values signify more positive sentiment towards Russia relative to general topics.") +
  theme_speciale

save_plot_speciale("output/senti_scores.png")




# Plot sentiment correlations ----
# General
senti_for_model_general %>%
  ggplot(.,
         aes(x = score_twitter,
             y = score_general)) +
  geom_point(aes(colour = sub_group)) +
  geom_smooth(aes(colour = sub_group), method = "lm") +
  facet_wrap(~sub_group) +
  scale_color_manual(values = colours_groups) +
  labs(title = "Correlation in general sentiment between Twitter and radio",
       subtitle = NULL) +
  theme_speciale

save_plot_speciale("output/senti_general_correlation.png")

# Russia-specific
senti_for_model_russia %>%
  ggplot(.,
         aes(x = score_twitter,
             y = score_russia)) +
  geom_point(aes(colour = sub_group)) +
  geom_smooth(aes(colour = sub_group), method = "lm") +
  facet_wrap(~sub_group) +
  scale_color_manual(values = colours_groups) +
  labs(title = "Correlation in sentiment re. Russia between Twitter and radio",
       subtitle = NULL) +
  theme_speciale

save_plot_speciale("output/senti_russia_correlation.png")






