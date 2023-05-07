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

# View tokens
master_sentiment %>%
  filter(token == "president") #%>% view()

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  group_by(document) %>%
  summarise(score_document = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Second add sentiment to thetas (topics) object ----
master_sentiment <- full_join(master_dt_thetas, sentiment_per_document, by = "document")

# Plot raw sentiment -----

# Boxplots 
calculate_sentiment(master_sentiment, pattern_1 = "franc") %>%
  ggplot(.,
         aes(y = difference)) +
  geom_boxplot(aes(colour = sub_group)) +
  facet_wrap(~orient) +
  scale_color_manual(name = "", values = colours_groups)

# Over time
calculate_sentiment(master_sentiment, pattern_1 = "centrafricain") %>%
  ggplot(.,
         aes(x = month,
             y = difference)) +
  #geom_point(aes(colour = orient)) +
  geom_smooth(aes(colour = orient, weight = n_docs_1), size = 2, se = TRUE, fill = "grey90") +
  #facet_wrap(~orient) +
  geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
  geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
  geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
  scale_color_manual(name = "", values = colours_groups) +
  labs(title = "Relative sentiment regarding X over time",
       subtitle = "Greater values signify more positive sentiment towards Russia relative to general topics.") +
  theme_speciale

save_plot_speciale("output/senti_scores.png")



# Create wide-dataset with pro-Russia in separate column for modelling ----
# 0 (general) sentiment
# Subset and then join
pro_russia_0 <- senti_0 %>%
  filter(orient == "Pro-Russia") %>%
  select(year, month, score_pro_russia = score_0)

senti_for_model_0 <- senti_0 %>%
  filter(orient == "Other") %>%
  # # Create lag per subgroup
  # group_by(sub_group) %>%
  # arrange(sub_group, month) %>%
  # mutate(score_0 = lead(score_0, 1)) %>%
  # ungroup() %>%
  left_join(pro_russia_0) %>%
  mutate(difference = score_pro_russia - score_0)

# Russ-specific sentiment
# Subset and join
pro_russia_1 <- senti_1 %>%
  filter(orient == "Pro-Russia") %>%
  select(year, month, score_pro_russia = score_1)

senti_for_model_1 <- senti_1 %>%
  filter(orient == "Other") %>%
  # # Create lag per subgroup
  # group_by(sub_group) %>%
  # arrange(sub_group, month) %>%
  # mutate(score_russ = lead(score_1, 1)) %>%
  # ungroup() %>%
  left_join(pro_russia_1) %>%
  mutate(difference = score_pro_russia - score_1)


# Plot sentiment correlations ----
# General
senti_for_model_0 %>%
  ggplot(.,
         aes(x = score_pro_russia,
             y = score_0)) +
  geom_point(aes(colour = sub_group)) +
  geom_smooth(aes(colour = sub_group), method = "lm") +
  facet_wrap(~sub_group) +
  scale_color_manual(values = colours_groups) +
  labs(title = "Correlation in general sentiment between pro-Russian outlets and others",
       subtitle = NULL) +
  theme_speciale

save_plot_speciale("output/senti_general_correlation.png")

# Russia-specific
senti_for_model_1 %>%
  ggplot(.,
         aes(x = score_1,
             y = score_0)) +
  geom_point(aes(colour = sub_group)) +
  geom_smooth(aes(colour = sub_group), method = "lm") +
  facet_wrap(~sub_group) +
  scale_color_manual(values = colours_groups) +
  labs(title = "Correlation in sentiment re. 'Russ' between pro-Russian outlets and others",
       subtitle = NULL) +
  theme_speciale

save_plot_speciale("output/senti_russia_correlation.png")




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



