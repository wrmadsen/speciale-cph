# Descriptive plots

# Documents per week ----
master_dt %>%
  group_by(group, sub_group, week) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = week,
             y = n)) +
  geom_line(aes(colour = sub_group)) +
  labs(title = "Number of documents per week",
       subtitle = "Documents mean Tweets and radio articles.")


# Plot cosine similarity score over time ----
# Box plot
master_cosine %>%
  mutate(date = floor_date(date, unit = "week"),
         date_factor = as.factor(date),
         date_factor = fct_reorder(date_factor, date)) %>%
  filter(cosine_sim > 0) %>%
  group_by(comparison, date_factor) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  filter(cosine_sim < 0.05) %>%
  ggplot(.,
         aes(x = date_factor,
             y = cosine_sim)) +
  geom_boxplot() +
  geom_smooth() +
  facet_wrap(~comparison) +
  labs(title = "Cosine similarity score between Tweets and radio articles over time")

master_cosine %>%
  filter(cosine_sim > 0) %>%
  group_by(comparison, date) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  ggplot(.,
         aes(x = date,
             y = cosine_sim)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~comparison) +
  geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
  geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
  geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
  labs(title = "Cosine similarity score between Tweets and radio articles over time")







