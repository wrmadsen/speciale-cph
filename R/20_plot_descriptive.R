# Descriptive plots

# Documents per week ----
master_dt %>%
  group_by(group, sub_group, week) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = week,
             y = n)) +
  geom_line(aes(colour = sub_group)) +
  facet_wrap(~sub_group) +
  labs(title = "Number of documents per week",
       subtitle = "Documents mean Tweets and radio articles.") +
  theme_speciale

save_plot_speciale("output/n_docs_per_week.png")

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
   labs(title = "Difference to Radio Ndeke Luka in cosine similarity score over time") +
   theme_speciale

save_plot_speciale("output/diff_in_cosine_sim.png")


# Plot sentiment over time -----
# Regarding Russia (Poutine, Russia)
master_senti_scores %>%
   ggplot(.,
          aes(x = week,
              y = difference)) +
   geom_point(aes(colour = sub_group)) +
   geom_smooth(aes(colour = sub_group), size = 2, se = FALSE) +
   facet_wrap(~sub_group) +
   geom_vline(xintercept = as.Date("2018-01-15")) + # Wagner in CAR mentioned by Stratfor
   geom_vline(xintercept = as.Date("2019-10-15")) + # October 2019 FB breakdown
   geom_vline(xintercept = as.Date("2020-12-27")) + # CAR pres election
   labs(title = "Relative sentiment regarding Russia over time",
        subtitle = "Greater values signify larger difference in positive sentiment towards Russia than general topics.") +
   theme_speciale

save_plot_speciale("output/senti_scores.png")





