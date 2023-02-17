# Descriptive plots

# Documents per week ----
master_dt %>%
  group_by(group, week) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = week,
             y = n)) +
  geom_line(aes(colour = group)) +
  labs(title = "Number of documents per week",
       subtitle = "Documents mean Tweets and radio articles.")


# Plot cosine similarity score over time ----
# Box plot
cosine_sim_final %>%
  #group_by()
  #summarise(n = n()) %>%
  filter(cosine_sim < 0.05) %>%
  ggplot(.,
         aes(x = as.factor(date),
             y = cosine_sim)) +
  geom_boxplot() +
  labs(title = "Cosine similarity score between Tweets and radio articles over time")

cosine_sim_final %>% distinct(date)
  group_by(date) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  ggplot(.,
         aes(x = date,
             y = cosine_sim)) +
  geom_point() +
  labs(title = "Cosine similarity score between Tweets and radio articles over time")







