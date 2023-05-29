# Plot tokens

# Count over time -----
# Function to plot over time
plot_freq_over_time_roll <- function(data_for_plot){
  
  ggplot(data_for_plot,
         aes(x = date,
             y = y_value)) +
    geom_line(aes(colour = token), show.legend = FALSE) +
    geom_label(data = data_for_plot %>% group_by(token) %>% filter(date == max(date)),
               family = theme_font, size = 5,
               aes(label = token)) +
    scale_fill_discrete(name = "") +
    scale_x_date(labels = dateformat(), date_breaks = "6 months") +
    theme_speciale
  
}

## Plot Radio Sengo Longo ----
tokens_frequency %>%
  filter(sub_group == "Radio Lengo Songo") %>%
  #filter(month > as.Date("2020-09-01")) %>%
  filter(count_lifetime > 160) %>% #distinct(token)
  mutate(y_value = share_roll,
         date = month,
         token = as.factor(token),
         token = fct_reorder(token, -count_lifetime)) %>%
  plot_freq_over_time_roll() +
  facet_wrap(~token, labeller = label_wrap_gen()) +
  geom_vline(xintercept = as.Date("2020-08-15")) + # caused by very few tokens during this period
  geom_vline(xintercept = as.Date("2020-11-01")) +
  geom_vline(xintercept = as.Date("2021-03-01")) +
  geom_vline(xintercept = as.Date("2021-07-01")) +
  geom_vline(xintercept = as.Date("2022-03-01")) +
  labs(title = "Most common tokens in Radio Lengo Songo",
       subtitle = "Each token's share of tokens in group (6-week rolling average). Tokens ordered by total count.",
       x = NULL,
       y = "Share of words, %",
       caption = "Source: WRM.\nRemoved less interesting tokens such as 'fait', 'tout', and 'c'est'.")


# Weird spikes around mid August 2020
tokens_frequency %>%
  filter(sub_group == "Radio Lengo Songo") %>%
  filter(share_roll > 1) %>%
  view()

# Plot total per week (that share is based on)
tokens_frequency %>%
  filter(!is.na(count_total)) %>%
  distinct(sub_group, date = month, count_total, token = "total") %>%
  filter(sub_group == "Radio Lengo Songo") %>%
  arrange(date) %>% view()
  #filter(week > as.Date("2021-06-01")) %>%
  #filter(week > as.Date())
  mutate(y_value = count_total) %>%
  plot_freq_over_time_roll() +
  geom_vline(xintercept = as.Date("2020-08-15")) +
  geom_vline(xintercept = as.Date("2020-11-01")) +
  geom_vline(xintercept = as.Date("2021-03-01")) +
  geom_vline(xintercept = as.Date("2021-07-01")) +
  geom_vline(xintercept = as.Date("2022-03-01"))



# Total count ----
facebook_count_total <- x

master_tokens_tbl %>%
  combine_tokens() %>%
  group_by(group_name, token) %>%
  summarise(n = n(),
            likes_mean = mean(likes),
            comments_mean = mean(comments)) %>%
  group_by(group_name) %>%
  mutate(share = n/sum(n)*100) %>%
  ungroup()