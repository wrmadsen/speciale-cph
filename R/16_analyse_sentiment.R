# Plot sentiment by topic -----


## Overall sentiment per media -----
master_sentiment_raw_sub %>%
  group_by(sub_group) %>%
  summarise(mean = mean(afinn_document, na.rm = TRUE)) %>%
  mutate(sub_group = fct_reorder(sub_group, -mean)) %>%
  ggplot(.,
         aes(x = mean,
             y = sub_group)) +
  geom_col(aes(fill = sub_group), show.legend = FALSE) +
  scale_fill_manual(name = "", values = colours_groups) +
  scale_x_continuous(breaks = seq(-0.2, 0.1, 0.05)) +
  labs(title = "Mean sentiment per document by media",
       x = "Average sentiment per document",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())
  

## Overall sentiment per media and topic -----
data_for_plot <- master_sentiment_raw_sub %>%
  group_by(topic_name, topic_no) %>%
  mutate(mean_overall = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(sub_group, mean_overall, topic_name, topic_no) %>%
  summarise(mean_per_group = mean(afinn_document, na.rm = TRUE)) %>%
  ungroup()

data_for_plot %>%
  mutate(half = if_else(mean_overall > 3, "", " "),
         topic_name = fct_reorder(topic_name, mean_overall)) %>%
  ggplot(.,
         aes(x = mean_per_group,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 5) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean sentiment for topics by media",
       x = "Mean sentiment",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale






## Sentiment over time ----
master_sentiment_raw_sub %>%
  ggplot(.,
         aes(x = date,
             y = afinn_document,
             colour = sub_group,
             linetype = sub_group)) +
  #geom_point(alpha = 0.1) +
  #geom_line() +
  #geom_smooth(se = FALSE, linewidth = 1.5) +
  geom_smooth(aes(weight = topic_proportion), se = FALSE, linewidth = 1.2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~topic_name,
             scales = "free"
             ) +
  scale_colour_manual(name = "", values = colours_groups[1:4]) +
  scale_linetype_manual(name = "", values = lines_group) +
  #scale_y_continuous(trans = "log") +
  scale_x_date(labels = dateformat(), date_breaks = "12 months") +
  labs(title = "Sentiment per topic per media",
       x = NULL,
       y = "Sentiment score",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale





# Correlation between media and sentiment -----

# Linear model? Simple? See UCL slides

lm(score_document ~ month + sub_group*topic, data = master_sentiment_raw) %>% summary()



# Correlation sentiment and topic share -----

## Plot ----
master_sentiment_raw %>%
  filter() %>%
  ggplot(.,
         aes(x = topic_proportion,
             y = afinn_document,
             group = sub_group)) +
  geom_point(fill = "grey99", shape = 21, alpha = 0.05) +
  geom_smooth(aes(colour = sub_group, linetype = sub_group),
              se = FALSE,
              method = "lm") +
  facet_wrap(~topic_name, scales = "free") +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Correlation between topic proportion and sentiment score",
          x = "Topic proportion, %",
          y = "Sentiment score, AFINN",
          caption = "Source: William Rohde Madsen.") +
  theme_speciale


## Model ----

lm(score_document ~ topic_share + year + sub_group*topic, data = master_sentiment_raw) %>% summary()









