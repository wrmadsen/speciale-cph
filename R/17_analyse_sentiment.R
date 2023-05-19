# Methodology and plots -----

## Check some documents -----
sentiment_per_doc_thetas_sub %>%
  filter(sub_group == "RJDH") %>%
  filter(topic_no == "x4") %>%
  select(sub_group, topic_proportion, afinn_document, text, url) %>%
  #group_by(sub_group) %>% summarise(n = n())
  arrange(-topic_proportion) #%>% view()

## Overall sentiment per media and year -----
sentiment_per_doc_thetas_sub %>%
  group_by(sub_group, year) %>%
  summarise(mean = mean(afinn_document, na.rm = TRUE)) %>%
  mutate(sub_group = as.factor(sub_group),
         sub_group = fct_relevel(sub_group,
                                 "Radio Lengo Songo",
                                 "Ndjoni Sango")) %>%
  ggplot(.,
         aes(x = mean,
             y = sub_group)) +
  geom_col(aes(fill = sub_group), show.legend = FALSE) +
  facet_wrap(~year) +
  scale_fill_manual(name = "", values = colours_groups) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, 0.25)) +
  labs(title = "Mean sentiment of documents per year by media outlets",
       x = "Average sentiment per document",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())


## Variance of sentiment per media ----
# Show if RJDH, for example, are biased by the way they use words
# words that feature on AFINN with particurlary high scores
master_tokens_sentiment %>%
  filter(year > 2019) %>%
  filter(year < 2023) %>%
  group_by(sub_group, year) %>%
  summarise(variance = var(afinn_median, na.rm = TRUE)) %>%
  ggplot(.,
         aes(x = year,
             y = variance,
             colour = sub_group)) +
  geom_line(aes(linetype = sub_group),
            linewidth = 2) +
  geom_point(size = 3) +
  #facet_wrap(~sub_group) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Variance of sentiment per media",
       x = NULL,
       y = NULL) +
  theme_speciale

master_tokens_sentiment %>%
  filter(year >= 2020) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(.,
         aes(x = afinn_median,
             group = year)) +
  geom_density(aes(colour = sub_group), show.legend = FALSE) +
  geom_vline(xintercept = 3) +
  geom_vline(xintercept = -3) +
  facet_wrap(~sub_group) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Distribution of sentiment per media",
       x = NULL,
       y = NULL) +
  theme_speciale

## Based on how many tokens per document ----

# Summary
sentiment_per_doc_thetas$n_tokens %>% summary()

## Per year ----
# Average tokens use to calculate sentiment per document
data_for_plot <- sentiment_per_doc_thetas_sub %>%
  group_by(sub_group, year) %>%
  summarise(mean = mean(n_tokens))

data_for_plot

data_for_plot %>%
  ggplot(.,
         aes(x = year,
             y = mean)) +
  #geom_point(size = 3) + 
  geom_line(aes(colour = sub_group, linetype = sub_group), size = 2) +
  scale_color_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Mean number of tokens used to calculate sentiment per document",
       x = NULL,
       y =  "Mean number of tokens") +
  theme_speciale

## Per topic ----
to_colour_vector_1 <- c("x18", "x16", "x19", "x17")

data_for_plot <- sentiment_per_doc_thetas_sub %>%
  group_by(topic_name, topic_no, sub_group) %>%
  summarise(mean = mean(n_tokens)) %>%
  ungroup() %>%
  mutate(sub_group = as.factor(sub_group),
         to_colour = if_else(topic_no %in% to_colour_vector_1, "1", "0"),
         topic_name = reorder_within(topic_name, mean, sub_group))

data_for_plot %>%
  ggplot(.,
         aes(x = mean,
             y = topic_name)) +
  geom_col(aes(fill = to_colour), show.legend = FALSE) +
  facet_wrap(~sub_group, scales = "free") +
  scale_y_reordered() +
  scale_fill_manual(name = "", values = c("1" = gold_speciale, "0" = bluel_speciale)) +
  labs(title = "Mean number of tokens used to calculate sentiment per document and topic",
       y = NULL,
       x =  "Mean number of tokens",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale


# Total sentiment -----
# Plot sentiment per year
# Calculate
data_for_plot <- sentiment_per_doc_thetas_sub %>%
  filter(year >= 2020) %>%
  filter(year < 2023) %>%
  mutate(time = floor_date(month, "quarter")) %>%
  group_by(sub_group, time) %>%
  summarise(mean = mean(afinn_document, na.rm = TRUE)) 

# Plot
data_for_plot %>%
  ggplot(.,
         aes(x = time,
             y = mean)) +
  geom_line(aes(colour = sub_group, linetype = sub_group), size = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Mean quarterly sentiment per media outlet",
       x = NULL,
       y =  "Mean sentiment per quarter") +
  theme_speciale

save_plot_speciale("output-figures/analysis_senti_mean_per_quarter.png", height = 23, width = 30)


# Topic plots ----

## Sentiment per media and topic -----
# Calculate
data_for_plot <- sentiment_per_doc_thetas_sub %>%
  filter(topic_no %in% to_colour_vector_1) %>%
  filter(!year %in% c(2022, 2023)) %>%
  group_by(topic_name) %>%
  mutate(mean_per_topic = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(sub_group) %>%
  mutate(mean_per_group = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(sub_group, mean_per_topic, mean_per_group, topic_name) %>%
  summarise(mean_per_topic_and_group = mean(afinn_document, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(difference = mean_per_topic_and_group - mean_per_group)

# Plot
data_for_plot %>%
  mutate(half = if_else(mean_per_topic > 3, "", " "),
         topic_name = fct_reorder(topic_name, mean_per_topic)) %>%
  ggplot(.,
         aes(x = mean_per_topic_and_group,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 7, stroke = 2) +
  geom_vline(xintercept = 0) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean sentiment for topics per media",
       x = "Mean sentiment",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output-figures/analysis_senti_mean_per_topic.png", height = 23, width = 30)

## Sentiment over time ----
# Calculate
data_for_plot <- sentiment_per_doc_thetas_sub %>%
  filter(topic_no %in% to_colour_vector_1) %>%
  group_by(sub_group, year) %>%
  mutate(mean_per_group_and_year = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(year, sub_group, mean_per_group_and_year, topic_name, topic_no) %>%
  summarise(mean_per_topic_and_year_and_group = mean(afinn_document, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(difference = mean_per_topic_and_year_and_group - mean_per_group_and_year) %>%
  # Calculate index
  group_by(sub_group, topic_name) %>%
  mutate(index = difference/difference[year == 2020],
         index = index*100) %>%
  ungroup() %>%
  arrange(sub_group, topic_name)

# Plot
data_for_plot %>%
  filter(year > 2019) %>%
  filter(year < 2023) %>%
  ggplot(.,
         aes(x = year,
             y = index,
             colour = sub_group,
             linetype = sub_group)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100) +
  facet_wrap(~topic_name,
             scales = "free"
  ) +
  scale_x_continuous(breaks = seq(2020, 2022, 1)) +
  scale_colour_manual(name = "", values = colours_groups[1:4]) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Sentiment per topic per media",
       x = NULL,
       y = "Sentiment score",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale


# Correlation between media and sentiment -----

# Correlations -----

## Topic prop. and sentiment ----
sentiment_per_doc_thetas %>% distinct(topic_name) %>% pull() # check out topic names

data_for_plot <- sentiment_per_doc_thetas %>%
  select(-topic_name) %>%
  pivot_wider(names_from = topic_no, values_from = topic_proportion) %>%
  transmute(sub_group, document, afinn_document, difference = x17 - x7)

data_for_plot %>%
  ggplot(.,
         aes(x = difference*100,
             y = afinn_document,
             group = sub_group)) +
  geom_point(fill = "grey99", shape = 21, alpha = 0.02) +
  geom_smooth(aes(colour = sub_group, linetype = sub_group),
              se = FALSE, linewidth = 2, method = "lm", show.legend = FALSE) +
  # Arrow 1
  geom_segment(data = data_for_plot %>% filter(sub_group == "Ndjoni Sango"),
               aes(x = 10, y = -2, xend = 60, yend = -2),
               linewidth = 0.7, colour = "black",
               arrow = arrow(length = unit(0.4, "cm"))) +
  geom_text(data = data_for_plot %>% filter(sub_group == "Ndjoni Sango"),
            aes(x = 35, y = -2.5, label = "More about MINUSCA, less about victims"),
            family = theme_font, colour = "black", size = 5,
  ) +
  # Arrow 2
  geom_segment(data = data_for_plot %>% filter(sub_group == "Ndjoni Sango"),
               aes(x = -87, y = -1.45, xend = -87, yend = 1.45),
               linewidth = 0.7, colour = "black",
               arrow = arrow(length = unit(0.4, "cm"))) +
  geom_text(data = data_for_plot %>% filter(sub_group == "Ndjoni Sango"),
            aes(x = -93, y = 0, label = "More positive"),
            family = theme_font, colour = "black", size = 5,
            angle = 90) +
  # Other
  facet_wrap(~sub_group#, scales = "free"
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  labs(title = "Russian media use more negative language to describe MINUSCA than victims",
       subtitle = "Each point is a document. The sentiment is calculated for each document.\nThen the difference in proportion is calculated for two topics in each document.",
       x = "Topic proportion difference, percentage points",
       y = "Sentiment score",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output-figures/analysis_senti_cor_minusca_victims.png", height = 23, width = 30)



