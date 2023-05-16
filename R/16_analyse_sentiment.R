# Check some documents -----
master_sentiment_raw_sub %>%
  filter(sub_group == "RJDH") %>%
  filter(topic_no == "x4") %>%
  select(sub_group, topic_proportion, afinn_document, text, url) %>%
  #group_by(sub_group) %>% summarise(n = n())
  arrange(-topic_proportion) #%>% view()


# Plot sentiment by topic -----

## Overall sentiment per media and year -----
master_sentiment_raw_sub %>%
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
  scale_x_continuous(breaks = seq(-0.4, 0.8, 0.2)) +
  labs(title = "Mean sentiment of documents per year by media outlets",
       x = "Average sentiment per document",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())

## Overall sentiment per media and topic -----
data_for_plot <- master_sentiment_raw_sub %>%
  group_by(topic_name) %>%
  mutate(mean_per_topic = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(sub_group) %>%
  mutate(mean_per_group = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(sub_group, mean_per_topic, mean_per_group, topic_name) %>%
  summarise(mean_per_topic_and_group = mean(afinn_document, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(difference = mean_per_topic_and_group - mean_per_group)

data_for_plot %>%
  mutate(half = if_else(mean_per_topic > 3, "", " "),
         topic_name = fct_reorder(topic_name, mean_per_topic)) %>%
  ggplot(.,
         aes(x = difference,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 5) +
  geom_vline(xintercept = 0) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean relative sentiment for topics of the documents by media",
       x = "Mean relative sentiment",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

# Boxplots
# data_for_plot <- master_sentiment_raw_sub %>%
#   group_by(topic_name) %>%
#   mutate(mean_per_topic = mean(afinn_document, na.rm = TRUE)) %>%
#   group_by(sub_group) %>%
#   mutate(mean_per_group = mean(afinn_document, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(difference = afinn_document - mean_per_group)
# 
# data_for_plot %>%
#   ggplot(.,
#          aes(x = afinn_document,
#              y = sub_group,
#              colour = sub_group)) +
#   geom_boxplot(outlier.shape = NA) +
#   facet_wrap(~topic_name, scales = "free")

## Sentiment over time ----
data_for_plot <- master_sentiment_raw_sub %>%
  group_by(year, sub_group) %>%
  mutate(mean_per_group_and_year = mean(afinn_document, na.rm = TRUE)) %>%
  group_by(year, sub_group, mean_per_group_and_year, topic_name, topic_no) %>%
  summarise(mean_per_topic_and_year_and_group = mean(afinn_document, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(difference = mean_per_topic_and_year_and_group - mean_per_group_and_year)

data_for_plot <- data_for_plot %>%
    filter(topic_no %in% top_topics_no)

data_for_plot %>%
  filter(year < 2023) %>%
  ggplot(.,
         aes(x = year,
             y = difference,
             colour = sub_group,
             linetype = sub_group)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0) +
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

# Linear model? Simple? See UCL slides
library(tidymodels)
lm(difference ~ year + sub_group + topic_name, data = data_for_plot) %>%
  summary() %>%
  tidy() %>%
  arrange(term) %>%
  print(n = 25)


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


## Multilevel model ----

lm(score_document ~ topic_share + year + sub_group*topic, data = master_sentiment_raw) %>% summary()


library(lme4)
multi_1 <- lme4::lmer(afinn_document ~ topic_proportion + (1 + topic_name | sub_group),
                      data = master_sentiment_raw_sub)

summary(multi_1)





