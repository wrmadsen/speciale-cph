# Calculate sentiment

# Join tokens with sentiment dictionary -----
master_sentiment <- left_join(master_tokens_tbl, afinn, by = c("token" = "token")) %>%
  select(document, orient, group, sub_group, text_nchar, text, token, date, week, year, month, afinn)

test <- master_sentiment %>%
  filter(text_nchar < 400) %>%
  slice_sample(n = 1000) %>%
  select(sub_group, token, afinn, text)

# Share of tokens with a sentiment score
master_sentiment %>%
  group_by(category = if_else(is.na(afinn), "No match", "Matched")) %>%
  summarise(n = n()) %>%
  mutate(share = (n/sum(n)*100) %>% round)

# Check some without
#master_sentiment %>% filter(is.na(score)) %>% select(token)

# Check most popular tokens and their sentiment per media
master_sentiment %>%
  filter(!is.na(afinn)) %>%
  group_by(sub_group, token, afinn) %>%
  summarise(n = n()) %>%
  group_by(sub_group) %>%
  slice_min(order_by = n, n = 20) %>%
  left_join(., popular_tokens) %>%
  mutate(order = row_number()) %>%
  select(sub_group, non_stemmed, afinn)

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_document <- master_sentiment %>%
  group_by(document, text, text_nchar) %>%
  summarise(afinn_document = mean(afinn, na.rm = TRUE)) %>%
  # ungroup() %>%
  # mutate(binary_document = if_else(afinn_document > 0.5, "Positive", "Negative")) %>%
  ungroup() %>%
  select(document, afinn_document, text, text_nchar)

# See some
sentiment_per_document %>%
  filter(text_nchar < 400) #%>% view

# Add sentiment to thetas (topics) object ----
#load("output/master_dt_thetas_long.Rdata")

thetas_that_will_be_joined <- master_dt_thetas_long %>%
  select(sub_group, document, date, month, topic_no, topic_name, topic_proportion) %>%
  filter(year(date) >= 2020)

master_sentiment_joined <- left_join(thetas_that_will_be_joined,
                                     sentiment_per_document %>% select(-c(text, text_nchar)),
                                     by = "document")

# # Pivot longer
# master_sentiment_long <- master_sentiment_joined %>%
#   pivot_longer(cols = c(15:44), names_to = "topic", values_to = "topic_share") %>%
#   select(-c(text_nchar, url, spike_no, spike_text, spike_binary))

# Subset topics
# top_topics_no is created in the 13th script
master_sentiment_joined <- master_sentiment_joined %>%
  filter(topic_no %in% top_topics_no)

# Calculate sentiment ----

## Calculate share of binary per month -----
# master_sentiment_share <- master_sentiment_joined %>%
#   # First choose topic per document
#   group_by(document) %>%
#   filter(topic_proportion == max(topic_proportion)) %>%
#   # Calculate monthly share
#   group_by(sub_group, month, topic_name) %>%
#   summarise(n = n()) %>%
#   arrange(sub_group, month, topic_name) %>%
#   group_by(sub_group, month, topic_name) %>%
#   mutate(total = sum(n),
#          share = n/total)

## Calculate raw score per document ----
master_sentiment_raw <- master_sentiment_joined

summary(master_sentiment_raw$topic_proportion)

master_sentiment_raw_sub <- master_sentiment_raw %>%
  # Choose only very majority topics
  filter(topic_proportion > 0.01) #%>%
  # Or, for each document, choose largest topic by share
  # group_by(document) %>%
  # filter(topic_proportion == max(topic_proportion))

## Calculate relative sentiment -----


# Plot sentiment by topic and media -----

## Raw sentiment-per-sentiment  plot ----
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









