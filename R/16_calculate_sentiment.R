# Calculate sentiment

# Join tokens with sentiment dictionary -----
master_tokens_sentiment <- left_join(master_tokens_tbl, afinn, by = c("token" = "token")) %>%
  select(document, orient, group, sub_group, text_nchar, text, token, date, week, year, month,
         afinn_mean, afinn_median)

# Deal with outliers, bias?
master_tokens_sentiment <- master_tokens_sentiment %>%
  mutate(across(c(afinn_mean, afinn_median), as.numeric)) #%>%
#mutate(across(c(afinn_mean, afinn_median), ~if_else(. > 1 | . < -1, NA_integer_, .))) %>%
# mutate(across(c(afinn_mean, afinn_median),
#               ~case_when(. > 1 ~ as.numeric(1),
#                          . < -1 ~ as.numeric(-1),
#                          . < 1 & . > 0 ~ as.numeric(1),
#                          . > -1 & . < 0 ~ as.numeric(-1),
#                          is.na(.) ~ as.numeric(NA),
#                          .default = as.numeric(.))
#               ))

# test <- master_tokens_sentiment %>%
#   filter(text_nchar < 400) %>%
#   slice_sample(n = 1000) %>%
#   select(sub_group, token, afinn_median, text)

# Share of tokens with a sentiment score
master_tokens_sentiment %>%
  group_by(category = if_else(is.na(afinn_median), "No match", "Matched")) %>%
  summarise(n = n()) %>%
  mutate(share = (n/sum(n)*100) %>% round)

# Check some without
#master_tokens_sentiment %>% filter(is.na(score)) %>% select(token)

# Check most popular tokens and their sentiment per media
master_tokens_sentiment %>%
  filter(!is.na(afinn_median)) %>%
  group_by(sub_group, token, afinn_median) %>%
  summarise(n = n()) %>%
  group_by(sub_group) %>%
  slice_min(order_by = n, n = 20) %>%
  left_join(., popular_tokens) %>%
  mutate(order = row_number()) %>%
  select(sub_group, non_stemmed, afinn_median)

# First calculate sentiment per document ----
# And add columns based on tokens to calculate later sentiments
sentiment_per_doc <- master_tokens_sentiment %>%
  filter(!is.na(afinn_median)) %>%
  group_by(document, text, text_nchar) %>%
  summarise(afinn_document = mean(afinn_median, na.rm = TRUE),
            n_tokens = n()) %>%
  # ungroup() %>%
  # mutate(binary_document = if_else(afinn_document > 0.5, "Positive", "Negative")) %>%
  ungroup() %>%
  select(document, afinn_document, n_tokens, text, text_nchar)

# Average sentiment
summary(sentiment_per_doc$afinn_document)

# See some
sentiment_per_doc %>%
  filter(is.na(n_tokens))

sentiment_per_doc %>%
  filter(text_nchar < 400) #%>% view

# Add thetas (topics) ----
thetas_that_will_be_joined <- master_dt_thetas_long %>%
  select(sub_group, document, date, month, year, topic_no, topic_name, topic_proportion, text_nchar, text, url)

sentiment_per_doc_thetas <- left_join(thetas_that_will_be_joined,
                                      sentiment_per_doc %>% select(-c(text, text_nchar)),
                                      by = "document") %>%
  relocate(text_nchar, text, url, .after = last_col())

# Subset data ----

## Remove NAs ----
sentiment_per_doc_thetas <- sentiment_per_doc_thetas %>%
  filter(!is.na(n_tokens) & !is.na(afinn_document)) 

## Select by topic proportion ----

### Determine best way ----
# Trade-off b.t.
# Number of unique documents
# High topic proportion (mean)
filter_by_topic_proportion <- function(input, rate){
  
  input %>%
    filter(topic_proportion >= rate) %>%
    transmute(rate = rate, document, sub_group, topic_proportion)
  
}

best_topic_selection <- bind_rows(filter_by_topic_proportion(sentiment_per_doc_thetas, 0.0),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.1),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.2),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.3),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.4),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.5),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.6),
                                  filter_by_topic_proportion(sentiment_per_doc_thetas, 0.7))

best_topic_selection

data_for_plot <- best_topic_selection %>%
  group_by(sub_group, rate) %>%
  summarise(docs = n_distinct(document),
            mean = mean(topic_proportion)*100) %>%
  group_by(sub_group) %>%
  mutate(docs_original = max(docs),
         docs_share = docs/docs_original*100)

data_for_plot %>%
  ggplot(.,
         aes(x = mean,
             y = docs_share)) +
  geom_vline(xintercept = 50) +
  geom_line(aes(colour = sub_group, linetype = sub_group), size = 2,
            show.legend = FALSE) +
  geom_point(size = 3) +
  geom_text_repel(data = data_for_plot %>% filter(sub_group == "Ndjoni Sango" & rate == 0.3),
                  aes(label = paste0(rate*100, "% minimum")),
                  family = theme_font,
                  size = 4.5) +
  scale_color_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
  facet_wrap(~sub_group, scales = "free_y") +
  labs(title = "Balance between a high topic proportion and many documents",
       y = "Share of original documents, %",
       x =  "Mean topic proportion, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

#save_plot_speciale("output-figures/appendix_select_topic_prop.png")

### Actually do it -----
# slice_max() is also an option
sentiment_per_doc_thetas_sub <- sentiment_per_doc_thetas %>%
  filter(topic_proportion >= 0.2)


# Summary post -----

# Variance
sentiment_per_doc_thetas_sub$topic_proportion %>% summary()

# Average sentiment
sentiment_per_doc_thetas_sub %>%
  group_by(sub_group, topic_name) %>%
  summarise(mean = mean(afinn_document)) %>%
  filter(grepl("FACA", topic_name))

# Summary
summary(sentiment_per_doc_thetas_sub$topic_proportion)

# Number of observations
nrow(sentiment_per_doc_thetas_sub) # pre
nrow(sentiment_per_doc_thetas_sub) # post
nrow(sentiment_per_doc_thetas_sub)/4/20 # per media and topic



