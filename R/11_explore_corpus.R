# Explore corpus

# Data collection chapter ---

## Before pre-processing but after min. 100 characters  ----
# Number of words in total

# Summary
master_text %>% summary()

# Average per month per group
master_text %>%
  group_by(sub_group, month) %>%
  summarise(n_month = n()) %>%
  group_by(sub_group) %>%
  mutate(n_total = sum(n_month),
         average_month = mean(n_month)) %>%
  distinct(sub_group, n_total, average_month)

# Total per group
master_text %>%
  group_by(sub_group) %>%
  summarise(n = n())

# Periods per group
master_text %>%
  group_by(sub_group) %>%
  summarise(min = min(date),
            max = max(date))

# Per month, mean, median
master_text %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(mean = mean(n),
            median = median(n))


## After pre-processing -----
# 12,953 documents
master_tokens_tbl %>%
  distinct(document, sub_group, year) %>%
  group_by(sub_group) %>%
  summarise(n = n())






