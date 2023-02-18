# Calculate similarity

# Cosine similarity -----

# Subset each in weighted dfm
radio_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Radio")
nrow(radio_for_sim)

twitter_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Twitter")
nrow(twitter_for_sim)

# Calculate similarity
cosine_sim_scores <- textstat_simil(x = twitter_for_sim, 
                                    y = radio_for_sim,
                                    method = "cosine")

# To tibble
cosine_sim <- cosine_sim_scores %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "twitter_document") # named after whatever variable refers to obs length

# Pivot
cosine_sim_long <- cosine_sim %>%
  mutate(cosine_id = row_number()) %>%
  select(cosine_id, twitter_document, everything()) %>%
  pivot_longer(cols = c(3:ncol(.)), names_to = "radio_document", values_to = "cosine_sim") %>%
  mutate(cosine_id = row_number()) %>%
  pivot_longer(cols = c(radio_document, twitter_document), names_to = "text_name", values_to = "document_no")

# Add dates and group names
# Join by document number twice
# For Twitter, then Radio
dates_and_groups <- master_dt %>%
  transmute(document_no = document, date, document_factor = as.factor(document_no)) %>%
  tibble()

# Join function
cosine_sim_full <- right_join(cosine_sim_long, dates_and_groups, by = "document_no")

# Check dates correspond
# Radio should show 2017 to 2023
# text9556 is the oldest for radio ndeke
# text14065 is the newest for radio ndeke
# Twitter should be 2014 to 2023
cosine_sim_full %>%
  filter(text_name == "radio_document") %>%
  .$date %>% summary()

# Filter those similarity pairs whose dates are within range
# Calculate difference in days
# Then choose the date of the radio document
# as the future reference marker
cosine_sim_wide <- cosine_sim_full %>%
  select(cosine_sim, cosine_id, text_name, date) %>%
  pivot_wider(names_from = text_name, values_from = date) %>%
  mutate(days_diff = difftime(radio_document, twitter_document, units = c("days")),
         days_diff = abs(days_diff) %>% as.integer())

cosine_sim_sub <- cosine_sim_wide %>%
  filter(days_diff < 10)

# Return
cosine_sim_final <- cosine_sim_sub %>%
  select(date = radio_document, cosine_sim)

cosine_sim_final

summary(cosine_sim_final$date)





