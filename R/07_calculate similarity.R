# Calculate similarity

# Cosine similarity -----
# Function to calculate cosine similarity
cosine_sim <- function(a, b){
  
  # Calculate the inner product of the two vectors
  numerator <- sum(a * b)
  
  # Calculate the magnitude of the first vector
  magnitude_a <- sqrt(sum(a^2))
  
  # Calculate the magnitude of the second vector
  magnitude_b <- sqrt(sum(b^2))
  
  # Calculate the denominator
  denominator <- magnitude_a * magnitude_b
  
  # Calculate the similarity
  cos_sim <- numerator/denominator
  
  return(cos_sim)
  
}

# Run the functions

x_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Radio")

y_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Twitter")

# Calculate similarity
cosine_sim <- textstat_simil(x = x_for_sim, 
                             y = y_for_sim,
                             method = "cosine")

# To tibble
cosine_sim <- cosine_sim %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "radio_document")

# Pivot
cosine_sim_long <- cosine_sim %>%
  pivot_longer(cols = c(2:ncol(.)), names_to = "twitter_document", values_to = "cosine_sim") %>%
  mutate(cosine_id = row_number()) %>%
  pivot_longer(cols = c(radio_document, twitter_document), names_to = "text_name", values_to = "document_no")

# Add dates and group names
# Join by document number twice
# For Twitter, then Radio
dates_and_groups <- master_dt %>%
  select(document, name, group, date) %>%
  tibble()

cosine_sim_full <- left_join(cosine_sim_long, dates_and_groups, by = c("document_no" = "document"))

summary(cosine_sim_full$date)

# Filter those similarity pairs whose dates are within range
cosine_sim_wide <- cosine_sim_full %>%
  select(cosine_sim, cosine_id, text_name, date) %>%
  pivot_wider(names_from = text_name, values_from = date) %>%
  mutate(days_diff = difftime(radio_document, twitter_document, units = c("days")) %>% abs() %>% as.integer())

summary(cosine_sim_wide$twitter_document)
summary(cosine_sim_wide$radio_document)

cosine_sim_sub <- cosine_sim_wide %>%
  filter(days_diff < 200)

# Return
cosine_sim_final <- cosine_sim_sub %>%
  select(date = radio_document, cosine_sim)

summary(cosine_sim_final$date)

cosine_sim_final






