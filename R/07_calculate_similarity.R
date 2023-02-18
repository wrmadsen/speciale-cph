# Calculate similarity

# Cosine similarity -----

# Create cosine tibble
create_cosine_tibble <- function(input_twitter, input_radio, name_of_column, master_dt){
  
  # Test
  # name_of_column = "Test"
  # input_twitter <- twitter_for_sim
  # input_radio <- radio_for_sim
  
  cosine_sim_scores <- textstat_simil(x = input_twitter, 
                                      y = input_radio,
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
    transmute(document_no = document, date) %>%
    tibble()
  
  # Join function
  cosine_sim_full <- left_join(cosine_sim_long, dates_and_groups, by = "document_no")
  
  # Check dates correspond
  # Radio should show 2017 to 2023
  # text9556 is the oldest for radio ndeke
  # text14065 is the newest for radio ndeke
  # Twitter should be 2014 to 2023
  cosine_sim_full %>%
    filter(text_name == "twitter_document") %>%
    .$date %>% summary()
  
  # Filter those similarity pairs whose dates are within range
  # Calculate difference in days
  # Then choose the date of the radio document
  # as the future reference marker
  cosine_sim_wide <- cosine_sim_full %>%
    select(cosine_sim, cosine_id, text_name, date) %>%
    pivot_wider(names_from = text_name, values_from = date) %>%
    mutate(days_diff = difftime(radio_document, twitter_document, units = c("days")),
           days_diff = days_diff %>% as.integer(),
           days_diff_abs = abs(days_diff) %>% as.integer())
  
  # Radio must be after Twitter document (positive difference)
  # If days_diff is 10 days, radio document is 10 days after twitter document
  cosine_sim_sub <- cosine_sim_wide %>%
    filter(days_diff < 10 & days_diff > 0)
  
  # Return
  cosine_sim_final <- cosine_sim_sub %>%
    transmute(comparison = name_of_column, date = radio_document, cosine_sim)
  
  cosine_sim_final
  

}


# Run function
master_dt %>% distinct(sub_group)

# Radio Ndeke and Twitter
radio_for_sim <- dfm_subset(master_dfm_tf_idf, sub_group == "Radio Ndeke Luka")

twitter_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Twitter")

cosine_results_1 <- create_cosine_tibble(radio_for_sim, twitter_for_sim, "Radio Ndeke Luka and Twitter", master_dt)

# Radio Lengo Songo and Twitter
radio_for_sim <- dfm_subset(master_dfm_tf_idf, sub_group == "Radio Lengo Songo")

cosine_results_2 <- create_cosine_tibble(radio_for_sim, twitter_for_sim, "Radio Lengo Songo and Twitter", master_dt)

# Radio Reseau and Twitter
radio_for_sim <- dfm_subset(master_dfm_tf_idf, sub_group == "Reseau des journalistes")

cosine_results_3 <- create_cosine_tibble(radio_for_sim, twitter_for_sim, "Reseau des journalistes and Twitter", master_dt)

# All radios and Twitter
radio_for_sim <- dfm_subset(master_dfm_tf_idf, group == "Radio")

cosine_results_4 <- create_cosine_tibble(radio_for_sim, twitter_for_sim, "All radios and Twitter", master_dt)


# Bind cosine results
master_cosine <- bind_rows(cosine_results_1, cosine_results_2, cosine_results_3, cosine_results_4)


