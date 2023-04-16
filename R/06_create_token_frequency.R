# Prepare to plot  

# Create functions ----
## Combine tokens ----
combine_tokens <- function(input){
  
  input %>%
    mutate(token = case_when(token %in% c("russ", "russo", "vladimir poutine",
                                          "sergue", "lavrov") ~ "RUSSIA",
                             token %in% c("zhihong", "chin", "chinois") ~ "CHINA",
                             token %in% c("minusma", "guterres", "onu", "nations unies") ~ "UN/MINUSMA",
                             token %in% c("europeen", "union europeenne", "europ",
                                          "eutm") ~ "EUROPE",
                             token %in% c("macron", "franc", "francais",
                                          "francophon", "barkhan") ~ "FRANCE",
                             token %in% c("cedeao") ~ "CEDEAO",
                             token %in% c("ouattara", "cote divoire") ~ "IVORY COAST/\nOUATTARA",
                             token %in% c("ukrain", "zelensky", "ukrainien") ~ "UKRAINE",
                             token %in% c("burkinab", "burkina faso") ~ "BURKINA FASO",
                             token %in% c("etats unis", "biden", "joe biden",
                                          "obam") ~ "USA",
                             token %in% c("occident", "occidental") ~ "THE WEST",
                             token %in% c("mal", "malien") ~ "MALI",
                             token %in% c("gouvern", "etat", "pay", "national") ~ "STATE/GOV",
                             token %in% c("africain", "afriqu") ~ "AFRICA",
                             TRUE ~ token)
    )
  
}

## Frequencies over time
count_per_period <- function(input, tokens_combined = FALSE){
  
  # Group_by depending on condition (if tokens have been combined or NOT)
  if (tokens_combined == FALSE) {
    
    # HAVE NOT BEEN COMBINED
    
    distinct_1 <- c("sub_group", #"token_less_interesting", "token_was_recoded", "token_more_interesting",
                    "week", "month", "year", "token")
    group_2 <- c("sub_group", #"token_less_interesting", "token_was_recoded", "token_more_interesting",
                 "week", "month", "year", "token")
    
  } else {
    
    # HAVE BEEN COMBINED
    
    distinct_1 <- c("sub_group","post_url", "week", "token", "likes", "comments")
    group_2 <- c("sub_group", "week", "token")
    
  }
  
  # Create count and share
  output <- input %>%
    filter(!is.na(token)) %>%
    # SPAM
    # Make sure each token is counted once per post
    # So simply because a post mentions X multiple times
    # X is still only counted once
    dplyr::distinct_at(distinct_1) %>%
    # Summarise count after accounting for SPAM
    # NOT SUMMARISING this count = sum(token_count)
    dplyr::group_by_at(group_2) %>%
    summarise(count = n()) %>%
    group_by(sub_group, month) %>%
    # Calculate share per month
    mutate(count_total = sum(count),
           share = count/count_total*100) %>%
    # Calculate total count all lifetime
    group_by(sub_group, token) %>%
    mutate(count_lifetime = sum(count)) %>%
    ungroup() %>%
    arrange(sub_group, token, week)
  
  # Return
  output
  
}

filter_tokens <- function(input, drop_value, consec_weeks){
  
  # Add max share and min share columns to enable filtering later
  output <- input %>%
    group_by(sub_group, token) %>%
    mutate(share_min = min(share),
           share_max = max(share)) %>%
    ungroup()
  
  # Also filter by share
  # Drop tokens with a very low max share
  output <- output %>%
    filter(share_max > drop_value)
  
  # Tokens must be mentioned in at least 4 weeks
  if(consec_weeks) {
    
    output <- output %>%
      group_by(sub_group, token) %>%
      filter(n() >= 4) %>%
      ungroup()
  }
  
  output
  
}

# Complete each sub_group
complete_sub_group <- function(input){
  
  # input <- facebook_frequency %>%
  #   filter(sub_group == "🇲🇱YÈRÈWOLO DEBOUT SUR LES REMPARTS 🇲🇱") %>%
  #   filter(token %in% c("clarification", "afrique", "windows"))
  
  # Fill missing rows
  first_date = min(input$week)
  
  last_date = max(input$week)
  
  output <- input %>%
    group_by(token) %>%
    complete(month = seq.Date(first_date, last_date, by = "month"),
             fill = list(share = NA_integer_)) %>%
    ungroup()
  
  # Each token must at least have X consecutive non-NA values
  output <- output %>%
    arrange(token, month) %>%
    group_by(token) %>%
    # keep token if keep == 1
    # keep = 1 if 3 rows consecutively at some point are non-NA
    mutate(keep = if_else(!is.na(share) & !is.na(lag(share)) & !is.na(lag(share, 2)), 1, 0),
           keep = max(keep)) %>%
    filter(keep == 1) %>%
    select(-keep) %>%
    ungroup()
  
  # Replace share NAs with zero
  # Perhaps not - will mess up our rolling average calculation below
  # output <- output %>%
  #   mutate(share = if_else(is.na(share), 0, share))
  
  # Complete count life time
  # To enable later ordering plots (for example)
  output <- output %>%
    group_by(token) %>%
    fill(count_lifetime) %>%
    fill(count_lifetime, .direction = "up")
  
  output
  
}

# Create rolling average and index
create_rolling_index <- function(input){
  
  # Rolling average
  output <- input %>%
    group_by(sub_group, token) %>%
    arrange(token, week) %>%
    mutate(share_roll = RcppRoll::roll_mean(share, 4, fill = NA, na.rm = TRUE)) %>%
    ungroup()
  
  # Create index
  # output <- output %>%
  #   filter(!is.na(share_roll)) %>%
  #   group_by(sub_group, token) %>%
  #   mutate(base_period = min(month)) %>%
  #   mutate(share_roll_index = share_roll/share_roll[week == base_period],
  #          share_roll_index = share_roll_index*100) %>%
  #   ungroup()
  
  # Return
  
}


# Run functions ----

# Count
# To combine token OR NOT
# Drop less interesting tokens
tokens_count_period <- master_tokens_tbl %>%
  #combine_tokens() %>% # COMBINE or NOT
  count_per_period(tokens_combined = FALSE)

# Check token if need be
tokens_count_period %>%
  group_by(token) %>%
  summarise(n = sum(count)) %>%
  arrange(-n)

tokens_count_period %>%
  filter(token == "THE WEST")

tokens_count_period %>%
  filter(sub_group == "Radio Lengo Songo") #%>% view()

# Filter
# The higher drop_value, the fewer rows
tokens_count_period_sub <- tokens_count_period %>%
  filter_tokens(drop_value = 0.1, consec_weeks = TRUE)

tokens_count_period_sub %>% distinct(token) %>% nrow() # tokens included in total

# Create frequency object with completed and rolled values

# Complete for each group and token
# So one group may be completed in dates X-Y
# While another is completed Y-Z
# "windows" should have been removed
tokens_frequency <- tokens_count_period_sub %>%
  group_by(sub_group) %>%
  group_modify(~complete_sub_group(.x)) %>%
  group_by(sub_group) %>%
  fill(sub_group) %>%
  fill(sub_group, .direction = "up") %>%
  ungroup()

# Rolling index
tokens_frequency <- create_rolling_index(tokens_frequency)


