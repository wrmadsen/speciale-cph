# Create diff-in-diff model

load("data-formatted/master_cosine.RData")

# Cosine similarity -----
# Running variable
data_for_model <- master_cosine %>%
  mutate(week = floor_date(date, unit = "week")) %>%
  group_by(comparison, week) %>%
  summarise(cosine_sim = mean(cosine_sim)) %>%
  ungroup() %>%
  transmute(comparison = as.factor(comparison),
            week,
            year = year(week),
            year = as.factor(year),
            cosine_sim)

data_for_model <- data_for_model %>%
  #mutate(id = row_number()) %>%
  pivot_wider(names_from = comparison, values_from = cosine_sim) %>%
  pivot_longer(cols = c("All radios and Twitter", "Radio Lengo Songo and Twitter",
                        "Reseau des journalistes and Twitter"),
               values_to = "comparison") %>%
  clean_names() %>%
  transmute(year, week, name, difference = comparison - radio_ndeke_luka_and_twitter) %>%
  filter(name != "All radios and Twitter") %>%
  arrange(week)

model_results <- lm(gdvote ~ as.factor(comparison) + as.factor(year) + trarrprop,
                    data  = data_for_model)



# Sentiment score ----




