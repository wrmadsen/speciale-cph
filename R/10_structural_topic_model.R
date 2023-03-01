# Structural topic models (STMs)

glimpse(master_tokens_tbl)

# Run preliminary models ----
# Fit
stm_out <- stm(documents = master_dfm,
               K = 15,
               seed = 12345)

# Save model
save(stm_out, file = "output/stm_out.Rdata")

# Plot stm
plot(stm_out)

labelTopics(stm_out)

# Topic over time
# Assign the topic of interest to the data
# I have chosed topic 4, you might have selected something else.
master_dt$topic_7_russ <- stm_out$theta[,7]

master_dt %>%
  ggplot(aes(x = month,
             y = topic_7_russ)) +
  geom_point(aes(colour = sub_group), alpha = .2) +
  geom_smooth() +
  facet_wrap(~sub_group) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "14 months") +
  labs(title = "Proportion of documents allocated to topic 7",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

# Run model with covariates ----
# Fit model
docvars(master_dfm) %>% names

stm_out_prevalence_one <- stm(documents = master_dfm,
                              prevalence = ~spike_no + sub_group,
                              K = 15,
                              seed = 12345)

# Save model
save(stm_out_prevalence_one, file = "output/stm_out_prevalence_one.Rdata")

# Extract the matrix of words with highest frex scores
topic_labels_matrix <- labelTopics(stm_out_prevalence_one, n = 7)$frex

# Collapse the words for each topic into a single label
topic_labels <- apply(topic_labels_matrix, 1, paste0, collapse = "_")

topic_labels

# Estimate effects of covariates
# Effect of sub_group
prevalence_effects <- estimateEffect(formula = c(1:15) ~ sub_group, 
                                     stmobj = stm_out_prevalence_one,
                                     metadata = docvars(master_dfm))

summary(prevalence_effects)

plot.estimateEffect(prevalence_effects,
                    topics = 6,
                    covariate = "sub_group",
                    method = "pointestimate",
                    main = topic_labels[6])

# Topic over time
# Assign the topic of interest to the data
# I have chosed topic 4, you might have selected something else.
master_dt$topic_7_russ <- stm_out$theta[,7]

master_dt %>%
  ggplot(aes(x = month,
             y = topic_7_russ)) +
  geom_point(aes(colour = sub_group), alpha = .2) +
  geom_smooth() +
  facet_wrap(~sub_group) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "14 months") +
  labs(title = "Proportion of documents allocated to topic 7",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale









