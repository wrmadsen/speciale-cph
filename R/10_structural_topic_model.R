# Structural topic models (STMs)

glimpse(master_tokens_tbl)

# Run preliminary models ----
# Fit
stm_out <- stm(documents = master_dfm,
               K = 15,
               seed = 12345)

# Save model
save(stm_out, file = "output/stm_out.Rdata")
load("output/stm_out.Rdata")

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
## Fit model one -----
docvars(master_dfm) %>% names

stm_out_prevalence_one <- stm(documents = master_dfm,
                              prevalence = ~sub_group,
                              K = 15,
                              seed = 12345)

# Save model
save(stm_out_prevalence_one, file = "output/stm_out_prevalence_one.Rdata")
load("output/stm_out_prevalence_one.Rdata")

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

topic_no <- 12

topic_label <- topic_labels[12]

plot.estimateEffect(prevalence_effects,
                    topics = topic_no,
                    covariate = "sub_group",
                    method = "pointestimate",
                    main = topic_labels[topic_no])

# Create object of thetas
stm_out_prevalence_one_theta <- stm_out_prevalence_one$theta %>%
  data.frame() %>%
  tibble() %>%
  clean_names()

names(stm_out_prevalence_one_theta) <- paste0("theta_", str_pad(1:15, 2, pad = "0"), "_", topic_labels)

# Join theta values to master_dt
master_dt_thetas <- master_dt %>%
  bind_cols(stm_out_prevalence_one_theta)

### Topic proportion by time and group ----
# Calculate
data_for_plot <- master_dt_thetas %>%
  group_by(sub_group, week) %>%
  summarise(n = n(),
            across(starts_with("theta"), mean)) %>%
  ungroup()

data_for_plot <- data_for_plot %>%
  pivot_longer(cols = c(starts_with("theta")))

# # Add "total" rows to
# data_for_plot <- data_for_plot %>%
#   filter(!sub_group %in% c("Radio Lengo Songo", "Non-Russian total")) %>%
#   mutate(sub_group = "Non-Russian average") %>%
#   group_by(sub_group, name, week) %>%
#   summarise(value = mean(value)) %>%
#   ungroup() %>%
#   bind_rows(data_for_plot)

# Plot
data_for_plot %>%
  mutate(name = gsub("(.{23})\\_(.*)", "\\1\n\\2", name)) %>%
  filter(week > as.Date("2020-01-01")) %>%
  filter(sub_group %in% c("Radio Lengo Songo", "Non-Russian total", "Non-Russian average")) %>%
  filter(!grepl("13", name)) %>%
  ggplot(aes(x = week,
             y = value)) +
  #geom_point(aes(colour = sub_group), alpha = 0.1) +
  geom_smooth(aes(colour = sub_group), se = FALSE, linewidth = 2) +
  facet_wrap(~name, scales = "free") +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "8 months") +
  #scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Proportion of documents allocated to topic ",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

# Topic proportion by spike period
master_dt_thetas %>%
  group_by(sub_group, spike_binary) %>%
  summarise(n = n(),
            across(starts_with("theta"), mean)) %>%
  ungroup() %>%
  pivot_longer(cols = c(starts_with("theta"))) %>%
  ggplot(aes(x = spike_binary,
             y = value)) +
  geom_col() +
  facet_wrap(~name)
labs(title = paste0("Proportion of documents allocated to topic ", topic_no, "\n", topic_label),
     x = NULL,
     caption = "Source: William Rohde Madsen.") +
  theme_speciale

## Fit model two -----
docvars(master_dfm) %>% names

stm_out_prevalence_two <- stm(documents = master_dfm,
                              prevalence = ~sub_group*spike_binary,
                              K = 15,
                              seed = 12345)

# Save model
save(stm_out_prevalence_two, file = "output/stm_out_prevalence_two.Rdata")
load("output/stm_out_prevalence_two.Rdata")

# Extract the matrix of words with highest frex scores
topic_labels_matrix <- labelTopics(stm_out_prevalence_two, n = 7)$frex

# Collapse the words for each topic into a single label
topic_labels <- apply(topic_labels_matrix, 1, paste0, collapse = "_")

topic_labels <- paste0(str_pad(1:15, 2, pad = "0"), "_", topic_labels)

topic_labels <- gsub("(.{18})\\_(.*)", "\\1\n\\2", topic_labels)

topic_labels_tibble <- topic_labels %>%
  tibble("topic_name" = .) %>%
  mutate(topic = row_number() %>% as.factor())

# Estimate effects of covariates
# Effect of sub_group
# 1:15 indicate the topic, thus all are included with 1:15
prevalence_effects <- estimateEffect(formula = c(1:15) ~ sub_group*spike_binary, 
                                     stmobj = stm_out_prevalence_two,
                                     metadata = docvars(master_dfm))

summary(prevalence_effects)


# Extract estimate effects
extract.estimateEffect(prevalence_effects,
                       "sub_group")

## Plot interaction effect between spike_binary and sub_group ----
effects_int_1 <- stminsights::get_effects(estimates = prevalence_effects,
                                          variable = "sub_group",
                                          type = "pointestimate",
                                          moderator = "spike_binary",
                                          modval = 1)

effects_int_0 <- stminsights::get_effects(estimates = prevalence_effects,
                                          variable = "sub_group",
                                          type = "pointestimate",
                                          moderator = "spike_binary",
                                          modval = 0)

# Bind
effects_int_tibble <- bind_rows(effects_int_1,
                                effects_int_0) %>%
  left_join(topic_labels_tibble) %>%
  select(sub_group = value, prop = proportion, topic_name, moderator)

# Pivot wide to calculate difference
effects_int_tibble <- effects_int_tibble %>%
  mutate(moderator = paste0("binary_", moderator)) %>%
  pivot_wider(names_from = moderator, values_from = prop) %>%
  mutate(difference = binary_1 - binary_0)

# Plot
effects_int_tibble %>%
  ggplot(.,
         aes(x = difference,
             y = sub_group)) +
  geom_col(aes(fill = sub_group)) +
  facet_wrap(~topic_name) +
  scale_fill_manual(name = "", values = colours_groups) +
  labs(title = "Difference in effect of spike_binary = 1",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/diff_in_effect_spike_binary.png")

## Plot interaction effect between specific spike period and sub_group ----
effects_int_1 <- stminsights::get_effects(estimates = prevalence_effects,
                                          variable = "sub_group",
                                          type = "pointestimate",
                                          moderator = "spike_period",
                                          modval = 0)

effects_int_0 <- stminsights::get_effects(estimates = prevalence_effects,
                                          variable = "sub_group",
                                          type = "pointestimate",
                                          moderator = "spike_period",
                                          modval = 1)

# Bind
effects_int_tibble <- bind_rows(effects_int_1,
                                effects_int_0) %>%
  left_join(topic_labels_tibble) %>%
  select(sub_group = value, prop = proportion, topic_name, moderator)

# Pivot wide to calculate difference
effects_int_tibble <- effects_int_tibble %>%
  mutate(moderator = paste0("binary_", moderator)) %>%
  pivot_wider(names_from = moderator, values_from = prop) %>%
  mutate(difference = binary_1 - binary_0)

# Plot
effects_int_tibble %>%
  ggplot(.,
         aes(x = difference,
             y = sub_group)) +
  geom_col(aes(fill = sub_group)) +
  facet_wrap(~topic_name) +
  scale_fill_manual(name = "", values = colours_groups) +
  labs(title = "Difference in effect of spike_binary = 1",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/diff_in_effect_spike_binary.png")



## Calculate probability of each word by topic ----
# Using tidytext?
# library(tidytext)
stm_out_prevalence_two %>% tidy()

td_gamma <- tidy(stm_out_prevalence_two, matrix = "gamma",
                 document_names = rownames(out$meta))

td_gamma



# Topic correlations network ----
set.seed(381)

mod_out_corr <- topicCorr(stm_out_prevalence_two)

plot(mod_out_corr, vlabels = topic_labels)

# extract network
library(stminsights)

stm_corrs <- stminsights::get_network(model = stm_out_prevalence_two,
                                      method = "simple",
                                      labels = topic_labels,
                                      cutoff = 0.001,
                                      cutiso = TRUE)

# plot network with ggraph
ggraph(stm_corrs, layout = "fr") +
  geom_edge_link(aes(edge_width = weight),
                 label_colour = orange_speciale,
                 edge_colour = bluel_speciale) +
  geom_node_point(size = 4, colour = black_speciale)  +
  geom_node_label(aes(label = name, size = props),
                  colour = black_speciale,  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(3, 7), labels = scales::percent) +
  scale_edge_width(range = c(1, 3)) +
  labs(title = "Network and correlations of topics in structural topic model",
       subtitle = NULL,
       x = NULL,
       y = NULL,
       size = "Topic Proportion",  edge_width = "Topic Correlation") + 
  theme_speciale +
  theme(panel.grid.major = element_blank())




