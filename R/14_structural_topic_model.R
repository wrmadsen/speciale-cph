# Structural topic models (STMs)

glimpse(master_tokens_tbl)

# Run preliminary models ----
# Fit
# stm_out <- stm(documents = master_dfm,
#                K = 15,
#                seed = 12345)

# Save model
#save(stm_out, file = "output/stm_out.Rdata")
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

## Fit model two -----
docvars(master_dfm) %>% names

stm_out_prevalence_two <- stm(documents = master_dfm,
                              prevalence = ~sub_group*spike_binary,
                              K = 15,
                              seed = 12345)

# Save model
save(stm_out_prevalence_two, file = "output/stm_out_prevalence_two.Rdata")
load("output/stm_out_prevalence_two.Rdata")

# Check models ----

# Model to check
stm_to_check <- stm_out_prevalence_one

# Extract the matrix of words with highest frex scores
topic_labels_matrix <- labelTopics(stm_to_check, n = 10)$frex

# Collapse the words for each topic into a single label
topic_labels_underscore <- apply(topic_labels_matrix, 1, paste0, collapse = "_")

topic_labels_space <- apply(topic_labels_matrix, 1, paste0, collapse = ", ")

### Create object of thetas ----
stm_to_check_theta <- stm_to_check$theta %>%
  data.frame() %>%
  tibble() %>%
  clean_names()

#names(stm_out_prevalence_one_theta) <- paste0("topic_", str_pad(1:15, 2, pad = "0"), "_", topic_labels)
names(stm_to_check_theta) <- paste0("Topic ", str_pad(1:15, 2, pad = "0"))

# Join theta values to master_dt
master_dt_thetas <- master_dt %>%
  bind_cols(stm_to_check_theta)

# Long version
master_dt_thetas_long <- master_dt_thetas %>%
  pivot_longer(cols = c(starts_with("Topic")), names_to = "topic", values_to = "topic_proportion")

### Table of topics and total proportion ----
topic_labels_space

master_dt_thetas_long %>%
  filter(sub_group != "Non-Russian total") %>%
  group_by(topic) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup() %>%
  mutate(topic_labels_space) %>%
  transmute(Topic = topic, `Proportion (%)` = topic_proportion*100,
            `Common words` = topic_labels_space) %>%
  # To table
  gt::gt() %>%
  gt::tab_header(title = "Topics identified by structural topic model (STM)") %>%
  gt::fmt_number(columns = `Proportion (%)`,
                 decimals = 2) %>%
  gt::gtsave("output/table_topic_proportion.png")

### Topic proportion total by group ----
# Calculate 
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, topic) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup()

# Plot
data_for_plot %>%
  mutate(colour_of_bars = if_else(topic %in% c("Topic 13"), 1, 0) %>% as.factor) %>%
  #mutate(name = gsub("(.{23})\\_(.*)", "\\1\n\\2", name)) %>%
  #filter(sub_group %in% c("Radio Lengo Songo", "Non-Russian total")) %>%
  ggplot(aes(x = topic_proportion,
             y = sub_group)) +
  geom_col(aes(fill = sub_group)) +
  facet_wrap(~topic, scales = "free") +
  scale_fill_manual(name = "", values = colours_groups) +
  scale_x_continuous(n.breaks = 3) +
  labs(title = "Mean proportion of topics by sub group",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/topic_prop_mean_by_group.png", height = 23, width = 30)

### Topic proportion over time by group ----
# Calculate
data_for_plot <- master_dt_thetas_long %>%
  mutate(quarter = floor_date(date, unit = "quarter")) %>%
  group_by(sub_group, date = week, topic) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup()

# Plot
data_for_plot %>%
  mutate(topic = gsub("(.{23})\\_(.*)", "\\1\n\\2", topic)) %>%
  filter(date > as.Date("2020-01-01")) %>%
  filter(sub_group %in% c("Radio Lengo Songo", "Ndjoni Sango", "Non-Russian total")) %>%
  ggplot(aes(x = date,
             y = topic_proportion)) +
  #geom_point(aes(colour = sub_group), alpha = 0.1) +
  geom_smooth(aes(colour = sub_group), se = FALSE, linewidth = 2) +
  facet_wrap(~topic, scales = "free") +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "12 months") +
  #scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Mean proportion of topics per week",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

#save_plot_speciale("output/topic_prop_mean_over_time.png", height = 23, width = 30)

### Plot topics 5 and 11 ----
# Vertical lines
vertical_lines <- bind_rows(tibble("date" = as.Date(c("2021-08-15", "2021-12-05", "2022-09-01"))) %>%
                              mutate(topic = "Topic 05"),
                            tibble("date" = as.Date(c("2021-07-01", "2022-08-01"))) %>%
                              mutate(topic = "Topic 11")
)

# Plot prop per week
data_for_plot %>%
  filter(date >= as.Date("2020-01-01")) %>%
  filter(topic %in% c("Topic 05", "Topic 11")) %>%
  ggplot(aes(x = date,
             y = topic_proportion)) +
  geom_smooth(aes(colour = sub_group), se = FALSE, linewidth = 2) +
  #geom_vline(data = vertical_lines, linewidth = 1.5, colour = red_speciale, linetype = 2,
  #           aes(xintercept = date)) +
  facet_wrap(~topic, scales = "free") +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "6 months") +
  labs(title = "Mean proportion of topics 5 and 11 per week",
       x = NULL,
       y = "Topic proportion, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

#save_plot_speciale("output/topic_prop_mean_over_time_specific.png")

# Plot change per month
# Topic 05
data_for_plot_change <- data_for_plot %>%
  filter(date >= as.Date("2021-05-01")) %>%
  arrange(sub_group, topic, date) %>%
  group_by(sub_group, topic) %>%
  mutate(change = topic_proportion - lag(topic_proportion),
         change_rel = change/topic_proportion*100) %>%
  ungroup()

data_for_plot_change %>%
  filter(topic %in% c("Topic 05")) %>%
  ggplot(aes(x = date,
             y = change)) +
  geom_smooth(aes(colour = sub_group), se = FALSE) +
  #geom_col(aes(fill = sub_group)) +
  geom_vline(data = vertical_lines %>% filter(topic == "Topic 06"),
             linewidth = 1.5, colour = red_speciale, linetype = 2,
             aes(xintercept = date)) +
  facet_wrap(~sub_group, scales = "free") +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "6 months") +
  labs(title = "Change in proportion of topic 5 per week",
       x = NULL,
       y = "Change in proportion, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/topic_prop_change_over_time_first.png")

# Topic 11
data_for_plot_change %>%
  filter(topic %in% c("Topic 11")) %>%
  ggplot(aes(x = date,
             y = change_rel)) +
  geom_col(aes(fill = sub_group)) +
  geom_vline(data = vertical_lines %>% filter(topic == "Topic 13"),
             linewidth = 1.5, colour = red_speciale, linetype = 2,
             aes(xintercept = date)) +
  facet_wrap(~sub_group, scales = "free") +
  scale_fill_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "6 months") +
  labs(title = "Change in proportion of topic 13 per week",
       x = NULL,
       y = "Change in proportion, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/topic_prop_change_over_time_13.png")




### Difference in proportion spike_binary ------
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, topic, spike_binary) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup() %>%
  pivot_wider(names_from = spike_binary, values_from = topic_proportion) %>%
  clean_names() %>%
  mutate(difference = x1 - x0)

data_for_plot %>%
  ggplot(aes(x = difference,
             y = sub_group)) +
  geom_col(aes(fill = sub_group)) +
  facet_wrap(~topic) +
  scale_fill_manual(name = "", values = colours_groups) +
  labs(title = "Change in topic proportion for groups during spike periods",
       x = "Change during spike periods, % points",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/change_in_prop_spike_binary.png", height = 23, width = 30)

# Collapse the words for each topic into a single label
topic_labels <- apply(topic_labels_matrix, 1, paste0, collapse = "_") %>%
  paste0(str_pad(1:15, 2, pad = "0"), "_", .) %>%
  gsub("(.{18})\\_(.*)", "\\1\n\\2", .)

topic_labels_tibble <- topic_labels %>%
  tibble("topic_name" = .) %>%
  mutate(topic = row_number() %>% as.factor())

# Estimate effects of covariates
# Effect of sub_group
# 1:15 indicate the topic, thus all are included with 1:15
prevalence_effects <- estimateEffect(formula = c(1:15) ~ sub_group*spike_binary, 
                                     stmobj = stm_to_check,
                                     metadata = docvars(master_dfm))


### Plot interaction effect between spike_binary and sub_group ----
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

### Plot interaction effect between specific spike period and sub_group ----
calculate_diff_in_spike_period <- function(spike_period = 1){
  
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
  
}
  
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
    labs(title = "Difference in effect of spike period",
         x = NULL,
         caption = "Source: William Rohde Madsen.") +
    theme_speciale
  
  save_plot_speciale("output/diff_in_effect_spike_binary.png")
  
  
  
  ### Calculate probability of each word by topic ----
  # Using tidytext?
  # library(tidytext)
  stm_to_check_theta %>% tidy()
  
  td_gamma <- tidy(stm_to_check_theta, matrix = "gamma",
                   document_names = rownames(out$meta))
  
  td_gamma
  
  
  
  ## Topic correlations network ----
  set.seed(381)
  
  mod_out_corr <- topicCorr(stm_to_check_theta)
  
  plot(mod_out_corr, vlabels = topic_labels)
  
  # extract network
  library(stminsights)
  
  stm_corrs <- stminsights::get_network(model = stm_to_check_theta,
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
  
  
  
  
  