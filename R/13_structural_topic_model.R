# Structural topic models (STMs)

glimpse(master_tokens_tbl)

# Run model with covariates ----
## Fit multiple models to find optimal K ----
values_of_k <- c(10, 20, 30, 40, 50, 60, 70)
values_of_k = c(30)
n <- length(values_of_k)
many_models_one = list()
many_models_one = vector("list", length = n)

for (i in 1:n) {
  
  value_of_k <- values_of_k[i]
  
  model_in_loop <- stm(documents = master_dfm,
                       prevalence = ~sub_group,
                       K = value_of_k,
                       seed = 12345)
  
  many_models_one[[i]] <- model_in_loop
  
  runif(1, 0.1, 0.2) %>% Sys.sleep()
  
  percentage_done <- i/n
  percentage_done <- percentage_done*100
  percentage_done <- round(percentage_done, 0)
  
  paste(i, "of", n, " (", percentage_done, "% done).") %>% print()
  
}

# Save models
#save(many_models_one, file = "output/many_models_one.Rdata")
load("output/many_models_one.Rdata")

# Calculate
results_of_k <- data.frame(K = values_of_k) %>%
  mutate(topic_model = many_models_one)

heldout <- make.heldout(master_dfm)

results_of_k <- results_of_k %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, master_dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, master_dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
  select(-topic_model)

#save(results_of_k, file = "output/results_of_k.Rdata")
load("output/results_of_k.Rdata")

# Plot
# Four facets
results_of_k %>%
  transmute(K,
            #`Lower bound` = lbound,
            Exclusivity = map_dbl(exclusivity, mean),
            #Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            #`Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
  ) %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -K) %>%
  ggplot(aes(x = K, y = Value)) +
  geom_line(linewidth = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 40, linewidth = 2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (the number of topics)",
       y = NULL,
       title = "Exclusivity and semantic coherence for topic models") +
  theme_speciale

save_plot_speciale("output-figures/exclusivity_semantic_coherence.png")

## Select model -----
master_stm <- many_models_one[[4]]

number_of_topics <- master_stm$settings$dim$K

# Check model ----

# Extract the matrix of words with highest frex scores
topic_labels_matrix <- labelTopics(master_stm, n = 10)$frex

# Collapse the words for each topic into a single label
topic_labels_underscore <- apply(topic_labels_matrix, 1, paste0, collapse = "_")

topic_labels_space <- apply(topic_labels_matrix, 1, paste0, collapse = ", ")

### Create object of thetas ----
master_stm_theta <- master_stm$theta %>%
  data.frame() %>%
  tibble() %>%
  clean_names()

names(master_stm_theta) <- paste0("Topic ", str_pad(1:number_of_topics, 2, pad = "0"))

# Add document number as column to thetas
#master_dfm %>% dfm_subset(., document == "text312") # check text400 and 1012 are empty/fully sparse
document_no_thetas <- convert(master_dfm, to = "stm")
document_no_thetas <- names(document_no_thetas$documents)

master_stm_theta <- master_stm_theta %>%
  mutate(document = document_no_thetas) %>%
  select(document, everything())

# Join theta values to master_dt
# Ensure master_dt has same documents as master_stm
# Due to the empty documents dropped during stm(), dfm2stm(x, docvars, omit_empty = TRUE)
master_dt_thetas <- full_join(master_dt, master_stm_theta)

# Long version
master_dt_thetas_long <- master_dt_thetas %>%
  pivot_longer(cols = c(starts_with("Topic")), names_to = "topic", values_to = "topic_proportion")

### Table of topics and total proportion ----
topic_labels_space

master_dt_thetas_long %>%
  filter(sub_group != "Non-Russian total") %>%
  group_by(topic) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(topic_labels_space) %>%
  arrange(-topic_proportion) %>%
  transmute(Topic = topic,
            topic_proportion = topic_proportion*100,
            topic_proportion = round(topic_proportion, 2),
            topic_labels_space) %>%
  # To flextable
  flextable() %>%
  width(., width = 1.1) %>%
  width(., j = "topic_labels_space", width = 4.8) %>%
  #border_inner_h(.) %>%
  set_header_labels(.,
                    topic = "Topic",
                    topic_proportion = "Proportion (%)",
                    topic_labels_space = "Common words") #%>%
#save_as_docx(path = "output-tables/master_dt_thetas_long.docx")

### Topic proportion total by group ----
# Calculate 
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, topic) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)*100) %>%
  ungroup()

# Plot
# Top 10 by media 
data_for_plot %>%
  #mutate(colour_of_bars = if_else(topic %in% c("Topic 13"), 1, 0) %>% as.factor) %>%
  #mutate(name = gsub("(.{23})\\_(.*)", "\\1\n\\2", name)) %>%
  #filter(!sub_group %in% c("Non-Russian total", "Pro-Russian total")) %>%
  group_by(sub_group) %>%
  slice_max(n = 10, order_by = topic_proportion) %>%
  ungroup %>%
  mutate(sub_group = as.factor(sub_group),
         topic_colour = as.factor(topic),
         topic = reorder_within(topic, topic_proportion, sub_group)) %>%
  ggplot(aes(x = topic_proportion,
             y = topic)) +
  geom_col(aes(fill = topic_colour), show.legend = FALSE) +
  facet_wrap(~sub_group, scales = "free_y") +
  #scale_fill_manual(name = "", values = colours_groups) +
  scale_x_continuous(n.breaks = 3) +
  scale_y_reordered() +
  labs(title = "Mean proportion for 10 most frequent topics per sub group",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

# Plot with points
data_for_plot %>%
  mutate(half = topic %in% paste0("Topic ", str_pad(21:40, 2, pad = "0"))) %>%
  ggplot(.,
         aes(x = topic_proportion,
             y = topic)) +
  geom_point(aes(shape = sub_group), size = 4) +
  facet_wrap(~half, scales = "free_y") +
  scale_shape_manual(values = points_group) +
  labs(title = "Mean proportion for topics by media",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output-figures/topic_prop_mean_by_group.png", height = 23, width = 30)

### Topic proportion over time by group ----
# Calculate
data_for_plot <- master_dt_thetas_long %>%
  mutate(quarter = floor_date(date, unit = "quarter")) %>%
  group_by(sub_group, date = week, topic) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup()

# Plot
data_for_plot %>%
  # mutate(topic = gsub("(.{23})\\_(.*)", "\\1\n\\2", topic)) %>%
  # filter(date > as.Date("2020-01-01")) %>%
  filter(topic %in% c("Topic 10", "Topic 28", "Topic 12", "Topic 21", "Topic 06", "Topic 31")) %>%
  ggplot(aes(x = date,
             y = topic_proportion)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  facet_wrap(~topic, scales = "free") +
  scale_colour_manual(name = "", values = bw_colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "24 months") +
  #scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Mean proportion of topics per week",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.key.size =  unit(0.5, "in"))

save_plot_speciale("output-figures/topic_prop_mean_over_time.png", height = 23, width = 30)

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

#save_plot_speciale("output-figures/topic_prop_mean_over_time_specific.png")

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

#save_plot_speciale("output/topic_prop_change_over_time_first.png")

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

#save_plot_speciale("output-figures/topic_prop_change_over_time_13.png")



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

save_plot_speciale("output-figures/change_in_prop_spike_binary.png", height = 23, width = 30)

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

save_plot_speciale("output-figures/diff_in_effect_spike_binary.png")

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

save_plot_speciale("output-figures/diff_in_effect_spike_binary.png")



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




