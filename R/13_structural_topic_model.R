# Structural topic models (STMs)

# Run model with covariates ----
## Fit multiple models to find optimal K ----
values_of_k <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)

# Prepare loop
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
  geom_vline(xintercept = 30, linewidth = 2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (the number of topics)",
       y = NULL,
       title = "Exclusivity and semantic coherence for topic models") +
  theme_speciale

save_plot_speciale("output-figures/exclusivity_semantic_coherence.png")

# Analyse model -----
(master_stm <- many_models_one[[5]])

#save(master_stm, file = "output/master_stm.Rdata")
load("output/master_stm.Rdata")

(number_of_topics <- master_stm$settings$dim$K)

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

names(master_stm_theta) <- paste0(1:number_of_topics)

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
# Master_dt is introduced here in this script
master_dt_thetas <- full_join(master_dt, master_stm_theta) %>%
  tibble()

# Long version
columns_to_pivot <- 1:30 %>% paste0()

master_dt_thetas_long <- master_dt_thetas %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "topic_no", values_to = "topic_proportion") %>%
  mutate(topic_no = as.integer(topic_no),
         topic_name = case_match(topic_no,
                                 16 ~ "Humanitarian concerns",
                                 22 ~ "Defense, Touadéra",
                                 20 ~ "Fake news",
                                 15 ~ "Sports",
                                 17 ~ "United Nations",
                                 3 ~ "Culture, youth",
                                 27 ~ "European Union",
                                 6 ~ "Rebels, France",
                                 11 ~ "Russian instructors",
                                 28 ~ "Election, scrutiny",
                                 24 ~ "Republican dialogue",
                                 19 ~ "Referendum, court, reform",
                                 99 ~ "At least one condition must be supplied",
                                 .default = as.character(topic_no)))

# Save those that have been named to vector for later viz
top_topics_no <- master_dt_thetas_long %>%
  distinct(topic_no, topic_name) %>%
  filter(as.character(topic_no) != topic_name) %>%
  pull(topic_no)

save(master_dt_thetas_long, file = "output/master_dt_thetas_long.Rdata")
load("output/master_dt_thetas_long.Rdata")

### Table of topics and total proportion ----
topic_labels_space

master_dt_thetas_long %>%
  group_by(topic_name) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(topic_labels_space) %>%
  arrange(-topic_proportion) %>%
  transmute(Topic = topic_name,
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
  group_by(sub_group, topic_name, topic_no) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)*100) %>%
  ungroup()

# Plot
# Top ten topics by media 
data_for_plot %>%
  #mutate(colour_of_bars = if_else(topic %in% c("Topic 13"), 1, 0) %>% as.factor) %>%
  #mutate(name = gsub("(.{23})\\_(.*)", "\\1\n\\2", name)) %>%
  #filter(!sub_group %in% c("Non-Russian total", "Pro-Russian total")) %>%
  group_by(sub_group) %>%
  slice_max(n = 10, order_by = topic_proportion) %>%
  ungroup %>%
  mutate(sub_group = as.factor(sub_group),
         topic_colour = as.factor(topic_name),
         topic_name = reorder_within(topic_name, topic_proportion, sub_group)) %>%
  ggplot(aes(x = topic_proportion,
             y = topic_name)) +
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
  mutate(half = topic_no %in% c(1:15)) %>%
  ggplot(.,
         aes(x = topic_proportion,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 4) +
  facet_wrap(~half, scales = "free_y") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean proportion for topics by media",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output-figures/topic_prop_mean_by_group.png", height = 23, width = 30)

### Topic proportion over time by group ----
# Calculate
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, month, topic_name, topic_no) %>%
  summarise(topic_proportion = mean(topic_proportion)) %>%
  ungroup()

# Plot
data_for_plot %>%
  filter(year(month) >= 2020) %>%
  filter(topic_no %in% top_topics_no) %>%
  ggplot(aes(x = month,
             y = topic_proportion)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  facet_wrap(~topic_name#, scales = "free"
             ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "12 months") +
  #scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Mean proportion of topics per month",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.key.size =  unit(0.5, "in"))

save_plot_speciale("output-figures/topic_prop_mean_over_time.png", height = 23, width = 30)


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




