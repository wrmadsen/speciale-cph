# Load STM data ----
values_of_k <- c(20, 30, 40, 50, 60, 70, 80, 100)

#load("data-formatted/many_models.Rdata")

# K-results ----
## Calculate ----
results_of_k <- data.frame(K = values_of_k) %>%
  mutate(topic_model = many_models)

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

# Calculate mean
results_of_k_sub <- results_of_k %>%
  tibble() %>%
  transmute(K,
            residuals = map_dbl(residual, "dispersion"),
            exclusivity = map_dbl(exclusivity, mean),
            eval_heldout = map_dbl(eval_heldout, "expected.heldout"),
            semantic_coherence = map_dbl(semantic_coherence, mean))

results_of_k_sub

## Plot ----
# Four facets
results_of_k_sub %>%
  rename(Residuals = residuals,
         `Held-out likelihood` = eval_heldout,
         `Semantic coherence` = semantic_coherence,
         Exclusivity = exclusivity) %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -K) %>%
  ggplot(aes(x = K,
             y = Value,
             group = Metric)) +
  geom_line(aes(colour = Metric),
            linewidth = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 30, linewidth = 2) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (the number of topics)",
       y = NULL,
       title = "Exclusivity and semantic coherence for topic models") +
  theme_speciale

save_plot_speciale("output-figures/exclusivity_semantic_coherence.png")

# Load master_stm ----
# Save and load
#(master_stm <- many_models[[2]])

#save(master_stm, file = "output/master_stm.Rdata")
load("output/master_stm.Rdata")

(number_of_topics <- master_stm$settings$dim$K)

## Extract labels ----
# Extract the matrix of words with highest frex scores
topic_labels_matrix_frex <- labelTopics(master_stm, n = 15)$frex
topic_labels_matrix_lift <- labelTopics(master_stm, n = 15)$lift

# Collapse the words for each topic into a single label
topic_labels_matrix_frex_space <- apply(topic_labels_matrix_frex, 1, paste0, collapse = ", ")
topic_labels_matrix_lift_space <- apply(topic_labels_matrix_lift, 1, paste0, collapse = ", ")

## Find thoughts ----
findThoughts(master_stm,
             topic = 16,
             texts = master_dfm$text,
             n = 50)$docs %>% #view()
  data.frame() %>%
  tibble() %>%
  rename(text = 1) %>%
  mutate(text = substr(text, 1, 300)) %>% print(n = 100) #view()

# Create object of thetas ----
master_stm_theta <- master_stm$theta %>%
  data.frame() %>%
  tibble() %>%
  clean_names()

names(master_stm_theta) <- paste0(1:number_of_topics)

## Add document number as column to thetas -----
#master_dfm %>% dfm_subset(., document == "text312") # check text400 and 1012 are empty/fully sparse
document_no_thetas <- convert(master_dfm, to = "stm")
document_no_thetas <- names(document_no_thetas$documents)

master_stm_theta <- master_stm_theta %>%
  mutate(document = document_no_thetas) %>%
  select(document, everything())

## Join theta values to master_dt ----
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
                                 16 ~ "Insecurity, humanitarian",
                                 # 22 ~ "Battles, embargo",
                                 # 20 ~ "Int. court",
                                 # 15 ~ "Sports",
                                 # 6 ~ "Parliament",
                                 # 18 ~ "Infrastructure",
                                 # 11 ~ "Salaries, strikes",
                                 # 5 ~ "Religious",
                                 # 25 ~ "Rep. dialogue",
                                 # 4 ~ "Policing, rights",
                                 # 3 ~ "Culture",
                                 # 27 ~ "European Union, aid",
                                 # 1 ~ "Finance, budget",
                                 # 10 ~ "France, USA",
                                 # 21 ~ "Sanctions",
                                 # 2 ~ "Int. court, Hassan",
                                 # 23 ~ "NA", # NOT SURE
                                 # 8 ~ "Agri.",
                                 # 9 ~ "Families",
                                 # 24 ~ "Bangassou", # NOT SURE
                                 # 13 ~ "Sanitary",
                                 # 17 ~ "UN",
                                 # 30 ~ "Gas prices",
                                 # 26 ~ "Schooling",
                                 # 7 ~ "Vehicles",
                                 # 28 ~ "Elections",
                                 # 19 ~ "Opp., const. reform",
                                 # 29 ~ "Rebels",
                                 # 14 ~ "Russian in.",
                                 # 12 ~ "Living cond.",
                                 99 ~ "At least one condition must be supplied",
                                 .default = as.character(topic_no)))

# Save those that have been named to vector for later viz
top_topics_no <- master_dt_thetas_long %>%
  distinct(topic_no, topic_name) %>%
  filter(as.character(topic_no) != topic_name) %>%
  pull(topic_no)

#save(master_dt_thetas_long, file = "output/master_dt_thetas_long.Rdata")
#load("output/master_dt_thetas_long.Rdata")

# Table of topics and total proportion ----
topic_labels_matrix_frex_space

master_dt_thetas_long %>%
  group_by(topic_name) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(topic_labels_matrix_frex_space,
         topic_labels_matrix_lift_space) %>%
  arrange(-topic_proportion) %>%
  transmute(Topic = topic_name,
            topic_proportion = topic_proportion*100,
            topic_proportion = round(topic_proportion, 2),
            topic_labels_matrix_frex_space,
            topic_labels_matrix_lift_space) %>%
  # To flextable
  flextable() %>%
  width(., width = 1.1) %>%
  width(., j = "topic_labels_matrix_frex_space", width = 4.8) %>%
  width(., j = "topic_labels_matrix_lift_space", width = 4.8) %>%
  #border_inner_h(.) %>%
  set_header_labels(.,
                    topic = "Topic",
                    topic_proportion = "Proportion (%)",
                    topic_labels_matrix_frex_space = "Common words (FREX)",
                    topic_labels_matrix_lift_space = "Common words (Lift)") #%>%
#save_as_docx(path = "output-tables/master_dt_thetas_long.docx")

# Topic proportion total by group ----
# Calculate 
data_for_plot <- master_dt_thetas_long %>%
  group_by(topic_name, topic_no) %>%
  mutate(mean_overall = mean(topic_proportion, na.rm = TRUE)*100) %>%
  group_by(sub_group, mean_overall, topic_name, topic_no) %>%
  summarise(mean_per_group = mean(topic_proportion, na.rm = TRUE)*100) %>%
  ungroup()

# Plot
# Top ten topics by media 
data_for_plot %>%
  #mutate(colour_of_bars = if_else(topic %in% c("Topic 13"), 1, 0) %>% as.factor) %>%
  #mutate(name = gsub("(.{23})\\_(.*)", "\\1\n\\2", name)) %>%
  #filter(!sub_group %in% c("Non-Russian total", "Pro-Russian total")) %>%
  group_by(sub_group) %>%
  slice_max(n = 13, order_by = mean_per_group) %>%
  ungroup %>%
  mutate(sub_group = as.factor(sub_group),
         topic_colour = as.factor(topic_name),
         topic_name = reorder_within(topic_name, mean_per_group, sub_group)) %>%
  ggplot(aes(x = mean_per_group,
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
  mutate(half = if_else(mean_overall > 3, "", " "),
         topic_name = fct_reorder(topic_name, mean_overall)) %>%
  ggplot(.,
         aes(x = mean_per_group,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 4) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean proportion for topics by media",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output-figures/topic_prop_mean_by_group.png", height = 23, width = 30)

# Topic proportion over time by group ----
# Calculate
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, month, topic_name, topic_no) %>%
  summarise(topic_proportion = mean(topic_proportion)*100) %>%
  ungroup()

# Plot
(topics_to_filter <- master_dt_thetas_long %>%
    filter(topic_no %in% c(16, 22, 20, 15, 6, 25, 2, 28, 19, 14, 29)) %>%
    distinct(topic_name) %>%
    pull(topic_name) %>%
    paste0(., collapse = "|"))

data_for_plot %>%
  filter(year(month) >= 2020) %>%
  filter(grepl(topics_to_filter, topic_name)) %>%
  ggplot(aes(x = month,
             y = topic_proportion)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  facet_wrap(~topic_name, scales = "free"
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "16 months") +
  #scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Mean proportion of topics per month",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.key.size =  unit(0.5, "in"))

save_plot_speciale("output-figures/topic_prop_mean_over_time.png", height = 23, width = 30)


# Topic correlations ----
# Format and join
data_w_cor <- master_dt_thetas_long %>%
  select(sub_group, document, y_name = topic_name, y_value = topic_proportion)

data_w_cor <- full_join(data_w_cor,
                        data_w_cor %>% rename(x_name = y_name, x_value = y_value),
                        by = c("sub_group", "document")) %>%
  filter(y_name != x_name)

# Calculate correlations
data_w_cor <- data_w_cor %>%
  select(sub_group, x_name, y_name, x_value, y_value) %>%
  group_by(sub_group, x_name, y_name) %>%
  summarise(cor = cor(x_value, y_value)) %>%
  ungroup() %>%
  mutate(cor_abs = abs(cor))

nrow(data_w_cor)

# Remove duplicates based sub_group and x and y names
remove_duplicates_based_on_alpha_order <- function(input){
  
  input %>%
    dplyr::mutate(normalized = purrr::map2_chr(x_name, y_name, ~paste(sort(c(.x, .y)), collapse = ""))) %>%
    dplyr::group_by(normalized) %>%
    dplyr::summarise(x_name = dplyr::first(x_name),
                     y_name = dplyr::first(y_name)) %>%
    dplyr::select(-normalized)
  
}

data_w_cor <- data_w_cor %>%
  group_by(sub_group, cor, cor_abs) %>%
  group_modify(~remove_duplicates_based_on_alpha_order(.)) %>%
  ungroup()

nrow(data_w_cor)

# Verify pair count
# 29
data_w_cor %>%
  group_by(sub_group, y_name) %>%
  summarise(n = n())

data_for_plot <- data_w_cor %>%
  mutate(pair_name = paste0(x_name, " | ", y_name)) %>%
  filter(grepl(topics_to_filter, pair_name)) %>%
  filter(!grepl("NA", pair_name)) %>%
  group_by(pair_name, x_name, y_name) %>%
  mutate(mean_overall = mean(cor, na.rm = TRUE)) %>%
  group_by(sub_group, mean_overall, pair_name, x_name, y_name) %>%
  ungroup() %>%
  arrange(x_name, y_name) %>%
  mutate(mean_overall_abs = abs(mean_overall))

data_for_plot

## Plot top correlations absolute per media ----
data_for_plot %>%
  group_by(sub_group) %>%
  slice_max(order_by = cor_abs, n = 10) %>%
  mutate(sub_group = as.factor(sub_group),
         pair_name = as.factor(pair_name),
         pair_name_colour = pair_name,
         colour_group = if_else(grepl("Russian|dialogue|Touadéra", pair_name), "1", "0"),
         pair_name = reorder_within(pair_name, cor, sub_group)) %>%
  ggplot(aes(x = cor,
             y = pair_name)) +
  geom_col(aes(fill = colour_group), show.legend = FALSE) +
  facet_wrap(~sub_group, scales = "free_y") +
  scale_fill_manual(name = "", values = c("1" = crimson_red, "0" = greenm_speciale)) +
  scale_y_reordered() +
  labs(title = "Correlation for 10 most frequent topics per sub group",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())


## Plot with points ----
data_for_plot %>%
  filter(!grepl("NA|Bangassou", pair_name)) %>%
  filter(grepl(topics_to_filter, pair_name)) %>%
  #filter(mean_overall > 0) %>%
  mutate(half = if_else(mean_overall > 0, "", " "),
         pair_name = fct_reorder(pair_name, mean_overall)) %>%
  ggplot(.,
         aes(x = cor,
             y = pair_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 5) +
  facet_wrap(~x_name, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Mean sentiment for topics by media",
       x = "Mean sentiment",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale







