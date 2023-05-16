# Load STM data ----
values_of_k <- c(10, 15, 20, 25, 30, 35, 40, 45)

load("data-formatted/many_models.Rdata")

# Optimal K ----
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

#save(results_of_k, file = "data-formatted/results_of_k.Rdata")
load("data-formatted/results_of_k.Rdata")

# Calculate mean
results_of_k_sub <- results_of_k %>%
  tibble() %>%
  transmute(K,
            #residuals = map_dbl(residual, "dispersion"),
            exclusivity = map_dbl(exclusivity, mean),
            #eval_heldout = map_dbl(eval_heldout, "expected.heldout"),
            semantic_coherence = map_dbl(semantic_coherence, mean))

results_of_k_sub

## Numbers 20 and 30 ----
results_of_k_sub %>%
  filter(K %in% c(20, 30)) %>%
  pivot_longer(cols = c(2,3)) %>%
  mutate(value = abs(value)) %>%
  pivot_wider(names_from = K, values_from = value) %>%
  clean_names() %>%
  mutate(x20_to_x30 = ((x30 - x20)/x30)*100,
         x30_to_x20 = ((x20 - x30)/x20)*100
  )

## Plot ----
# Facets
results_of_k_sub %>%
  transmute(K,
            #Residuals = residuals,
            #`Held-out likelihood` = eval_heldout,
            `Semantic coherence` = semantic_coherence,
            Exclusivity = exclusivity) %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -K) %>%
  ggplot(aes(x = K,
             y = Value,
             group = Metric)) +
  geom_line(aes(colour = Metric),
            linewidth = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 20, linewidth = 1.5) +
  geom_vline(xintercept = 30, linewidth = 1.5) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (the number of topics)",
       y = NULL,
       title = "Exclusivity and semantic coherence for topic models") +
  theme_speciale

save_plot_speciale("output-figures/analysis_exclusivity_semantic_coherence.png")

# Load master_stm ----
# Save and load
#(master_stm <- many_models[[3]])

#save(master_stm, file = "data-formatted/master_stm.Rdata")
load("data-formatted/master_stm.Rdata")

(number_of_topics <- master_stm$settings$dim$K)

## Extract labels ----
# Extract the matrix of words with highest frex scores
topic_labels_matrix_frex <- labelTopics(master_stm, n = 10)$frex
topic_labels_matrix_lift <- labelTopics(master_stm, n = 10)$lift

# Collapse the words for each topic into a single label
topic_labels_matrix_frex_space <- apply(topic_labels_matrix_frex, 1, paste0, collapse = ", ")
topic_labels_matrix_lift_space <- apply(topic_labels_matrix_lift, 1, paste0, collapse = ", ")

# Turn to tibble
topic_labels <- tibble(frex = topic_labels_matrix_frex_space,
                       lift = topic_labels_matrix_lift_space) %>%
  mutate(topic_no = paste0("x", 1:20))

## Find thoughts ----
findThoughts(master_stm,
             topic = 20,
             texts = master_dfm$text,
             n = 1000)$docs %>%
  data.frame() %>%
  tibble() %>%
  rename(text = 1) %>%
  mutate(text_nchar = nchar(text)) %>%
  arrange(text_nchar) %>%
  #filter(grepl("ordur", text)) %>%
  mutate(text = substr(text, 1, 300)) %>%
  #filter(grepl("loi", text)) %>%
  #filter(text_nchar < 400) %>%
  slice_sample(n = 3) %>%
  pull(text)

# Create object of thetas ----
master_stm_theta_raw <- master_stm$theta %>%
  data.frame() %>%
  tibble() %>%
  clean_names()

names(master_stm_theta_raw) <- paste0(1:number_of_topics)

## Add document number as column to thetas -----
#master_dfm %>% dfm_subset(., document == "text312") # check text400 and 1012 are empty/fully sparse
document_no_thetas <- convert(master_dfm, to = "stm")
document_no_thetas <- names(document_no_thetas$documents)

master_stm_theta <- master_stm_theta_raw %>%
  mutate(document = document_no_thetas) %>%
  select(document, everything()) %>%
  clean_names()

## Join theta values to master_dt ----
# Ensure master_dt has same documents as master_stm
# Due to the empty documents dropped during stm(), dfm2stm(x, docvars, omit_empty = TRUE)
# Master_dt is introduced here in this script
master_dt_thetas <- full_join(master_dt, master_stm_theta, by = "document") %>%
  tibble()

## Check various documents per topic ----
# Check difference between topics 18 and 7
master_dt_thetas %>%
  clean_names() %>%
  slice_max(order_by = x13, n = 1000) %>%
  slice_sample(n = 300) %>%
  arrange(-x13) %>%
  transmute(x14, x13, x15, text) #%>% view()

## Pivot longer ----
# Long version
columns_to_pivot <- paste0("x", 1:number_of_topics)

master_dt_thetas_long <- master_dt_thetas %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "topic_no", values_to = "topic_proportion") %>%
  mutate(topic_name = case_match(topic_no,
                                 "x1" ~ "1 ID card, biz., soc.",
                                 "x2" ~ "2 Gas, tax, trans.",
                                 "x3" ~ "3 Police",
                                 "x4" ~ "4 Rus., foreign rel.",
                                 "x5" ~ "5 EU, WBG",
                                 "x6" ~ "6 Elex., const. court",
                                 "x7" ~ "7 Victims, refugees",
                                 "x8" ~ "8 Int. justice",
                                 "x9" ~ "9 Covid-19",
                                 "x10" ~ "10 Strikes, unions",
                                 "x11" ~ "11 Education",
                                 "x12" ~ "12 Urban, develop.",
                                 "x13" ~ "13 Sport",
                                 "x14" ~ "14 Health, rel.",
                                 "x15" ~ "15 Culture, celeb.",
                                 "x16" ~ "16 Enemy, FR, Bozize",
                                 "x17" ~ "17 MINUSCA, rebels",
                                 "x18" ~ "18 FACA, combat",
                                 "x19" ~ "19 Rep. dialogue, Khartoum",
                                 "x20" ~ "20 Parl.",
                                 "x99" ~ "At least one condition must be supplied",
                                 .default = as.character(topic_no)))

# Save those that have been named to vector for later viz
top_topics_no <- c("x16", "x5", "x19", "x18", "x4", "x6")

top_topics_name <- master_dt_thetas_long %>%
  distinct(topic_no, topic_name) %>%
  filter(topic_no %in% top_topics_no) %>%
  pull(topic_name)

#save(master_dt_thetas_long, file = "data-formatted/master_dt_thetas_long.Rdata")
#load("data-formatted/master_dt_thetas_long.Rdata")

# Table of topics and total proportion ----
(master_dt_thetas_long %>%
  group_by(topic_name, topic_no) %>%
  summarise(topic_proportion = mean(topic_proportion, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(., topic_labels, by = "topic_no") %>%
  arrange(-topic_proportion) %>%
  #arrange(gsub("x", "", topic_no) %>% as.integer) %>%
  transmute(Topic = topic_name,
            topic_proportion = topic_proportion*100,
            topic_proportion = round(topic_proportion, 2),
            frex,
            lift) %>%
  # To flextable
  flextable() %>%
  width(., width = 2) %>%
  width(., j = "frex", width = 4.5) %>%
  width(., j = "lift", width = 4.5) %>%
  #border_inner_h(.) %>%
  set_header_labels(.,
                    topic = "Topic",
                    topic_proportion = "Prop. (%)",
                    frex = "FREX)",
                    lift = "Lift") %>%
  save_as_docx(path = "output-tables/analysis_master_dt_thetas_long.docx"))

# Topic proportion total by group ----
# Calculate 
data_for_plot <- master_dt_thetas_long %>%
  group_by(topic_name, topic_no) %>%
  mutate(mean_overall = mean(topic_proportion, na.rm = TRUE)*100) %>%
  group_by(sub_group, mean_overall, topic_name, topic_no) %>%
  summarise(mean_per_group = mean(topic_proportion, na.rm = TRUE)*100) %>%
  ungroup()

# Plot with points
data_for_plot %>%
  mutate(half = if_else(mean_overall > 4.7, "", " "),
         topic_name = fct_reorder(topic_name, mean_overall)) %>%
  ggplot(.,
         aes(x = mean_per_group,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 5) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Total mean proportion for topics of the content by media outlets",
       x = "Total mean proportion of content, %",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output-figures/analysis_topic_prop_mean_by_group.png", height = 23, width = 30)

# Topic proportion over time by group ----
# Calculate
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, month, topic_name, topic_no) %>%
  summarise(topic_proportion = mean(topic_proportion)*100) %>%
  ungroup()

# Plot
data_for_plot %>%
  filter(year(month) >= 2020) %>%
  #filter(topic_no %in% top_topics_no) %>%
  ggplot(aes(x = month,
             y = topic_proportion)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  facet_wrap(~topic_name, scales = "free",
             labeller = label_wrap_gen()
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "16 months") +
  labs(title = "Mean proportion for topics of the monthly content by media outlets",
       x = NULL,
       y = "Proportion of content, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/analysis_topic_prop_mean_over_time.png", height = 23, width = 30)


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
  ungroup() %>%
  arrange(x_name, y_name) %>%
  group_by(pair_name) %>%
  mutate(mean_per_pair = mean(cor, na.rm = TRUE),
         #mean_per_pair_abs = abs(mean_per_pair),
         #max_abs_per_pair = max(cor_abs),
         max_per_pair = max(cor),
         mean_per_pair_russia = mean(cor[sub_group %in% c("Ndjoni Sango", "Radio Lengo Songo")]),
         diff_russia = max_per_pair - mean_per_pair_russia) %>%
  ungroup() %>%
  select(-c(cor_abs))

data_for_plot

## Plot top correlations absolute per media ----
# data_for_plot %>%
#   group_by(sub_group) %>%
#   slice_max(order_by = cor, n = 5) %>%
#   mutate(sub_group = as.factor(sub_group),
#          pair_name = as.factor(pair_name),
#          pair_name_colour = pair_name,
#          colour_group = if_else(grepl("Russian|dialogue|Touadéra", pair_name), "1", "0"),
#          pair_name = reorder_within(pair_name, cor, sub_group)) %>%
#   ggplot(aes(x = cor,
#              y = pair_name)) +
#   geom_col(aes(fill = pair_name_colour), show.legend = FALSE) +
#   facet_wrap(~sub_group, scales = "free_y") +
#   #scale_fill_manual(name = "", values = c("1" = crimson_red, "0" = greenm_speciale)) +
#   scale_y_reordered() +
#   labs(title = "Correlation for 10 most frequent topics per sub group",
#        x = NULL,
#        caption = "Source: William Rohde Madsen.") +
#   theme_speciale +
#   theme(panel.grid.major.y = element_blank())


## Plot with points ----
data_for_plot %>%
  filter(x_name %in% top_topics_name | y_name %in% top_topics_name) %>%
  #filter(mean_per_pair_russia > 0) %>%
  filter(abs(diff_russia) > 0.1) %>%
  mutate(#half = if_else(mean_per_pair > 0.07, "", " "),
    pair_name = fct_reorder(pair_name, mean_per_pair)) %>%
  ggplot(.,
         aes(x = cor,
             y = pair_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 5) +
  geom_vline(xintercept = 0) +
  #facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  #scale_y_discrete(labels = wrap_format(20)) +
  labs(title = "Correlation of topic pairs in the total content by media outlet",
       subtitle = "Selection of pairs where Russian-supported media demonstrated largest difference",
       x = "Correlation statistic for topic pairs",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output-figures/analysis_pairwise.png")






