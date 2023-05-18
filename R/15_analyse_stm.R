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
topic_labels_matrix_prob <- labelTopics(master_stm, n = 10)$prob

# Collapse the words for each topic into a single label
topic_labels_matrix_frex_space <- apply(topic_labels_matrix_frex, 1, paste0, collapse = ", ")
topic_labels_matrix_lift_space <- apply(topic_labels_matrix_lift, 1, paste0, collapse = ", ")
topic_labels_matrix_prob_space <- apply(topic_labels_matrix_prob, 1, paste0, collapse = ", ")

# Turn to tibble
topic_labels <- tibble(frex = topic_labels_matrix_frex_space,
                       lift = topic_labels_matrix_lift_space,
                       prob = topic_labels_matrix_prob_space) %>%
  mutate(topic_no = paste0("x", 1:20))

## Find thoughts ----
findThoughts(master_stm,
             topic = 4,
             texts = master_dfm$text,
             n = 1000)$docs %>%
  data.frame() %>%
  tibble() %>%
  rename(text = 1) %>%
  mutate(text_nchar = nchar(text)) %>%
  arrange(text_nchar) %>%
  filter(grepl("etat", text)) %>%
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
  slice_max(order_by = x4, n = 1000) %>%
  slice_sample(n = 300) %>%
  arrange(-x4) %>%
  transmute(x14, x13, x15, text) #%>% view()

## Pivot longer ----
# Long version
columns_to_pivot <- paste0("x", 1:number_of_topics)

master_dt_thetas_long <- master_dt_thetas %>%
  pivot_longer(cols = all_of(columns_to_pivot), names_to = "topic_no", values_to = "topic_proportion") %>%
  mutate(topic_name = case_match(topic_no,
                                 "x1" ~ "1 Soc., biz.",
                                 "x2" ~ "2 Gas, trans.",
                                 "x3" ~ "3 Police",
                                 "x4" ~ "4 Rus., foreign rel.",
                                 "x5" ~ "5 EU, WBG",
                                 "x6" ~ "6 Elex., const. court",
                                 "x7" ~ "7 Victims, refugees",
                                 "x8" ~ "8 Int. justice",
                                 "x9" ~ "9 Covid-19",
                                 "x10" ~ "10 Strikes, corruption",
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

# Top topics -----
top_topics_no <- c("x16", "x5", "x19",
                   "x18", "x4", "x6",
                   "x7", "x13", "x10")

(top_topics_name <- master_dt_thetas_long %>%
    distinct(topic_no, topic_name) %>%
    filter(topic_no %in% top_topics_no) %>%
    pull(topic_name))

# Table of topics and total proportion ----
(table_of_topics <- master_dt_thetas_long %>%
   group_by(sub_group, topic_name, topic_no) %>%
   summarise(prop = mean(topic_proportion, na.rm = TRUE)) %>%
   group_by(topic_name, topic_no) %>%
   summarise(prop = mean(prop)) %>%
   ungroup() %>%
   left_join(., topic_labels, by = "topic_no") %>%
   arrange(-prop) %>%
   mutate(prop = prop*100,
          prop = round(prop, 2)
   ) %>%
   select(topic_name, prop, frex))

# Save as Excel
table_of_topics %>%
  rename("Topic" = topic_name, `Prop. (%)` = prop, "FREX" = frex) %>%
  write.xlsx(., file = "output-tables/analysis_stm_table_of_topics.xlsx")

# Save flextable
table_of_topics %>%
  # To flextable
  flextable() %>%
  width(., width = 2) %>%
  width(., j = "frex", width = 4) %>%
  width(., j = "prop", width = 1) %>%
  #border_inner_h(.) %>%
  set_header_labels(.,
                    topic_name = "Topic",
                    prop = "Prop. (%)",
                    frex = "FREX") %>%
  save_as_docx(path = "output-tables/analysis_stm_table_of_topics.docx")

# Topic proportion total by group ----
# Calculate 
data_for_plot <- master_dt_thetas_long %>%
  group_by(sub_group, topic_name, topic_no) %>%
  summarise(mean_per_group = mean(topic_proportion, na.rm = TRUE)*100) %>%
  group_by(topic_name, topic_no) %>%
  mutate(mean_overall = mean(mean_per_group, na.rm = TRUE)) %>%
  ungroup()

# Plot with points
data_for_plot %>%
  mutate(half = if_else(mean_overall > 4.95, "", " "),
         topic_name = fct_reorder(topic_name, mean_overall)) %>%
  ggplot(.,
         aes(x = mean_per_group,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 6) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Total mean proportion for topics of the content by media outlets",
       x = "Total mean proportion of content, %",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output-figures/analysis_stm_prop_mean_by_group.png",
                   height = 23, width = 30)

# Check numbers
data_for_plot %>%
  filter(grepl("Enemy", topic_name))

# Calculate average difference b.t. groups' mean -----
data_for_calc <- data_for_plot %>%
  select(-c(topic_no, mean_overall))

left_join(data_for_calc, data_for_calc, by = "topic_name") %>%
  filter(sub_group.x != sub_group.y) %>%
  arrange(topic_name) %>%
  mutate(diff = mean_per_group.x - mean_per_group.y,
         diff_ab = abs(diff)) %>%
  group_by(sub_group.x, sub_group.y) %>%
  summarise(mean = mean(diff_ab))

# Topic proportion over time by group ----
# Calculate mean and index
data_for_plot <- master_dt_thetas_long %>%
  # Mean
  group_by(sub_group, month, topic_name, topic_no) %>%
  summarise(mean = mean(topic_proportion)*100) %>%
  ungroup() %>%
  # Index
  group_by(sub_group, topic_name) %>%
  mutate(index = mean/mean[month == as.Date("2020-02-01")],
         index = index*100) %>% 
  ungroup()

# Plot
data_for_plot %>%
  filter(year(month) >= 2020) %>%
  filter(topic_no %in% top_topics_no) %>%
  ggplot(aes(x = month,
             y = mean)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-12-15")) +
  facet_wrap(~topic_name, scales = "free",
             labeller = label_wrap_gen()
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "16 months") +
  labs(title = "Mean proportion for topics of the monthly content by media outlets",
       subtitle = "Index base is February 2022.",
       x = NULL,
       y = "Proportion of content, %",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/analysis_stm_prop_mean_over_time.png", height = 23, width = 31)


# Topic correlations ----
## Format and join ----
# Create object with all possible pairs but without values
data_w_cor <- master_dt_thetas_long %>%
  select(sub_group, month, document, url, name = topic_name, value = topic_proportion)

pairs_without <- full_join(data_w_cor %>% rename(x_name = name) %>% select(-value),
                           data_w_cor %>% rename(y_name = name) %>% select(-value),
                           by = c("sub_group", "document", "month")) %>%
  filter(y_name != x_name)

pairs_without <- pairs_without %>%
  distinct(x_name, y_name)

# Remove duplicates
pairs_without <- data.frame(t(apply(pairs_without, 1, sort)))

pairs_without <- unique(pairs_without) %>%
  tibble() %>%
  rename(x_name = X1, y_name = X2)

pairs_without

# Add x and y values
pairs_w_values <- full_join(data_w_cor %>% rename(x_value = value),
                            pairs_without, by = c("name" = "x_name")) %>%
  rename(x_name = name)

nrow(pairs_w_values)

pairs_w_values <- full_join(data_w_cor %>% rename(y_value = value),
                            pairs_w_values, by = c("name" = "y_name",
                                                   "sub_group", "month",
                                                   "document", "url")) %>%
  rename(y_name = name)

pairs_w_values %>% distinct(x_name, y_name)

# Remove NAs
pairs_w_values <- pairs_w_values %>%
  filter(!is.na(x_name) & !is.na(y_name))

nrow(pairs_w_values)

# Verify count, should be 190
pairs_w_values %>% distinct(x_name, y_name)

# Subset top topics
pairs_w_values <- pairs_w_values %>%
  filter(x_name %in% top_topics_name | y_name %in% top_topics_name) %>%
  mutate(pair_name = paste0(x_name, " | ", y_name)) %>%
  arrange(x_name, y_name)

pairs_w_values

## Calculate correlations ----
# Lifetime
cor_lifetime <- pairs_w_values %>%
  group_by(sub_group, pair_name) %>%
  summarise(cor = cor(x_value, y_value)) %>%
  ungroup() %>%
  mutate(cor_abs = abs(cor))

# Over time
cor_time <- pairs_w_values %>%
  mutate(time = floor_date(month, "year")) %>%
  #mutate(time = month) %>%
  group_by(sub_group, time, pair_name) %>%
  summarise(cor = cor(x_value, y_value)) %>%
  ungroup() %>%
  mutate(cor_abs = abs(cor))

## Plot lifetime, total ----
data_for_plot_life <- cor_lifetime %>%
  # Per pair, mean and max
  group_by(pair_name) %>%
  mutate(mean_per_pair = mean(cor, na.rm = TRUE),
         max_per_pair = max(cor)) %>%
  # Calculate mean of Russian pair
  # Then difference of this mean to the totot
  mutate(mean_per_pair_russia = mean(cor[sub_group %in% c("Ndjoni Sango", "Radio Lengo Songo")]),
         mean_per_pair_bench = mean(cor[sub_group %in% c("RJDH", "Radio Ndeke Luka")]),
         diff_russia_abs = (mean_per_pair_russia - mean_per_pair_bench) %>% abs()*100,
         diff_russia_rel_abs = ((mean_per_pair_russia - mean_per_pair_bench)/mean_per_pair_bench) %>% abs()*100
  ) %>%
  ungroup() %>%
  select(-c(cor_abs)) %>%
  select(sub_group, pair_name, cor, mean_per_pair,
         mean_per_pair_russia, diff_russia_abs, diff_russia_rel_abs)

data_for_plot_life_top <- data_for_plot_life %>%
  slice_max(order_by = diff_russia_rel_abs, n = 4*12) #distinct(pair_name) %>% nrow()

## Plot correlation with points ----
data_for_plot_life_top %>%
  mutate(pair_name = fct_reorder(pair_name, mean_per_pair),
         #half = if_else(mean_per_pair > 0.07, "", " ")
  ) %>%
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

save_plot_speciale("output-figures/analysis_stm_pairwise.png", height = 19)

## Plot correlation over time ----
# Data
data_for_plot_time <- cor_time %>%
  filter(pair_name %in% data_for_plot_life_top$pair_name) %>%
  #filter(grepl("FACA", pair_name)) %>%
  filter(year(time) >= 2021) %>%
  filter(year(time) < 2023) %>%
  # Index
  group_by(sub_group, pair_name) %>%
  mutate(index = cor/cor[time == as.Date("2021-07-01")],
         index = index*100)

# Plot
data_for_plot_time %>% #distinct(pair_name)
  ggplot(aes(x = time,
             y = index)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 1) +
  geom_vline(xintercept = as.Date("2020-12-15")) +
  geom_hline(yintercept = 0) +
  facet_wrap(~pair_name, scales = "free",
             labeller = label_wrap_gen()
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "12 months") +
  labs(title = "Correlation for topic pairs per month",
       subtitle = NULL,
       x = NULL,
       y = "Correlation statistic",
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/appendix_stm_cor_over_time.png", height = 23, width = 31)



# Correlation numbers -----
## Relative -----
# Top 10
data_for_plot_life %>%
  select(sub_group, pair_name, diff_russia_rel_abs) %>%
  slice_max(order_by = diff_russia_rel_abs, n = 4*10) %>%
  #summarise(diff_russia_rel_abs = mean(diff_russia_rel_abs)) %>%
  filter(diff_russia_rel_abs == max(diff_russia_rel_abs))

data_for_plot_life

# Mean among all
data_for_plot_life$diff_russia_rel_abs %>%
  mean()

data_for_plot_life %>%
  mutate(binary = diff_russia_rel_abs < 20) %>%
  group_by(binary) %>%
  summarise(n = n())

# Density
data_for_plot %>%
  arrange(diff_russia_rel_abs) %>%
  transmute(diff_russia_rel_abs = round(diff_russia_rel_abs, 0)) %>%
  ggplot(.,
         aes(x = diff_russia_rel_abs)) +
  geom_bar() +
  labs(title = "Distribution of absolute relative difference in correlation of topic pairs",
       subtitle = "Percentages have been rounded to nearest",
       x = "Absolute relative difference in correlation, %",
       y = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output-figures/appendix_stm_distribution_cor.png")










