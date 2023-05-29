# Load master_stm ----
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
             topic = 3,
             texts = master_dfm$text,
             n = 1000)$docs %>%
  data.frame() %>%
  tibble() %>%
  rename(text = 1) %>%
  mutate(text_nchar = nchar(text)) %>%
  arrange(text_nchar) %>%
  filter(grepl("sport", text)) %>%
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

## Change media names ----
master_dt_thetas <- master_dt_thetas %>%
  mutate(sub_group = case_match(sub_group,
                                "Ndjoni Sango" ~ "Ndjoni Sango (RUS)",
                                "Radio Lengo Songo" ~ "Radio Lengo Songo (RUS)",
                                .default = sub_group))

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
                                 "x1" ~ "Society",
                                 "x2" ~ "Gas prices",
                                 "x3" ~ "Law and order",
                                 "x4" ~ "Russia and foreign relations",
                                 "x5" ~ "Foreign aid",
                                 "x6" ~ "Election scrutiny",
                                 "x7" ~ "Civilian victims",
                                 "x8" ~ "Justice",
                                 "x9" ~ "Covid-19",
                                 "x10" ~ "Labour resistance",
                                 "x11" ~ "Education",
                                 "x12" ~ "Bangui",
                                 "x13" ~ "Sport",
                                 "x14" ~ "Religion",
                                 "x15" ~ "Celebrations",
                                 "x16" ~ "Enemy, France, Bozize",
                                 "x17" ~ "MINUSCA",
                                 "x18" ~ "CAR army and Russians",
                                 "x19" ~ "Peace talks",
                                 "x20" ~ "Parliament",
                                 "x99" ~ "At least one condition must be supplied",
                                 .default = as.character(topic_no)))


# Save table with examples -----
# Create
data_for_table <- master_dt_thetas_long %>%
  select(-topic_no) %>%
  mutate(topic_name_new = if_else(grepl("CAR army|Sport|Civilian victims", topic_name), topic_name, "Other topics")) %>%
  select(-topic_name) %>%
  group_by(orient, sub_group, date, url, document, topic_name_new) %>%
  summarise(topic_proportion = sum(topic_proportion)) %>%
  ungroup() %>%
  mutate(topic_proportion = topic_proportion*100,
         topic_proportion = round(topic_proportion, 3)) %>%
  pivot_wider(names_from = topic_name_new, values_from = topic_proportion) %>%
  relocate(`Other topics`, .after = last_col()) %>%
  group_by(sub_group) %>%
  slice_max(order_by = date, n = 1) %>%
  slice_min(order_by = `Other topics`, n = 1) %>%
  select(-document) %>%
  ungroup()

# Save
data_for_table %>%
  arrange(desc(orient)) %>%
  select(-orient) %>%
  mutate(date = format(date, "%d %B %Y"),
         across(c(4:ncol(.)), ~round(.) %>% paste0(" %"))) %>%
  rename("Media" = sub_group, "Month" = date, "URL" = url) %>%
  select(-URL) %>%
  write.xlsx(., file = "output/table4_appendix_articles_contain_topics.xlsx")


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
          prop = round(prop, 2),
          prop = format(prop, nsmall = 2)
   ) %>%
   transmute(topic_no = row_number() %>% as.integer(),
             topic_name,
             prop,
             frex))

# Save as Excel
table_of_topics %>%
  rename("No." = topic_no, "Topic" = topic_name, `Prop. (%)` = prop, "FREX" = frex) %>%
  write.xlsx(., file = "output/table3_analysis_stm_table_of_topics.xlsx")

# Save flextable
# table_of_topics %>%
#   # To flextable
#   flextable() %>%
#   width(., width = 2) %>%
#   width(., j = "frex", width = 4) %>%
#   width(., j = "prop", width = 1) %>%
#   #border_inner_h(.) %>%
#   set_header_labels(.,
#                     topic_name = "Topic",
#                     prop = "Prop. (%)",
#                     frex = "FREX") %>%
#   save_as_docx(path = "output-tables/analysis_stm_table_of_topics.docx")

# Topic proportion total  ----
# Calculate
## Per group ----
data_for_plot_group <- master_dt_thetas_long %>%
  group_by(sub_group, topic_name, topic_no) %>%
  summarise(mean = mean(topic_proportion, na.rm = TRUE)*100) %>%
  group_by(topic_name, topic_no) %>%
  mutate(mean_to_sort_by = mean(mean, na.rm = TRUE)) %>%
  ungroup()

# Plot with points
data_for_plot_group %>%
  mutate(half = if_else(mean_to_sort_by > 4.95, "", " "),
         topic_name = fct_reorder(topic_name, mean_to_sort_by)) %>%
  ggplot(.,
         aes(x = mean,
             y = topic_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 7, stroke = 2) +
  facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Figure 21. Total mean proportion for topics of the content\nper media",
       x = "Total mean proportion of content, %",
       y = NULL) +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2)) +
  theme(plot.margin = margin(0, 1, 0, 0, "cm"))

save_plot_speciale("output/fig21_analysis_stm_prop_mean_by_group.png",
                   height = 23, width = 30)

## Per orient ------
data_for_plot_orient <- master_dt_thetas_long %>%
  group_by(orient, topic_name, topic_no) %>%
  summarise(mean = mean(topic_proportion, na.rm = TRUE)*100) %>%
  group_by(topic_name) %>%
  mutate(mean_to_sort_by = mean[orient == "Pro-Russian media"]) %>%
  ungroup() %>%
  arrange(topic_name)

# Plot with points
data_for_plot_orient %>%
  mutate(half = if_else(mean_to_sort_by > 4.95, "", " "),
         topic_name = fct_reorder(topic_name, mean_to_sort_by)) %>%
  ggplot(.,
         aes(x = mean,
             y = topic_name)) +
  geom_point(aes(shape = orient, colour = orient), size = 7, stroke = 2) +
  facet_wrap(~half, scales = "free_y") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  labs(title = "Figure 3. Total mean proportion for topics of the content by media",
       x = "Total mean proportion of content, %",
       y = NULL) +
  theme_speciale +
  guides(colour = guide_legend(nrow = 1))

save_plot_speciale("output/fig03_analysis_stm_prop_mean_by_orient.png",
                   height = 23, width = 30)

## Check numbers -----
data_for_plot_orient %>%
  filter(grepl("victim", topic_name)) %>%
  transmute(orient, topic_name, mean = paste(mean))

## Calculate average difference b.t. groups' mean -----
data_for_calc <- data_for_plot_orient %>%
  select(-c(topic_no, mean_to_sort_by))

left_join(data_for_calc, data_for_calc, by = "topic_name") %>%
  filter(orient.x != orient.y) %>%
  arrange(topic_name) %>%
  mutate(diff = mean.x - mean.y,
         diff_ab = abs(diff)) %>%
  group_by(orient.x, orient.y) %>%
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
  filter(topic_no %in% top_topics_no) %>% #distinct(topic_name) %>% %>% pull
  ggplot(aes(x = month,
             y = mean)) +
  geom_smooth(aes(colour = sub_group,
                  linetype = sub_group),
              se = FALSE, linewidth = 2) +
  #geom_vline(xintercept = as.Date("2020-12-15")) +
  facet_wrap(~topic_name, scales = "free",
             labeller = label_wrap_gen()
  ) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "16 months") +
  labs(title = "Figure 4. Mean proportion for topics of the monthly content by media outlets",
       #subtitle = "Index base is February 2022.",
       x = NULL,
       y = "Proportion of content, %") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output/fig04_analysis_stm_prop_mean_over_time.png", height = 23, width = 31)

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
  mutate(pair_name = paste0(x_name, " -- ", y_name)) %>%
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
  mutate(mean_per_pair_russia = mean(cor[sub_group %in% c("Ndjoni Sango (RUS)", "Radio Lengo Songo (RUS)")]),
         mean_per_pair_bench = mean(cor[sub_group %in% c("RJDH", "Radio Ndeke Luka")]),
         diff_russia_abs = (mean_per_pair_russia - mean_per_pair_bench) %>% abs()*100,
         diff_russia_rel_abs = ((mean_per_pair_russia - mean_per_pair_bench)/mean_per_pair_bench) %>% abs()*100
  ) %>%
  ungroup() %>%
  select(-c(cor_abs)) %>%
  select(sub_group, pair_name, cor, mean_per_pair,
         mean_per_pair_russia, diff_russia_abs, diff_russia_rel_abs)

data_for_plot_life_top <- data_for_plot_life %>%
  filter(grepl("CAR army|Civilian victims|Sport|Religion", pair_name)) %>%
  slice_max(order_by = diff_russia_rel_abs, n = 4*10)

## Plot correlation with points ----
data_for_plot_life_top %>%
  mutate(pair_name = fct_reorder(pair_name, mean_per_pair_russia),) %>%
  ggplot(.,
         aes(x = cor,
             y = pair_name)) +
  geom_point(aes(shape = sub_group, colour = sub_group), size = 7, stroke = 2) +
  geom_vline(xintercept = 0) +
  #facet_wrap(~half, scales = "free") +
  scale_color_manual(name = "", values = colours_groups) +
  scale_shape_manual(name = "", values = points_group) +
  #scale_y_discrete(labels = wrap_format(20)) +
  labs(title = "Figure 5. Correlation of topic pairs",
       subtitle = NULL, #"Selection of pairs where Russian-supported media demonstrated largest difference",
       x = "Correlation statistic (Pearson)",
       y = NULL) +
  theme_speciale +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output/fig05_analysis_stm_pairwise.png", height = 19)


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
data_for_plot_life %>%
  select(diff_russia_rel_abs) %>%
  arrange(diff_russia_rel_abs) %>%
  ggplot(.,
         aes(x = diff_russia_rel_abs)) +
  #geom_bar() +
  geom_density(linewidth = 2) +
  #scale_x_continuous(trans = "log10") +
  labs(title = "Figure 11. Distribution of absolute relative difference",
       #subtitle = "Percentages have been rounded to nearest integer.",
       x = "Absolute relative difference in correlation, %",
       y = NULL) +
  theme_speciale

save_plot_speciale("output/fig11_appendix_stm_distribution_cor.png")











