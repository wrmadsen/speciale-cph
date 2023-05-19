# Count documents over time

# Create count object ----

# Count per week
count_docs <- master_dt %>%
  group_by(group, sub_group, week) %>%
  summarise(n = n()) %>%
  ungroup()

# Create rolling average
n_for_roll <- 4

count_docs <- count_docs %>%
  arrange(group, sub_group, week) %>%
  group_by(group, sub_group) %>%
  mutate(n_roll = RcppRoll::roll_mean(n, n_for_roll, fill = NA, na.rm = TRUE)) %>%
  ungroup()

# Create index
base_for_index <- as.Date("2020-04-20")

count_docs <- count_docs %>%
  group_by(group, sub_group) %>%
  mutate(n_roll_index = n_roll/n_roll[week == base_for_index],
         n_roll_index = n_roll_index*100) %>%
  ungroup()

# Plot -----

## Object with spike periods ----
spike_periods <- tibble("spike_no" = c(1:6),
                        "date_min" = as.Date(c("2020-10-27", "2021-07-01", "2021-11-01", "2022-03-01", "2022-07-01",
                                               "2022-11-01")),
                        "date_max" = as.Date(c("2021-02-10", "2021-10-05", "2022-01-15", "2022-04-20", "2022-10-01",
                                               "2023-02-01")),
                        "text" = c("Touadéra wins election.",
                                   "Republican Dialogue.",
                                   "Rebels leave Dialogue.",
                                   "Russia's invasion",
                                   "Judge Darlan fired.",
                                   "US influence."
                        )) %>%
  filter(spike_no %in% c(1, 2, 4, 5)) %>%
  mutate(date_middle = as.Date((as.numeric(date_max) + as.numeric(date_min))/2, origin = '1970-01-01'))

## Number of documents per week ----
count_docs %>%
  filter(week >= as.Date("2020-01-01")) %>%
  ggplot() +
  geom_line(aes(x = week,
                y = n_roll_index,
                colour = sub_group,
                linetype = sub_group), linewidth = 1.5) +
  # Spike periods
  geom_rect(data = spike_periods,
            aes(xmin = date_middle,
                xmax = date_middle + days(2),
                ymin = -Inf,
                ymax = 550),
            fill = "grey70") +
  geom_text(data = spike_periods,
            aes(x = date_middle,
                y = 610,
                label = stringr::str_wrap(text, 13)),
            family = theme_font,
            size = 5) +
  # Labels
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "6 months") +
  scale_y_continuous(limits = c(0, 620)) +
  labs(title = "Index of number of articles per week for CAR media outlets",
       subtitle = "4-week rolling average and index. Index 100 is April 2020.",
       y = NULL,
       x = NULL
  ) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/analysis_figure_n_docs_per_week_labelled.png", width = 30)


## Documents per year -----
master_dt %>%
  group_by(sub_group, year) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = year,
             y = n)) +
  geom_point(aes(colour = sub_group), size = 3) +
  geom_line(aes(colour = sub_group,
                linetype = sub_group), linewidth = 1.5) +
  geom_vline(xintercept = 2020) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_continuous(breaks = seq(2016, 2023, 1)) +
  labs(title = "Number of articles per year for CAR media outlets",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = "Source: William Rohde Madsen."
  ) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/method_articles_per_year.png", width = 30)


## In numbers ----

# Per sub_group
master_dt %>%
  group_by(sub_group) %>%
  summarise(n = n())

# Per year
master_dt %>%
  group_by(sub_group, year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = n)



# # Read text related to spikes ----
# # May 2020
# rows_to_read_as_txt <- master_dt %>%
#   tibble() %>%
#   filter(sub_group == "Radio Lengo Songo") %>%
#   filter(date > as.Date("2021-02-01") & date < as.Date("2021-03-15")) %>%
#   select(document, date, text)
# 
# nrow(rows_to_read_as_txt)
# 
# # Save as txt
# # Add empty rows between existing rows
# rows_to_read_as_txt %>%
#   split(rows_to_read_as_txt$document) %>%
#   Map(rbind, ., NA) %>%
#   do.call(rbind, .) %>%
#   mutate(id = rep(rows_to_read_as_txt$document, each = 2)) #%>%
# #write.table(., file = "output/my_data.txt", sep = "")






