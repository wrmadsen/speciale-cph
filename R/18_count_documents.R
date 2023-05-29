# Count documents over time

# Create count object ----

# Change media names
master_dt <- master_dt %>%
  mutate(sub_group = case_match(sub_group,
                                "Ndjoni Sango" ~ "Ndjoni Sango (RUS)",
                                "Radio Lengo Songo" ~ "Radio Lengo Songo (RUS)",
                                .default = sub_group))

# Count per week
count_docs <- master_dt %>%
  group_by(group, sub_group, week) %>%
  summarise(n = n()) %>%
  ungroup()

# Create rolling average
n_for_roll <- 5

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
spike_periods <- tibble("date_middle" = as.Date(c("2020-12-27",
                                                  "2021-09-15",
                                                  "2022-02-24",
                                                  "2022-11-20")),
                        "text" = c("Touadéra wins re-election.",
                                   "Republican Dialogue.",
                                   "Russia's invasion",
                                   "Judge Darlan fired."
                        ))

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
            aes(xmin = date_middle - days(2),
                xmax = date_middle + days(2),
                ymin = -Inf,
                ymax = 550),
            fill = "grey90") +
  geom_text(data = spike_periods,
            aes(x = date_middle,
                y = 610,
                label = stringr::str_wrap(text, 13)),
            family = theme_font,
            size = 6) +
  # Labels
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_date(labels = dateformat(), date_breaks = "6 months") +
  scale_y_continuous(limits = c(0, 620)) +
  labs(title = "Figure 2. Index of number of articles per week for CAR media outlets",
       #subtitle = "4-week rolling average and index. Index 100 is April 2020.",
       y = NULL,
       x = NULL
  ) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output/fig02_analysis_figure_n_docs_per_week_labelled.png", width = 30)


## Documents per year -----
master_dt %>%
  group_by(sub_group, year) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = year,
             y = n)) +
  geom_point(aes(colour = sub_group), size = 3, show.legend = FALSE) +
  geom_line(aes(colour = sub_group,
                linetype = sub_group), linewidth = 1.5) +
  #geom_vline(xintercept = 2020) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_linetype_manual(name = "", values = lines_group) +
  scale_x_continuous(breaks = seq(2016, 2023, 1)) +
  labs(title = "Figure 1. Number of articles per year for CAR media outlets",
       subtitle = NULL,
       y = NULL,
       x = NULL
  ) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

save_plot_speciale("output/fig01_method_articles_per_year.png", width = 30)


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




