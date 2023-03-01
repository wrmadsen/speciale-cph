# Count documents over time

# Create count object ----
# Count
count_docs <- master_dt %>%
  select(-date) %>%
  rename(date = week) %>%
  group_by(group, sub_group, date) %>%
  summarise(n = n()) %>%
  ungroup()

# Add "total" rows to
count_docs <- count_docs %>%
  filter(sub_group != "Radio Lengo Songo") %>%
  mutate(sub_group = "Non-Russian total") %>%
  group_by(group, sub_group, date) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  bind_rows(count_docs)

# Create rolling average
n_for_roll <- 4

count_docs <- count_docs %>%
  arrange(group, sub_group, date) %>%
  group_by(group, sub_group) %>%
  mutate(n_roll = RcppRoll::roll_mean(n, n_for_roll, fill = NA, na.rm = TRUE)) %>%
  ungroup()

# Create index
base_for_index <- as.Date("2020-04-20")

count_docs <- count_docs %>%
  group_by(group, sub_group) %>%
  mutate(n_roll_index = n_roll/n_roll[date == base_for_index],
         n_roll_index = n_roll_index*100) %>%
  ungroup()

# Plot -----

## Number of documents over time ----

# Object with spike periods
spike_periods <- tibble("spike_no" = c(1:6),
                        "date_min" = as.Date(c("2020-10-27", "2021-07-01", "2021-11-01", "2022-03-01", "2022-07-01",
                                               "2022-11-01")),
                        "date_max" = as.Date(c("2021-02-10", "2021-10-05", "2022-01-15", "2022-04-20", "2022-10-01",
                                               "2023-02-01")),
                        "text" = c("1ST:\nFB battle between FR-RUS and Touadera wins elex.",
                                   "2ND:\nRepublican dialogue, UN vote on embargo, and ceasefire.",
                                   "3RD: Rebels impose demands and then leave dialogue.",
                                   "4TH:\nRussia invades Ukraine.",
                                   "5TH: Project Sango, 'bloc republicain', constitutional reform, Darlan fired, and US diplomacy.",
                                   "6TH: US diplomacy and treasury crisis.")) %>%
  mutate(date_middle = as.Date((as.numeric(date_max) + as.numeric(date_min))/2, origin = '1970-01-01'))

# Save spike periods object
# To add to master objects from earlier
# for later analysis
#spike_periods %>% write_csv(., "output/spike_periods.csv")

# Plot
count_docs %>%
  filter(date >= as.Date("2020-01-01")) %>%
  ggplot() +
  geom_line(aes(x = date,
                y = n_roll_index, colour = sub_group), linewidth = 2) +
  geom_smooth(aes(x = date,
                  y = n_roll_index,
                  colour = sub_group), se = FALSE, linetype = 2) +
  geom_rect(data = spike_periods,
            aes(xmin = date_min,
                xmax = date_max,
                ymin = -Inf,
                ymax = Inf),
            alpha = 0.12,
            fill = orange_speciale) +
  geom_text(data = spike_periods,
            aes(x = date_middle,
                y = 460,
                label = stringr::str_wrap(text, 12)),
            family = theme_font,
            size = 6) +
  scale_colour_manual(name = "", values = colours_groups) +
  scale_x_date(labels = dateformat(), date_breaks = "4 months") +
  labs(title = "Index of number of documents over time for CAR media outlets (100 = April 2020)",
       subtitle = "Index. Central African Republic media outlets.",
       y = "Index",
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale

save_plot_speciale("output/n_docs_per_week.png")


## Read text related to spikes ----
# May 2020
rows_to_read_as_txt <- master_dt %>%
  tibble() %>%
  filter(sub_group == "Radio Lengo Songo") %>%
  filter(date > as.Date("2021-02-01") & date < as.Date("2021-03-15")) %>%
  select(document, date, text)

nrow(rows_to_read_as_txt)

# Save as txt
# Add empty rows between existing rows
rows_to_read_as_txt %>%
  split(rows_to_read_as_txt$document) %>%
  Map(rbind, ., NA) %>%
  do.call(rbind, .) %>%
  mutate(id = rep(rows_to_read_as_txt$document, each = 2)) %>%
  write.table(., file = "output/my_data.txt", sep = "")






