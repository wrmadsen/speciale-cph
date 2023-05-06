# Plot non-main data


# Forbes billionaires ----
forbes_raw %>%
  pivot_longer(cols = c(2:ncol(.))) %>%
  filter(grepl("billionaire wealth % national income", name)) %>%
  group_by(Year) %>%
  mutate(country = c("China", "USA", "France", "Russia", "Russia resident", "Germany")) %>%
  clean_names() %>%
  filter(country != "Russia resident") %>%
  filter(year > 1990) %>%
  ggplot(.,
         aes(x = year,
             y = value*100)) +
  geom_line(aes(colour = country), linewidth = 2) +
  scale_colour_discrete(name = "") +
  theme_speciale +
  labs(title = "Total wealth of billionaires as a share of national income",
       x = NULL,
       y = "Share of national income, %",
       caption = "Source: WID.")

save_plot_speciale("output/forbes_billionaries.png")



# ACLED conflicts data  -----

# Plot number of conflicts per month
acled %>%
  #bind_rows(acled %>% mutate(region = "Total", north = "Total")) %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  mutate(n_roll = RcppRoll::roll_mean(n, 3, fill = NA, na.rm = TRUE)) %>%
  ggplot(.,
         aes(x = month,
             y = n_roll)) +
  geom_point(aes(y = n)) +
  geom_line(colour = red_speciale, size = 2) +
  #geom_vline(xintercept = specific_events$date) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Number of conflicts per month in the CAR since 2019",
       subtitle = "3-month rolling average.",
       x = NULL,
       y = "Number per month",
       caption = "Source: ACLED.")

save_plot_speciale("output/conflicts_per_month.png")

# Plot Wagner's number of conflicts OVERALL
acled %>%
  filter(actor1 == "Wagner Group" | actor2 == "Wagner Group") %>%
  mutate(group = "Wagner Group") %>%
  bind_rows(acled %>% mutate(group = "Total")) %>%
  group_by(group, month) %>%
  summarise(n = n()) %>%
  group_by(group) %>%
  mutate(n_roll = RcppRoll::roll_mean(n, 3, fill = NA, na.rm = TRUE)) %>%
  ggplot(.,
         aes(x = month)) +
  geom_point(aes(y = n, colour = group)) +
  geom_line(aes(y = n_roll,
                colour = group), size = 2) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  scale_colour_manual(name = "",
                      values = c("Total" = red_speciale,
                                 "Wagner Group" = blued_speciale)) +
  labs(title = "Number of conflicts per month associated with the Wagner Group in the CAR since 2010",
       subtitle = "3-month rolling average.",
       x = NULL,
       y = "Number per month",
       caption = "Source: ACLED.")

save_plot_speciale("output/conflicts_per_month_wagner.png")

# Difference between Wagner and total

# Map

# Turn into sf object
acled_sf <- acled %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

# Plot with points
ggplot() +
  geom_sf(data = gadm_simp,
          colour = "darkgrey", fill = NA) +
  geom_sf(data = gadm_simp,
          colour = "black", fill = NA) +
  geom_sf(data = acled_sf,
          colour = "darkred", size = 0.3) +
  facet_wrap(~year) +
  geom_point(data = gadm_simp, aes(x = 20.73, y = 6.19), colour = "lightblue", size = 2, text = "DDD") + # Ndassima
  geom_point(data = gadm_simp, aes(x = 22.3949, y = 8.0706), colour = "lightblue", size = 2, text = "DDD") + # Damane killed
  geom_point(data = gadm_simp, aes(x = 18.5582, y = 4.3947), colour = "darkblue", size = 2, text = "DDD") + # Bangui +
  labs(title = "Conflicts across CAR per year since 2019",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major = element_blank()) +
  coord_sf(expand = FALSE, datum = NA)

save_plot_speciale("output-figures/conflicts_map_point.png")


# Heat map
# Join acled with GADM regions
#acled_gadm <- st_join(gadm_simp, acled_sf)

# First summarise ACLED data per region, then join with GADM
data_to_plot <- acled %>%
  group_by(region, halfyear) %>%
  summarise(n = n()) %>%
  group_by(halfyear) %>%
  mutate(share = n/sum(n)) %>%
  ungroup() %>%
  complete(region, halfyear, fill = list(n = 0, share = 0))

data_to_plot <- left_join(gadm_simp %>% mutate(region_1 = remove_accents(region_1)),
                          data_to_plot,
                          by = c("region_1" = "region"))

data_to_plot %>%
  ggplot() +
  geom_sf(aes(fill = share)) +
  facet_wrap(~halfyear) +
  scale_fill_continuous(trans = "log10") +
  geom_point(data = gadm_simp, aes(x = 20.73, y = 6.19), colour = "lightblue", size = 2, text = "DDD") + # Ndassima
  geom_point(data = gadm_simp, aes(x = 22.3949, y = 8.0706), colour = "lightblue", size = 2, text = "DDD") + # Damane killed
  geom_point(data = gadm_simp, aes(x = 18.5582, y = 4.3947), colour = "darkblue", size = 2, text = "DDD") + # Bangui
  labs(title = "Conflicts mapped in CAR")



# Mines coordinates -----
# Join GADM region boundaries with mine coordinates
car_mines_gadm <- st_join(car_mines, gadm_simp) %>%
  select(id = id.x, region = region_1) %>%
  mutate(id = fct_relevel(id, "Hydrocarbon", "Gold", "Diamonds"))

# Check which are NA (don't include any mines)
car_mines_gadm %>%
  filter(is.na(id))

# Bar plot
car_mines_gadm %>%
  tibble() %>%
  group_by(region, id) %>%
  summarise(n = n()) %>%
  group_by(region) %>%
  mutate(total = sum(n)) %>%
  ggplot(.,
         aes(x = n,
             y = reorder(region, total))) +
  geom_col(aes(fill = id)) +
  scale_fill_manual(name = "", values = c("Gold" = gold_speciale,
                                          "Diamonds" = bluel_speciale,
                                          "Hydrocarbon" = brown_speciale)) +
  labs(title = "Number of mines per region in the CAR per 2017",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())

save_plot_speciale("output-figures/mines_in_car_bar.png")

## Map point plot -----
ggplot() +
  geom_sf(data = gadm_simp) +
  geom_sf(data = car_mines,
          aes(colour = id),
          size = 2) +
  scale_colour_manual(name = "", values = c("Gold" = gold_speciale,
                                            "Diamonds" = bluel_speciale,
                                            "Hydrocarbon" = brown_speciale)) +
  labs(title = "Spatial distribution of mines in the CAR per 2017",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.y = element_blank()) +
  coord_sf(expand = FALSE, datum = NA)

save_plot_speciale("output-figures/mines_in_car_map_point.png")

## Map bar plot ----

## Find and add centroid x and y coordinates
europe_centroids <- st_centroid(europe_clipped$geometry)
europe_clipped_points <- cbind(europe_clipped, st_coordinates(europe_centroids))

###### Summarise followers by country
### Change certain country names (Russia)
data_for_plot <- demo_collapsed %>%
  filter(demo == "followerCountsByRegion" & company_name == company_in_loop_new) %>%
  filter(date == max(date)) %>%
  group_by(country_name) %>%
  summarise(followers_total = sum(followers_total)) %>%
  mutate(country_name = case_when(country_name == "Russian Federation" ~ "Russia",
                                  TRUE ~ as.character(country_name)))

###### Merge with followers data
### Replace NA followers with 0
data_for_plot <- merge(europe_clipped_points,
                       data_for_plot,
                       by.x = "name", by.y = "country_name",
                       all = TRUE) %>%
  filter(!is.na(admin)) %>%
  mutate(followers_total = case_when(is.na(followers_total) ~ as.integer(0),
                                     TRUE ~ as.integer(followers_total))) %>%
  arrange(-followers_total) %>%
  mutate(top_ten = if_else(row_number() <= 10, TRUE, FALSE),
         label_w_total = paste0(name, " (", followers_total, ")"))

###### Plot

# Breaks
# Create breaks for the color scale
# map_breaks <- c(0, 5, 25, 100, 250)
map_limits <- c(1, max(data_for_plot$followers_total))
total_followers_europe <- sum(data_for_plot$followers_total)

# Height of segment as index numbers
data_for_plot$segment_height <- data_for_plot$followers_total/max(data_for_plot$followers_total)*100/10


ggplot(data_for_plot) +
  geom_sf(alpha = 1, col = "white") +
  coord_sf(expand = FALSE, datum = NA) +
  #geom_point(aes(x = X, y = Y, size = followers_total), colour = colour_code) +
  geom_segment(data = data_for_plot[data_for_plot$followers_total > 0,],
               aes(x = X, y = Y-0.01, xend = X, yend = Y+segment_height+0.01),
               colour = "black", size = 7.2,
               lineend = "butt") +
  geom_segment(aes(x = X, y = Y, xend = X, yend = Y+segment_height),
               colour = colour_in_loop, size = 7,
               lineend = "butt") 




# Production mines -----
production %>%
  ggplot(.,
         aes(x = year,
             y = value,
             group = id)) +
  geom_line(aes(colour = id), size = 3) +
  facet_wrap(~id, scales = "free_y") +
  scale_colour_manual(name = "", values = c("Gold (grams)" = gold_speciale,
                                            "Diamonds (carats)" = bluel_speciale)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Production by mines in the CAR since 2016",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = "Source: William Rohde Madsen.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output-figures/mines_in_car_production.png")



