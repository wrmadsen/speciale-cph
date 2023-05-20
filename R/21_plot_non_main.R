# Plot non-main data


# Forbes billionaires ----
forbes_raw %>%
  pivot_longer(cols = c(2:ncol(.))) %>%
  filter(grepl("billionaire wealth % national income", name)) %>%
  group_by(Year) %>%
  mutate(country = c("China", "USA", "France", "Russia", "Russia resident", "Germany")) %>%
  clean_names() %>%
  filter(!country %in% c("Russia resident", "France", "Germany")) %>%
  filter(year > 1990) %>%
  ggplot(.,
         aes(x = year,
             y = value*100)) +
  geom_line(aes(colour = country, linetype = country), linewidth = 2) +
  scale_colour_manual(name = "", values = c("Germany" = blued_speciale,
                                            "China" = orange_speciale,
                                            "Russia" = redd_speciale,
                                            "USA" = blued_speciale,
                                            "France" = gold_speciale)) +
  scale_linetype_discrete(name = "") +
  labs(title = "Figure X. Total wealth of billionaires as a share of national income",
       x = NULL,
       y = "Share of national income, %",
       caption = "Source: WID.") +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output/appendix_forbes_billionaries.png")


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
  geom_point(aes(y = n), colour = blued_speciale, size = 3) +
  geom_line(colour = blued_speciale, size = 2) +
  #geom_vline(xintercept = specific_events$date) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Figure X. Number of conflicts per month in the CAR since 2019",
       subtitle = "3-month rolling average.",
       x = NULL,
       y = "Number per month",
       caption = "Source: ACLED.")

save_plot_speciale("output/appendix_conflicts_per_month_total.png")

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
                      values = c("Total" = blued_speciale,
                                 "Wagner Group" = redd_speciale)) +
  labs(title = "Figure X. Number of conflicts per month associated with the Wagner Group in the CAR since 2010",
       subtitle = "3-month rolling average.",
       x = NULL,
       y = "Number per month",
       caption = "Source: ACLED.")

save_plot_speciale("output/appendix_conflicts_per_month_wagner.png")

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
          colour = blued_speciale, size = 0.8) +
  facet_wrap(~year) +
  # Point and label for Bangui
  geom_point(data = gadm_simp,
             aes(x = 18.5582, y = 4.3947),
             colour = "black", shape = 21, size = 3) +
  geom_text_repel(data = gadm_simp %>% filter(region_1 == "Bangui"),
                  aes(x = 18.5582, y = 4.3947, label = "Bangui, the capital"),
                  family = theme_font,
                  size = 4,
                  nudge_x = 2.5,
                  nudge_y = -0.7,
                  min.segment.length = 0.1) +
  # Theme
  labs(title = "Figure X. Conflicts across CAR per year since 2019",
       subtitle = "Each small dot shows the location of a single conflict event.",
       y = NULL,
       x = NULL) +
  theme_speciale +
  theme(panel.grid.major = element_blank()) +
  coord_sf(expand = FALSE, datum = NA)

save_plot_speciale("output/appendix_conflicts_total_map_point.png")


# Heat map
# Join acled with GADM regions
#acled_gadm <- st_join(gadm_simp, acled_sf)

# First summarise ACLED data per region, then join with GADM
# data_to_plot <- acled %>%
#   group_by(region, halfyear) %>%
#   summarise(n = n()) %>%
#   group_by(halfyear) %>%
#   mutate(share = n/sum(n)) %>%
#   ungroup() %>%
#   complete(region, halfyear, fill = list(n = 0, share = 0))
# 
# data_to_plot <- left_join(gadm_simp %>% mutate(region_1 = remove_accents(region_1)),
#                           data_to_plot,
#                           by = c("region_1" = "region"))
# 
# data_to_plot %>%
#   ggplot() +
#   geom_sf(aes(fill = share)) +
#   facet_wrap(~halfyear) +
#   scale_fill_continuous(trans = "log10") +
#   geom_point(data = gadm_simp, aes(x = 20.73, y = 6.19), colour = "lightblue", size = 2, text = "DDD") + # Ndassima
#   geom_point(data = gadm_simp, aes(x = 22.3949, y = 8.0706), colour = "lightblue", size = 2, text = "DDD") + # Damane killed
#   geom_point(data = gadm_simp, aes(x = 18.5582, y = 4.3947), colour = "darkblue", size = 2, text = "DDD") + # Bangui
#   labs(title = "Conflicts mapped in CAR")



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
  labs(title = "Figure X. Number of mines per type and region in the CAR per 2017",
       subtitle = NULL,
       y = NULL,
       x = NULL) +
  theme_speciale +
  theme(panel.grid.major.y = element_blank())

save_plot_speciale("output/appendix_mines_in_car_bar.png")

## Map point plot -----
ggplot() +
  geom_sf(data = gadm_simp,
          fill = NA) +
  geom_sf(data = car_mines,
          aes(colour = id),
          size = 3) +
  # Point and label for Bangui
  geom_point(data = gadm_simp, aes(x = 18.5582, y = 4.3947),
             colour = "black", shape = 21, size = 3)  +
  geom_text_repel(aes(x = 18.5582, y = 4.3947, label = "Bangui, the capital"),
                  family = theme_font,
                  size = 5.5,
                  nudge_x = 1.3,
                  nudge_y = -0.5,
                  min.segment.length = 1) +
  # Theme
  scale_colour_manual(name = "", values = c("Gold" = gold_speciale,
                                            "Diamonds" = bluel_speciale,
                                            "Hydrocarbon" = brown_speciale)) +
  labs(title = "Figure X. Spatial distribution of mines in the CAR per 2017",
       subtitle = NULL,
       y = NULL,
       x = NULL) +
  theme_speciale +
  theme(panel.grid.major.y = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 10))) +
  coord_sf(expand = FALSE, datum = NA)

save_plot_speciale("output/appendix_mines_in_car_map_point.png")

# Production mines -----
production %>%
  ggplot(.,
         aes(x = year,
             y = value,
             group = id)) +
  geom_line(aes(colour = id), size = 3, show.legend = FALSE) +
  facet_wrap(~id, scales = "free_y") +
  scale_colour_manual(name = "", values = c("Gold (grams)" = gold_speciale,
                                            "Diamonds (carats)" = bluel_speciale)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Figure X. Production by mines in the CAR since 2016",
       subtitle = NULL,
       y = NULL,
       x = NULL) +
  theme_speciale +
  theme(panel.grid.major.x = element_blank())

save_plot_speciale("output/appendix_mines_in_car_production.png")



