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



# Conflicts over time -----
acled_raw %>% glimpse()
acled_raw %>% distinct(admin1)
acled_raw %>% group_by(actor1) %>% summarise(n = n()) %>% arrange(-n)
acled_raw %>% group_by(actor2) %>% summarise(n = n()) %>% arrange(-n)



northern_regions <- c("Vakaga", "Bamingui-Bangoran", "Haute-Kotto", "Haut-Mbomou", "Mbomou",
                      "Basse-Kotto", "Ouaka", "Kemo", "Nana-Grebizi")

actor_to_subset <- c("Military Forces of the Central African Republic (2016-)|Wagner Group|CPC: Coalition of Patriots for Change")

acled <- acled_raw %>%
  transmute(date = as.Date(event_date, "%d %b %Y"),
            month = floor_date(date, unit = "month"),
            quarter = floor_date(date, unit = "quarter"),
            halfyear = floor_date(date, "halfyear"),
            year,
            region = admin1, longitude, latitude, fatalities, actor1, actor2) %>%
  mutate(north = if_else(region %in% northern_regions, "North", "South"))

acled <- acled %>%
  #filter(fatalities > 0) %>%
  filter(grepl(actor_to_subset, actor1) | grepl(actor_to_subset, actor2))

acled %>%
  #bind_rows(acled %>% mutate(region = "Total", north = "Total")) %>%
  group_by(region, north, month) %>%
  summarise(n = n()) %>%
  ggplot(.,
         aes(x = month,
             y = n)) +
  geom_line(aes(group = region, colour = north)) +
  facet_wrap(~region) +
  theme_speciale +
  labs(title = "Number of conflict events",
       x = NULL,
       y = "Number per month",
       caption = "Source: ACLED.")

save_plot_speciale("output/conflicts_per_month.png")

# Map

# Turn into sf object
acled_sf <- acled %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

mines_and_points <- 

# Plot with points
ggplot() +
  geom_sf(data = gadm_simp,
          colour = "darkgrey", fill = NA) +
  geom_sf(data = gadm_simp,
          colour = "black", fill = NA) +
  geom_sf(data = acled_sf,
          colour = "red", size = 0.3) +
  facet_wrap(~year) +
  geom_point(data = gadm_simp, aes(x = 20.73, y = 6.19), colour = "lightblue", size = 2, text = "DDD") + # Ndassima
  geom_point(data = gadm_simp, aes(x = 22.3949, y = 8.0706), colour = "lightblue", size = 2, text = "DDD") + # Damane killed
  geom_point(data = gadm_simp, aes(x = 18.5582, y = 4.3947), colour = "darkblue", size = 2, text = "DDD") + # Bangui +
  labs(title = "Conflicts mapped in CAR")

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





