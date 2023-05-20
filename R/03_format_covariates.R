# Format covariates

# Spikes periods ----
# spike_periods_to_join <- spike_periods %>%
#   rowwise() %>%
#   mutate(date = list(seq.Date(date_min,
#                               date_max,
#                               by = "1 day"))
#   ) %>%
#   tidyr::unnest(date) %>%
#   select(spike_no, spike_text = text, date)

# Sub-national boundaries ----
gadm <- gadm_raw %>%
  clean_names() %>%
  transmute(country = as.character(country),
            region_1 = as.character(name_1),
            engtype_1,
            #region_2 = as.character(name_2),
            #engtype_2,
            geometry
  ) %>%
  # correct typos or adapt before joining with other objects, e.g. election data
  mutate(region_1 = case_when(region_1 == "Nassarawa" ~ "Nasarawa",
                              TRUE ~ region_1))

# Add centroids
gadm <- full_join(gadm %>%
                    mutate(id = row_number()),
                  gadm %>%
                    st_make_valid() %>%
                    st_centroid() %>%
                    sf::st_coordinates() %>%
                    as.data.frame() %>%
                    mutate(id = row_number())
)

## Crop and simplify
gadm_simp <- gadm %>%
  st_make_valid() %>%
  st_simplify(., dTolerance = 0.05)

gadm_simp <- gadm_simp %>%
  mutate(focus = country %in% c("Central African Republic"))

# gadm_simp <- sf::st_crop(gadm_simp,
#                          xmin = -13, xmax = 16, ymin = 8, ymax = 26)

gadm_simp <- gadm_simp %>%
  filter(focus)

#plot(gadm_simp)


# Format FEEL sentiment dictionary ----

# Remove accents and stem
feel <- feel_raw %>%
  transmute(token = word,
            token = remove_accents(token),
            token = remove_patterns_in_post(token),
            score = case_when(polarity == "positive" ~ 1,
                              polarity == "negative" ~ -1)) %>%
  filter(!grepl("\\s", token)) %>%
  # Stem words
  mutate(token = char_wordstem(token, language = "fr"),
         token = stri_trans_general(str = token, id = "Latin-ASCII"))


# Format AFINN sentiment -----
afinn_raw$afinn %>% summary()

# Format
afinn <- afinn_raw %>%
  mutate(token = remove_accents(token),
         token = remove_patterns_in_post(token),
         token = tolower(token),
         token = gsub("l'", "", token)) %>%
  filter(!grepl("\\s", token)) %>%
  # Stem words
  mutate(token = char_wordstem(token, language = "fr"),
         token = stri_trans_general(str = token, id = "Latin-ASCII")) %>%
  # Calculate
  group_by(token) %>%
  summarise(afinn_median = median(afinn),
            afinn_mean = mean(afinn))

# Check if AFINN has multiple scores per its stemmed token
# This is solved by taking its median (above)
# afinn %>%
#   group_by(stemmed, token) %>%
#   filter(n() > 1) %>%
#   arrange(stemmed)

# Format French stopwords -----
# Remove accents and stem
bbalet <- bbalet_raw %>%
  select(word = a) %>%
  transmute(word = remove_accents(word),
            word = remove_patterns_in_post(word)) 

# # Stem words
# feel <- feel %>%
#   mutate(text = char_wordstem(text, language = "fr"),
#          text = stri_trans_general(str = text, id = "Latin-ASCII"))


# Format Lock data -----
lock <- lock_raw %>%
  clean_names() %>%
  transmute(date = as.Date(a_1_date), approach = b_1_study_approach_2, quant = grepl("1", approach))

# Quant share
lock %>%
  group_by(quant) %>%
  summarise(n = n())


# Format ACLED -----
acled_raw %>% glimpse()
acled_raw %>% distinct(admin1)
acled_raw %>% group_by(actor1) %>% summarise(n = n()) %>% arrange(-n)
acled_raw %>% group_by(actor2) %>% summarise(n = n()) %>% arrange(-n)

northern_regions <- c("Vakaga", "Bamingui-Bangoran", "Haute-Kotto", "Haut-Mbomou", "Mbomou",
                      "Basse-Kotto", "Ouaka", "Kemo", "Nana-Grebizi")

actor_to_subset <- c("Military Forces of the Central African Republic (2016-)|Wagner Group|CPC: Coalition of Patriots for Change")

# Format
acled <- acled_raw %>%
  filter(country == "Central African Republic") %>%
  transmute(date = as.Date(event_date, "%d %b %Y"),
            month = floor_date(date, unit = "month"),
            quarter = floor_date(date, unit = "quarter"),
            halfyear = floor_date(date, "halfyear"),
            year,
            region = admin1, longitude, latitude, fatalities, actor1, actor2) %>%
  arrange(date) %>%
  #filter(grepl(actor_to_subset, actor1) | grepl(actor_to_subset, actor2)) %>%
  filter(year >= 2019) %>%
  #filter(fatalities > 0) %>%
  mutate(north = if_else(region %in% northern_regions, "North", "South"))

# Format mines ----
# Bind carbon, diamonds, and gold mine coordinates together
car_mines <- bind_rows(car_mines_carbon_raw %>% mutate(id = "Hydrocarbon"),
                       car_mines_diamonds_raw %>% mutate(id = "Diamonds"),
                       car_mines_gold_raw %>% mutate(id = "Gold"))


# Format production gold and diamonds ----
production <- production_raw %>%
  pivot_longer(cols = c(2:ncol(.)), names_to = "year")


# Format AidData ----
aiddata <- aiddata_raw %>%
  clean_names() %>%
  filter(recipient == "Central African Republic")

# Number of projects
nrow(aiddata)

# Total sum
aiddata %>%
  filter(recommended_for_aggregates == "Yes") %>%
  summarise(sum = sum(amount_constant_usd2017, na.rm = TRUE))








