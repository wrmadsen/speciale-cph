# Format covariates

## Sub-national boundaries ----
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

## Crop and simplify ----
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


