# Format radio data

# Tidy ----
fr_to_en_months <- c("janvier" = "january",
                     "février" = "February",
                     "mars" = "march",
                     "avril" = "april",
                     "mai" = "may",
                     "juin" = "June",
                     "juillet" = "july",
                     "août" = "august",
                     "septembre" = "september",
                     "octobre" = "october",
                     "novembre" = "november",
                     "décembre" = "december")

## Radio Ndeke Luka ----
radio <- radio_raw %>%
  # fix date
  mutate(date = gsub("\n\t\t\t[[:alpha:]]+ ", "", date),
         date = gsub(" ..\\:..$", "", date),
         date = str_replace_all(date, fr_to_en_months),
         date = as.Date(date, "%d %B %Y")) %>%
  transmute(name = "Radio Ndeke Luka",
            group = "Radio",
            date,
            week = floor_date(date, unit = "week"),
            text = paste0(title, " ", body),
            text_nchar = nchar(text),
            title,
            body,
            url
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on URL
  distinct(url, .keep_all = TRUE)

radio_master <- radio

head(radio_master)




