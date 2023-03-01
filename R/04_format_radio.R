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

## Correct dates first ----
radio_raw_binded <- bind_rows(
  # Radio Ndeke Luka
  radio_raw %>%
    filter(sub_group == "Radio Ndeke Luka") %>%
    mutate(date = gsub("\n\t\t\t[[:alpha:]]+ ", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%d %B %Y")),
  # Radio Lengo Songo
  radio_raw %>%
    filter(sub_group == "Radio Lengo Songo") %>%
    mutate(date = gsub("\n\t\t\t[[:alpha:]]+ ", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%B %d, %Y")),
  # Radio Reseau des journalistes
  radio_raw %>%
    filter(sub_group == "Reseau des journalistes") %>%
    mutate(date = gsub("\n\t\t\t\t", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%B %d, %Y"))
)

## Then transmute columns ----
radio <- radio_raw_binded %>%
  arrange(sub_group, date) %>%
  transmute(sub_group,
            group = "Radio",
            date,
            week = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
            month = floor_date(date, unit = "month"),
            year = year(date),
            text = paste0(title, " ", body),
            text_nchar = nchar(text),
            url
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on URL
  distinct(url, .keep_all = TRUE)

radio_master <- radio

head(radio_master)

summary(radio_master$date)




