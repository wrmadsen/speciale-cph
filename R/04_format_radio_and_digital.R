# Format radio and digital media data

# Months vector ----
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

# Tidy Radio ----
## Correct radio dates first ----
authors_to_remove_from_lengo_songo <- c("Auric De Jean Jovice Ouakara",
                                        "Auric De Jean Jovice OUAKARA",
                                        "Auric De Jean\nJovice Ouakara",
                                        "Auric\nDe Jean Jovice Ouakara",
                                        "Auric de Jean Jovice OUAKARA",
                                        "Auric De Jean Jovice Ouakra",
                                        "Auric De Jean Jovice\nOuakara",
                                        "Auric De Jovice Ouakara",
                                        "Auric\nDe Jean Jovice OUAKARA",
                                        "Arthur NGBOKOLI",
                                        "Eduard Chesnokov",
                                        "Théoneste Pounika",
                                        "Theoneste Pounika",
                                        "Théoneste POUNIKA",
                                        "Théoneste\nPounika",
                                        "Theoneste POUNIKA",
                                        "Thérèse Jasmine\nMongonou Imandjia",
                                        "Thérèse jasmine Mongonou",
                                        "Saint-Cyr Gbégbé-Ngaïna",
                                        "Saint-Cyr\nGbégbé-Ngaïna",
                                        "Saint-Cyr GBEGBE-NGAINA",
                                        "Saint-Cyr\nGbégbé-Ngaina",
                                        "Saint-Cyr Gbégbé-Ngaina",
                                        "Saint-Cyr\nGBEGBE-NGAÏNA",
                                        "Saint-Cyr GBEGBE-NGAÏNA",
                                        "Saint-Cyr\nGBEGBE-NGAÏNNA",
                                        "Saint-Gbégbé-Ngaïna",
                                        "Sabrina Larissa Vinciane Nailo",
                                        "Sabrina Larissa Vinciane Naïlo",
                                        "Sabrina Larissa\nVinciane Naïlo",
                                        "Sabrina Larissa Vinciane\nNaïlo",
                                        "Sabrina Larissa",
                                        "Sabrina\nLarissa Vinciane Naïlo",
                                        "Vinciane Nailo",
                                        "Vinciane NAÏLO",
                                        "Lydie Sérégaza",
                                        "Lydie SEREGAZA",
                                        "Lydie\nSérégaza",
                                        "Carole BYCEKOAN",
                                        "Carole Bycekoan",
                                        "Carole\nBYCEKOAN",
                                        "Kelly Kandoro",
                                        "Jolidon Josiana Tchéckoé",
                                        "Marcelin Endjikélé\nKossikako",
                                        "Marcelin ENDJIKELE KOSSIKAKO",
                                        "Marcelin Endjikélé Kossikako",
                                        "Marcelin ENDJIKELE\nKOSSIKAKO",
                                        "Marcelin Endjikele\nKossikako",
                                        "Marcelin Endjikélé-Kossikako",
                                        "Marcelin\nEndjikélé-Kossikako",
                                        "Marcelin Yondorma",
                                        "Melvine Julia Zanguindi",
                                        "Stève Martial Mbetissinga",
                                        "Stève Martial Mbétissinga",
                                        "Stève Martial\nMbétissinga",
                                        "Steve Martial\nMbetissinga",
                                        "Stève Martial MBETISSINGA",
                                        "Stève\nMartial MBETISSINGA",
                                        "Stève Martial\nMbetissinga",
                                        "Martial\nMbétissinga",
                                        "François Zioro",
                                        "François\nZioro",
                                        "Firmin Ngrébada",
                                        "Pépin Vital Assana",
                                        "Pépin Vital ASSANA",
                                        "Papin Vital Assana",
                                        "Pépin vital Assana",
                                        "Pépin Vital\nAssana",
                                        "Pépin\nVital Assana",
                                        "Albert Yaloké Mokpème",
                                        "Déus Gracias Tchémanguéré",
                                        "Blaise Didacien KOSSIMATCHI",
                                        "La Rédaction",
                                        "La\nRédaction",
                                        "Rédaction",
                                        "Jolidon Josiana\nTcheckoe Yombo",
                                        "Débora Nadie Ndoidet",
                                        "Débora Nadie Ndoïdet",
                                        "Éphrem Yalike",
                                        "Lydie\nSérégaza et Débora Nadie Ndoïdet",
                                        "Fait à Bangui",
                                        "depuis Bohong",
                                        "depuis Bouar",
                                        "depuis Batangafo",
                                        "à Bangui") %>%
  paste0(., collapse = "|")

authors_to_remove_from_rjdh <- c("Auguste Bati-Kalamet",
                                 "Barnabas BADIWI",
                                 "Barnabas Badiwi",
                                 "Benoit Mbetioro",
                                 "Carlos Watou",
                                 "Cyrille Wegue",
                                 "Cyrille WEGUE",
                                 "Constant Ndolo-Babou",
                                 "Christelle Fandoma",
                                 "Ketsia Kolissio",
                                 "Fiacre Salabé",
                                 "Jocelyne Nadège Kokada",
                                 "Justin Mapouka",
                                 "Richardo Dimanche",
                                 "Winny Touguelé",
                                 "Paméla Dounian-Doté",
                                 "Paméla Dounian Doté",
                                 "Marina Moulou-Gnatho",
                                 "Vianney Ingasso") %>%
  paste0(., collapse = "|")

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
  # Radio RJDH, Reseau des journalistes
  radio_raw %>%
    filter(sub_group == "RJDH") %>%
    mutate(date = gsub("\n\t\t\t\t", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%B %d, %Y"),
    )
)


## Then transmute columns ----
master_radio <- radio_raw_binded %>%
  arrange(sub_group, date) %>%
  transmute(sub_group,
            group = "Radio",
            date,
            week = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
            month = floor_date(date, unit = "month"),
            year = year(date),
            text = paste0(title, " ", body),
            url
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on URL
  distinct(url, .keep_all = TRUE)

# Rename
head(master_radio)

summary(master_radio$date)

# Tidy digital media -----
## Correct digital media dates -----
digital_raw_binded <- bind_rows(
  # Ndjoni Sango
  digital_raw %>%
    filter(sub_group == "Ndjoni Sango") %>%
    mutate(date = str_squish(date),
           dateog2 = date,
           date = str_extract(date, "\\d+\\s\\D+\\s\\d+"),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%d %B %Y"))
)

## Then transmute columns ----
master_digital <- digital_raw_binded %>%
  arrange(sub_group, date) %>%
  transmute(sub_group,
            group = "Digital",
            date,
            week = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
            month = floor_date(date, unit = "month"),
            year = year(date),
            text = paste0(title, " ", body),
            text = gsub("NA", "", text),
            url
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on URL
  distinct(url, .keep_all = TRUE)


# Combine radio and digital ----
## Bind digital and radio ----
# And add political orientation variable
russian_outlets <- c("Radio Lengo Songo", "Ndjoni Sango")

master_text <- bind_rows(master_radio, master_digital) %>%
  arrange(group, date) %>%
  mutate(orient = if_else(sub_group %in% russian_outlets, "Pro-Russia", "Other")) %>%
  select(orient, everything())

## Turn lower case ----
master_text <- master_text %>%
  mutate(text = tolower(text))

## Remove various strings from text ----
# French months, ndjoni sango, etc.
master_text <- master_text %>%
  # Various, general removal
  mutate(text = gsub("sangofrançais|doctarngaba|\\.com|radio|sango|rkatsuva|internews|émission|www|actualité et nous|actualité|rédaction|ndjoni.net|ndjoni|lire la suite|ndeke luka|\\.car@",
                     "",
                     text)) %>%
  # Ndjoni Sango
  # Remove city and date of post and media name, website, "Source"
  # Topics in title (Afrique, RCA, etc.)
  mutate(text = gsub("rca:|rca :|afrique:|.+:|bangui.+\\(ndjoni sango\\)|ndjoni-sango.+\\:|ndjoni sango|ndjoni-sango|ndjonisango|source|fait à bangui|ndjonisnago",
                     "",
                     text)) %>%
  # RJDH
  # Remove title topics
  # Remove patterns from body
  # Remove authors of articles
  mutate(text = gsub(".+:|rjdh|(émission actualité et nous)|sangofrançais" %>%
                       paste0(., authors_to_remove_from_rjdh, collapse = "|"),
                     "",
                     text)) %>%
  # Lengo Songo
  # Remove author of post
  mutate(text = gsub("lengo songo|radio" %>%
                       paste0(., authors_to_remove_from_lengo_songo, collapse = "|"),
                     "",
                     text)) %>%
  # Ndeke Luka
  # Remove title topics
  # Remove var player = new MediaElementPlayer('#player-16138');
  mutate(text = gsub(".+:|radio ndeke luka|var player = new mediaelementplayer.+\\);|radio ndeke luka",
                     "",
                     text))

## Verify sample ----
# Number of rows
nrow(master_text)

# Slice and view
master_text %>%
  filter(sub_group == "Ndjoni Sango") %>%
  #filter(sub_group == "Radio Ndeke Luka") %>%
  select(sub_group, text, url) %>%
  slice_sample(n = 500) %>%
  arrange(nchar(text)) #%>% view()

# transmute(text = substr(text, nchar(text)-50, nchar(text))) %>%
# slice_sample(n = 50) %>%
# arrange(text_nchar) #%>% view
#  print(n = 300)

## Remove months ----
# Format months before removal
months_to_remove <- fr_to_en_months %>%
  names() %>%
  paste0(., collapse = "|")

months_to_remove <- months_to_remove %>%
  remove_accents() %>%
  paste0(., months_to_remove, collapse = "|")

# Remove months
master_text <- master_text %>%
  mutate(text = gsub(months_to_remove,
                     "",
                     text))

## Calculate n_char ----
master_text <- master_text %>%
  mutate(text_nchar = nchar(text))

## Verify sample again ----
master_text %>%
  filter(text_nchar < 500) %>%
  #filter(sub_group == "Ndjoni Sango") %>%
  #filter(sub_group == "Radio Ndeke Luka") %>%
  select(sub_group, text, text_nchar, url) %>%
  slice_sample(n = 500) %>%
  arrange(text_nchar) #%>% view()

## Drop with less than 200 characters ----
(before <- nrow(master_text)) # before

master_text <- master_text %>%
  filter(text_nchar >= 100)

(after <- nrow(master_text)) # after

before - after

(before - after)/before




