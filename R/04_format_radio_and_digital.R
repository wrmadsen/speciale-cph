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
           date = as.Date(date, "%d %B %Y"),
           # Remove title topics
           title = gsub(".+:|Radio Ndeke Luka", "", title),
           # Remove var player = new MediaElementPlayer('#player-16138');
           body = gsub("var player = new MediaElementPlayer.+\\);|Radio Ndeke Luka", "", body)),
  # Radio Lengo Songo
  radio_raw %>%
    filter(sub_group == "Radio Lengo Songo") %>%
    mutate(date = gsub("\n\t\t\t[[:alpha:]]+ ", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%B %d, %Y"),
           # Remove author of post
           body = gsub(authors_to_remove_from_lengo_songo, "", body),
           body = gsub("Lengo Songo|Radio", "", body)),
  # Radio RJDH, Reseau des journalistes
  radio_raw %>%
    filter(sub_group == "RJDH") %>%
    mutate(date = gsub("\n\t\t\t\t", "", date),
           dateog2 = date,
           date = gsub(" ..\\:..$", "", date),
           date = str_replace_all(date, fr_to_en_months),
           date = as.Date(date, "%B %d, %Y"),
           # Remove title topics
           title = gsub(".+:", "", title),
           # Remove patterns from body
           body = gsub("RJDH|(émission Actualité et Nous)|SangoFrançais", "", body),
           # Remove authors of articles
           body = gsub(authors_to_remove_from_rjdh, "", body),
    )
  
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

# Test before tidying above
# Authors (at the end of string Lengo)
radio %>%
  #filter(sub_group == "Radio Lengo Songo") %>%
  select(url, text_nchar, text) %>%
  filter(text_nchar > 100) %>%
  arrange(text_nchar) %>%
# transmute(text = substr(text, nchar(text)-50, nchar(text))) %>%
  slice_sample(n = 50) %>%
  arrange(text_nchar) #%>% view
#   print(n = 300)

# Rename
radio_master <- radio

head(radio_master)

summary(radio_master$date)


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
           date = as.Date(date, "%d %B %Y"),
           # Remove city and date of post and media name, website, "Source"
           # Topics in title (Afrique, RCA, etc.)
           title = gsub("RCA:|RCA :|Afrique:|.+:", "", title),
           body = gsub("Bangui.+\\(Ndjoni Sango\\)|Ndjoni-Sango.+\\:|Ndjoni Sango|Ndjoni-Sango|ndjonisango|Source|Fait à Bangui", "", body)
    )
)

## Then transmute columns ----
digital <- digital_raw_binded %>%
  arrange(sub_group, date) %>%
  transmute(sub_group,
            group = "Digital",
            date,
            week = floor_date(date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
            month = floor_date(date, unit = "month"),
            year = year(date),
            text = paste0(title, " ", body),
            text = gsub("NA", "", text),
            text_nchar = nchar(text),
            url
  ) %>%
  arrange(date) %>%
  # Remove duplicates based on URL
  distinct(url, .keep_all = TRUE)

# Test before tidying above
digital %>%
  #transmute(body = substr(body, nchar(body)-50, nchar(body))) %>%
  select(text_nchar, text) %>%
  filter(text_nchar < 1100 & text_nchar > 900) %>%
  slice_sample(n = 300) #%>% view()

digital_master <- digital

# Number of rows in total -----
nrow(radio_master) + nrow(digital_master)


