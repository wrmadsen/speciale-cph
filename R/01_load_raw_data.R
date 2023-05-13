# Load raw data

# Radio data -----
read_bind_radio <- function(files){
  
  name_of_radio <- gsub("data\\-raw\\/Radio\\/radio_articles\\_|\\.csv|_.+", "", files)
  
  read_csv(files) %>%
    clean_names() %>%
    mutate(sub_group = name_of_radio,
           sub_group = recode(sub_group,
                              "radiondekeluka" = "Radio Ndeke Luka",
                              "radiolengo" = "Radio Lengo Songo",
                              "rjdh" = "RJDH"
           ))
  
}

radio_raw <- list.files(pattern = "radio_articles.+\\.csv", recursive = TRUE) %>%
  map_df(read_bind_radio)

# Digital media data -----
read_bind_digital <- function(files){
  
  name_of_digital <- gsub("data\\-raw\\/Digital\\/digital_articles\\_|\\.csv|_.+", "", files)
  
  read_csv(files) %>%
    clean_names() %>%
    mutate(sub_group = name_of_digital,
           sub_group = recode(sub_group,
                              "ndjonisango" = "Ndjoni Sango"
           ))
  
}

digital_raw <- list.files(pattern = "digital_articles.+\\.csv", recursive = TRUE) %>%
  map_df(read_bind_digital)

# Spike periods data ----
spike_periods <- read_csv("output/spike_periods.csv")

# Twitter data ----
# By keywords
# read_bind_twitter <- function(files){
#   
#   id <- gsub("data\\-raw\\/Twitter\\/twitter_keyword\\_|\\.csv|_.+", "", files)
#   
#   read_csv(files) %>%
#     clean_names() %>%
#     mutate(id = id)
#   
# }
# 
# twitter_keywords_raw <- list.files(pattern = "twitter_keyword_.+\\.csv", recursive = TRUE) %>%
#   map_df(read_bind_twitter)

# By user

# GADM data ----
# https://gadm.org/download_country.html
gadm_raw <- list.files(pattern = "1.shp", recursive = TRUE) %>%
  map_df(~read_sf(.))


# FEEL sentiment dictionary ----
# Amine Abdaoui, Jérôme Azé, Sandra Bringay et Pascal Poncelet
# http://advanse.lirmm.fr/feel.php
feel_raw <- read_delim("data-raw/Sentiment/FEEL.csv", delim = ";")

# AFINN en français -----
# https://github.com/fnielsen/afinn/blob/master/afinn/data/AFINN-fr-165.txt
# Finn Årup Nielsen, 2011
afinn_raw <- read_delim("data-raw/Sentiment/AFINN-fr-165.txt")

# French stopwords ----- 
# https://github.com/stopwords-iso/stopwords-fr
bbalet_raw <- read_table("data-raw/Sentiment/bbalet_stopwords_fr.txt")

# N-grams from Excel sheet ----
n_grams_to_keep_raw <- read_excel("data-raw/n_grams_recoding.xlsx", skip = 3)

# Load litt review data -----
# Lock
# https://uvaauas.figshare.com/collections/Organizational_Propaganda_on_the_Internet_A_Systematic_Review/4616030
lock_raw <- read_excel("data-raw/Andet/I.Lock_DATASET_Systematic Review Organizational Propaganda_PRI.xlsx")


# Oligarchs data ----
# WID inequality data
# https://wid.world/data/
# http://gabriel-zucman.eu/russia/
forbes_raw <- read_excel("data-raw/Oligarchs/NPZ2017MainFiguresTables.xlsx",
                         sheet = "DataSeriesWealth", range = "A6:CA44")


# Conflicts data -----
acled_raw <- read_csv("data-raw/Konflikter/2000-01-01-2023-05-31-Central_African_Republic-Mali.csv")

# CAR mines coordinates ----
car_mines_diamonds_raw <- read_sf("data-raw/Mines-CAR/diamond-blue.shp")
car_mines_gold_raw <- read_sf("data-raw/Mines-CAR/gold-gold.shp")
car_mines_carbon_raw <- read_sf("data-raw/Mines-CAR/hydrocarbon-black.shp")

# Production gold and diamonds ----
production_raw <- read_excel("data-raw/Mines-CAR/Mining stats CAR Manual.xlsx", skip = 1)


# AidData ----
# https://www.aiddata.org/data/aiddatas-global-chinese-development-finance-dataset-version-2-0
aiddata_raw <- read_excel("data-raw/Andet/AidDatas_Global_Chinese_Development_Finance_Dataset_Version_2_0/AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx",
                          sheet = "Global_CDF2.0")

