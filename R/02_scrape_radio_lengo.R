# Scrape Lengo Sango

# Scrape Réseau des journalistes pour les Droits de l'homme

# Set-up run script as a background job
source("R/01_packages.R")
source("R/01_utils.R")

## Get links -----

# Main URL
url_radio_lengo <- "https://lengosongo.cf/" #"2023/" for example

# Function to get links from each page
get_links_from_radio_lengo <- function(url){
  
  #url <- url_radio_lengo
  
  url %>%
    read_html() %>%
    html_elements(".jeg_post_title a") %>%
    html_attr("href") %>%
    tibble("link" = .)
  
}

# Run for loop
# from start to n
# use n in file name to be saved
start <- 2023
n <- 2020
data_list = list()
data_list = vector("list", length = n)

for (i in start:n) {
  
  #i <- 2023 # test
  
  # Total number of pages for this year
  total_pages <- url_radio_lengo %>%
    paste0(i, "/") %>%
    read_html() %>%
    html_elements(".page_number") %>%
    html_text() %>%
    tail(., 1) %>%
    as.integer()
  
  # Add year to page url to use now
  url_radio_lengo_with_year <- paste0(url_radio_lengo, i, "/page/")
  
  # For each page in this year, scrape the links
  for (j in 1:total_pages) {
    
    #j <- 1 # test
    
    new_links <- url_radio_lengo_with_year %>%
      paste0(., j, "/") %>%
      get_links_from_radio_lengo(.)
    
    new_links_no <- paste0(i, j) %>% as.integer()
    
    data_list[[new_links_no]] <- new_links
    
    Sys.sleep(1.5)
    
    percentage_done <- j/total_pages
    percentage_done <- percentage_done*100
    percentage_done <- round(percentage_done, 0)
    
    paste(j, "of", total_pages, " (", percentage_done, "% done). Year:", i) %>% print()
    
  }
  
}

# Bind all links together
# Remove NULLs first
data_list <- data_list[lengths(data_list) != 0]

all_links_radio_lengo <- data_list %>%
  do.call(rbind, .) %>%
  data.frame("url" = .) %>%
  tibble() %>%
  distinct(link, .keep_all = TRUE)

# Save URLs as a csv
write_csv(all_links_radio_lengo, "data-raw/Radio/radio_all_links_radio_lengo_2.csv")

## Then scrape articles ----

# Scrape each article with their url
scrape_radio_lengo_article <- function(url){
  
  #url <- all_links_radio_lengo$link[1]
  
  article_html <- read_html(url)
  
  title <- article_html %>%
    html_elements(".jeg_post_title") %>%
    html_text()
  
  date <- article_html %>%
    html_elements(".jeg_meta_date a") %>%
    html_text()
  
  body <- article_html %>%
    html_elements(".content-inner p") %>%
    html_text() %>%
    paste(collapse = "")
  
  paste("\n", paste(title)) %>% print()
  
  Sys.sleep(1.5)
  
  # Create df
  tibble(url = url,
         title = title,
         date = date,
         body = body)
  
}

# Read links
#all_links_radion_lengo <- read_csv("data-raw/Radio/radio_all_links_radio_lengo_2.csv")

# Map across to scrape articles
articles_radio_lengo <- map_df_progress(all_links_radion_lengo$link, scrape_radio_lengo_article)

# Save articles
write_csv(articles_radio_lengo, "data-raw/Radio/radio_articles_radio_lengo_2.csv")


