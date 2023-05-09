# Scrape Réseau des journalistes pour les Droits de l'homme

# Set-up run script as a background job
source("R/01_packages.R")
source("R/01_utils.R")

## Get links -----

# Main URL
url_rjdh <- "https://www.rjdhrca.org/category/actualites/page/" #"1/" for example

# Function to get links from each page
get_links_from_rjdh <- function(url){
  
  #url <- url_rjdh
  
  url %>%
    #paste0("1/") %>%
    read_html() %>%
    html_elements(".item-title a") %>%
    html_attr("href") %>%
    tibble("link" = .)
  
}

# Run for loop
# from start to n
# use n in file name to be saved
# start <- 1
# n <- 178
# data_list = list()
# data_list = vector("list", length = n)
# 
# for (i in start:n) {
#   
#   #i <- 1
#     
#   new_links <- url_rjdh %>%
#     paste0(., i, "/") %>%
#     get_links_from_rjdh(.)
#   
#   data_list[[i]] <- new_links
#   
#   Sys.sleep(1.5)
#   
#   percentage_done <- i/n
#   percentage_done <- percentage_done*100
#   percentage_done <- round(percentage_done, 0)
#   
#   paste(i, "of", n, " (", percentage_done, "% done).") %>% print()
#   
# }
# 
# # Bind all links together
# # Remove NULLs first
# data_list <- data_list[lengths(data_list) != 0]
# 
# all_links_rjdh <- data_list %>%
#   do.call(rbind, .) %>%
#   data.frame("url" = .) %>%
#   tibble() %>%
#   distinct(link, .keep_all = TRUE)

# Save URLs as a csv
#write_csv(all_links_rjdh, "data-raw/Radio/radio_all_links_rjdh_1.csv")

## Then scrape articles ----

# Scrape each article with their url
scrape_rjdh_article <- function(url){
  
  #url <- all_links_rjdh$link[1]
  
  article_html <- read_html(url)
  
  title <- article_html %>%
    html_elements(".entry-title") %>%
    html_text()
  
  date <- article_html %>%
    html_elements(".value") %>%
    html_text()
  
  body <- article_html %>%
    html_elements("p") %>%
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
all_links_rjdh <- read_csv("data-raw/Radio/radio_all_links_rjdh_0.csv")

# Map across to scrape articles
articles_rjdh <- map_df_progress(all_links_rjdh$link, scrape_rjdh_article)

# Save articles
#write_csv(articles_rjdh, "data-raw/Radio/radio_articles_rjdh_0.csv")


