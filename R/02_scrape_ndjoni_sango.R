# Scrape Ndjoni Sango
# Backed by Russia

# Set-up run script as a background job
source("R/01_packages.R")
source("R/01_utils.R")

# Scrape URLs of articles on each page on Radio website
# 636 pages

## Get links -----

# Main URL
url_ndjoni_sango <- "https://ndjonisango.com/category/centrafrique/page/" #13 for example

# Function to get links from each page
get_links_from_ndjoni_sango <- function(url){
  
  url %>%
    read_html() %>%
    html_elements(".td-block-span6 .td-module-title a") %>%
    html_attr("href") %>%
    tibble("link" = .) %>%
    mutate(link = link)
  
}

# Run for loop
# from start to n
# use n in file name to be saved
start <- 1
n <- 636
n*10 # n of articles
data_list = list()
data_list = vector("list", length = n)

# for (i in start:n) {
#   
#   new_links <- url_ndjoni_sango %>%
#     paste0(., i) %>%
#     get_links_from_ndjoni_sango(.)
#   
#   data_list[[i]] <- new_links
#   
#   runif(1, 0.1, 0.2) %>% Sys.sleep()
#   
#   percentage_done <- i/n
#   percentage_done <- percentage_done*100
#   percentage_done <- round(percentage_done, 0)
#   
#   paste(i, "of", n, " (", percentage_done, "% done).") %>% print()
#   
# }

# Bind all links together
# Remove NULLs first
# data_list <- data_list[lengths(data_list) != 0]
# 
# all_links_ndjoni_sango <- data_list %>%
#   do.call(rbind, .) %>%
#   data.frame("url" = .) %>%
#   tibble() %>%
#   distinct(link, .keep_all = TRUE)

# Save URLs as a csv
#write_csv(all_links_ndjoni_sango, "data-raw/Radio/radio_all_links_ndjoni_sango_2.csv")

## Then scrape articles ----

# Scrape each article with their url
scrape_ndjoni_sango_article <- function(url){
  
  #url <- all_links_ndjoni_sango$link[1]
  
  article_html <- read_html(url)
  
  title <- article_html %>%
    html_elements(".td-post-title") %>%
    html_text() %>%
    gsub("\\r.+", "", .)
  
  date <- article_html %>%
    html_elements(".td-post-title") %>%
    html_text() %>%
    gsub(".+-", "", .) %>%
    trimws() %>%
    str_extract(., ".+[0-9]{4}")
  
  body <- article_html %>%
    html_elements("p") %>%
    html_text() %>%
    .[!grepl("email|\\r|Par", .)] %>%
    paste(collapse = "")
  
  paste0("Article dated '", date, "' scraped at ", Sys.time()) %>% print()
  
  runif(1, 3, 6) %>% Sys.sleep()
  
  # Create df
  tibble(url = url,
         title = title,
         date = date,
         body = body)
  
}

# Map across to scrape articles
articles_ndjoni_sango <- map_df_progress(all_links_ndjoni_sango$link[1:2000], scrape_ndjoni_sango_article)

# Save Radio Ndeke Luka
write_csv(articles_ndjoni_sango, "data-raw/Radio/radio_articles_ndjoni_sango_1_2000.csv")




