# Scrape Réseau des journalistes pour les Droits de l'homme

## Get links -----

# Main URL
url_radio_reseau <- "https://www.rjdhrca.org/category/actualites/page/" #"1/" for example

# Function to get links from each page
get_links_from_radio_reseau <- function(url){
  
  url_radio_reseau %>%
    paste0("1/") %>%
    read_html() %>%
    html_elements(".articleLinkContainer") %>% 
    html_attr("href") %>%
    tibble("link" = .)
    mutate(link = paste0("https://www.radiondekeluka.org", link))
  
}

# Run for loop
# from start to n
# use n in file name to be saved
start <- 299
n <- 347
n*13 # n of articles
data_list = list()
data_list = vector("list", length = n)

for (i in start:n) {
  
  i <- i*13
  
  new_links <- url_radio_reseau %>%
    paste0(., i, "/") %>%
    get_links_from_radio_reseau(.)
  
  data_list[[i]] <- new_links
  
  Sys.sleep(1.5)
  
  percentage_done <- i/13/n
  percentage_done <- percentage_done*100
  percentage_done <- round(percentage_done, 0)
  
  paste(i/13, "of", n, " (", percentage_done, "% done).") %>% print()
  
}

# Bind all links together
# Remove NULLs first
data_list <- data_list[lengths(data_list) != 0]

all_links_radiondekeluka <- data_list %>%
  do.call(rbind, .) %>%
  data.frame("url" = .) %>%
  tibble() %>%
  distinct(link, .keep_all = TRUE)

# Save URLs as a csv
write_csv(all_links_radio_reseau, "data-raw/Radio/radio_all_links_radio_reseau_0.csv")

## Then scrape articles ----

# Scrape each article with their url
scrape_radio_reseau_article <- function(url){
  
  #url <- links_to_articles$link[1]
  
  article_html <- read_html(url)
  
  title <- article_html %>%
    html_elements("h2") %>%
    html_text()
  
  date <- article_html %>%
    html_elements(".pull-left") %>%
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

# Map across to scrape articles
articles_radio_reseau <- map_df_progress(all_links_radion_reseau$link, scrape_radio_reseau_article)

# Save articles
write_csv(articles_radio_reseau, "data-raw/Radio/radio_articles_radio_reseau_0.csv")


