# Scrape CAR medias

# Scrape Radio Ndeke Luka ----
# Scrape URLs of articles on each page on Radio website
# Until 2017, so around page 347 (4500 articles circa)
# For example 13:4485
# Test with 13:169

# Main URL
url_radiondekeluka <- "https://www.radiondekeluka.org/actualites.html?start" #=13 for example

# Function to get links from each page
get_links_from_radiondekeluka <- function(url){
  
  url %>%
    read_html() %>%
    html_elements(".articleLinkContainer") %>% 
    html_attr("href") %>%
    tibble("link" = .) %>%
    mutate(link = paste0("https://www.radiondekeluka.org", link))
  
}

# Run for loop
n <- 50
data_list = list()
data_list = vector("list", length = n)

for (i in 1:n) {
  
  i <- i*13
  
  new_links <- url_radiondekeluka %>%
    paste0(., "=", i) %>%
    get_links_from_radiondekeluka(.)
  
  data_list[[i]] <- new_links
  
  Sys.sleep(1)
  
  paste(i, "of", n*13) %>% print()
  
}

# Bind all links together
# Remove NULLs first
data_list <- data_list[lengths(data_list) != 0]

all_links_radiondekeluka <- data_list %>%
  do.call(rbind, .) %>%
  data.frame("url" = .) %>%
  tibble()

# Scrape each article with their url
scrape_radiondekeluka_article <- function(url){
  
  #url <- links_to_articles$link[1]
  
  article_html <- read_html(url)
  
  title <- article_html %>%
    html_elements("h2") %>%
    html_text()
  
  date <- article_html %>%
    html_elements(".pull-left") %>%
    html_text()
  
  text <- article_html %>%
    html_elements("p") %>%
    html_text() %>%
    paste(collapse = "")
  
  paste(paste(date), " is done.") %>% print()
  
  Sys.sleep(1)
  
  # Create df
  tibble(url = url,
         title = title,
         date = date,
         text = text)
  
}


# Map across to scrape articles
articles_radiondekeluka <- map_df(all_links_radiondekeluka$link, scrape_radiondekeluka_article)


# 

