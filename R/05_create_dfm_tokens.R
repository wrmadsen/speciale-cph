# Create tokens from text

# Create functions ----

# N-gram strings that should not be split nor stemmed
n_grams_to_keep <- n_grams_to_keep_raw %>%
  select(-c(explanation, category, study))

# Create named vector
n_grams_to_keep_vector <- n_grams_to_keep %>%
  pivot_longer(cols = c(1:ncol(.))) %>%
  filter(!is.na(value)) %>%
  select(-name) %>%
  # Between so that tokens are not split
  # After to reject stemming
  transmute(before = value,
            new = gsub(" ", "_", before),
            new = paste0(new, "_")) %>%
  deframe()

names(n_grams_to_keep_vector) <- paste0("\\b", names(n_grams_to_keep_vector), "\\b")

## Add underscores to ngrams before tokenizing
# Ensures that these ngrams are kept together
# Replace spaces with "_" to keep each as one token
# Problem is that centrafrique matches with "ue" (union europeene)
add_underscores_ngrams_before_tokenizing <- function(input, n_grams_to_keep_vector){
  
  # Replace
  output <- str_replace_all(input, n_grams_to_keep_vector)
  
  # Remove double "__"
  output <- gsub("__|___", "_", output)
  
  # Return
  output
  
}

## Find and remove repeated substrings
find_and_remove_repeated_substring <- function(input){
  
  gsub("^(.*)\\1$", "\\1", input)
  
}

## Create data.table dt
# To work faster
convert_to_dt <- function(input){
  
  # No need to arrange if done before
  input %>%
    mutate(document = paste0("text", row_number())) %>%
    as.data.table()
  
}

## Create and clean tokens
create_tokens <- function(dt, bbalet){
  
  # Create corpus
  corpus <- corpus(dt$text, docvars = dt)
  
  # Tokenise tweets
  # Remove various characters
  tokens_w_stopwords <- quanteda::tokens(corpus,
                                         remove_punct = TRUE,
                                         remove_symbols = TRUE,
                                         remove_numbers = TRUE,
                                         remove_url = TRUE
  )
  
  # Format stopwords (remove accents)
  french_stopwords <- stopwords(language = "fr") %>%
    stri_trans_general(str = ., id = "Latin-ASCII")
  
  # Remove stopwords before stemming the tokens
  # Also remove more stopwords (bbalet set of stopwords))
  tokens_without <- tokens_remove(tokens_w_stopwords, pattern = french_stopwords)
  tokens_without <- tokens_remove(tokens_without, pattern = bbalet$word)
  
  # Return
  tokens_without
  
}

ngrams_to_recode <- n_grams_to_keep %>%
  mutate(main_2 = main) %>%
  pivot_longer(cols = c(2:ncol(.))) %>%
  select(value, main) %>%
  filter(!is.na(value)) %>%
  mutate(value = gsub(" ", "_", value),
         value = paste0(value, "_")) %>%
  #select(main, value) %>%
  deframe()

# Recode ngrams
recode_ngrams <- function(tokens, ngrams_to_recode){
  
  # Test
  #tokens <- master_tokens_stemmed
  
  # Change names of named vector to recode
  # Names are the ones that match
  # Values = replacements
  names(ngrams_to_recode) <- paste0("^", names(ngrams_to_recode), "$")
  
  # Create replacement ngrams
  # No underscores, no whitespace at the end
  replacement_tokens <- types(tokens) %>%
    str_replace_all(., ngrams_to_recode)
  
  tokens <- tokens_replace(tokens, 
                           pattern = types(tokens), 
                           replacement = replacement_tokens,
                           valuetype = "fixed")
  
  # Return
  tokens
  
}

## Create dfm from tokens
create_dfm <- function(tokens){
  
  # To dfm
  dfm <- dfm(tokens)
  
  # Return
  dfm
  
}

# From dfm to tibble
convert_dfm_to_tibble <- function(dfm, dt){
  
  # Convert from dfm to data.table
  tokens_dt <- tidytext::tidy(dfm) %>%
    rename(token = term,
           token_count = count) %>%
    as.data.table()
  
  # Join docvars back by document number
  # Then to tibble
  tokens_as_tibble <- merge(dt, tokens_dt,
                            all.x = TRUE, by = "document") %>%
    tibble()
  
  # Return
  tokens_as_tibble
  
}

# Run functions ----
## Tidy text ----
# Text has been turned to lower case in the 04 script
master_text_tidied <- master_text %>%
  mutate(text = gsub("'|'|’|’|-", " ", text),
         text = remove_patterns_in_post(text),
         text = str_squish(text),
         text = remove_accents(text),
         text = find_and_remove_repeated_substring(text))

nrow(master_text_tidied)

# Replace certain number-strings
# If these were not taken care of,
# 3r would become "r" after removing the number, for example
number_strings <- c("\\b3r\\b" = "threer",
                    "\\b3 r\\b" = "threer",
                    "cod 2020" = "la coalition de l opposition democratique",
                    "cod2020" = "la coalition de l opposition democratique")

master_text_tidied$text <- str_replace_all(master_text_tidied$text,
                                           number_strings,
                                           names(number_strings))

## Add underscores and convert to dt -----
master_dt <- master_text_tidied %>%
  mutate(text = add_underscores_ngrams_before_tokenizing(text, n_grams_to_keep_vector)) %>%
  convert_to_dt()

## Create tokens ----
master_tokens <- master_dt %>%
  # Must remove number with gsub()
  # because create_tokens() only removes only-number tokens
  mutate(text = gsub("[0-9]+", "", text)) %>%
  create_tokens(., bbalet)

## Stem tokens -----
master_tokens_stemmed <- master_tokens %>%
  tokens_wordstem(., language = "fr")

## Recode tokens ----
master_tokens_stemmed <- master_tokens_stemmed %>%
  recode_ngrams(., ngrams_to_recode)

## Create dfm ----
master_dfm <- master_tokens_stemmed %>%
  create_dfm()

## Check summary so far ----
master_dfm

## Trim dfm ----
# Minimum
master_dfm <- master_dfm %>%
  dfm_trim(.,
           min_docfreq = 0.005,
           docfreq_type = "prop")

# Must have at least two character (e.g. "or")
master_dfm <- master_dfm %>%
  dfm_select(., min_nchar = 2)

# Summary
master_dfm

# Maximum
master_dfm <- master_dfm %>%
  dfm_trim(.,
         max_docfreq = 0.2,
         docfreq_type = "prop")

# Summary
master_dfm

## Dfm to tf-idf -----
master_dfm_tf_idf <- master_dfm %>%
  dfm_tfidf()

## To tibble -----
master_tokens_tbl <- master_dfm %>%
  convert_dfm_to_tibble(., master_dt)



# Print or view ----

## View most popular tokens and phrases ----
popular_tokens <- topfeatures(master_dfm, n = 10000) %>%
  data.frame() %>%
  rownames_to_column("token") %>%
  tibble() %>%
  rename("count" = ".")

reference_tokens <- tibble(non_stemmed = master_tokens %>% as.character(),
                           stemmed = master_tokens_stemmed %>% as.character()) %>%
  distinct()

popular_tokens <- left_join(popular_tokens, reference_tokens, by = c("token" = "stemmed"))

popular_tokens <- popular_tokens %>%
  nest(data = non_stemmed) %>%
  mutate(non_stemmed = paste0(data) %>% gsub("list\\(non_stemmed = c\\(|\\,|\\|\\)|\\(", "", .)) %>%
  select(token, non_stemmed)

# Count per token
master_tokens_tbl %>%
  group_by(token) %>%
  summarise(n = n()) %>%
  arrange(-n) #%>% view("count")

# Check individual tokens with view()
master_tokens_tbl %>%
  #filter(token == "touad") %>%
  filter(grepl("abdoulay", token)) %>%
  select(text_nchar, token) %>%
  filter(text_nchar < 1000) %>%
  slice_sample(n = 10) %>%
  arrange(text_nchar) #%>% view("token")


# TESTING ----
# Sample to find n-grams
# master_text_tidied %>%
#   filter(text_nchar < 800) %>%
#   group_by(sub_group) %>%
#   #slice_sample(n = 1000) %>%
#   arrange(text_nchar) %>%
#   select(text)

# Add underscores to n-grams
# Subset to test out
# master_text_tidied_test <- master_text_tidied %>%
#   filter(text_nchar < 800) %>%
#   slice_sample(n = 200) %>%
#   filter(grepl("titorenko", text))

# Check that ngrams have been recoded correctly
topfeatures(master_dfm, 10)



