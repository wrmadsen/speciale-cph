# Create tokens from Twitter master dataset

## Remove patterns and URLs ----
remove_patterns_in_post <- function(input){
  
  # Special characters
  # And l'
  random <- c("@", "#", "\\.", "\\,", ":", ";",
              "\\/", "\\(", "\\)",
              #"[^\x01-\x7F]", # remove all non-ASCII, emojis
              '"', "\\'",
              "\\!", "\\?", "・",
              "l'", "L'")
  
  # Quotes
  quotes <- c("['‘’”“]")
  
  # http, URLs
  urls <- c("http.*", "https.*")
  
  # Numbers, digits
  digits <- c("[0-9]+")
  
  # Combine those that are to be removed completely
  remove_completely <- c(random, digits, quotes, urls) %>% paste0(., collapse = "|")
  
  # Replace some with space
  replace_w_spaces <- paste("\\_", "\\-", sep = "|")
  
  # Remove and replace
  input %>%
    gsub(remove_completely, "", .) %>%
    gsub(replace_w_spaces, " ", .)
  
}

## Remove accents from strings ----
remove_accents <- function(input){
  
  stri_trans_general(str = input, id = "Latin-ASCII")
  
}

## Strings to recode and avoid stemming ----
# Only those with more than one word included here
# Others 1-word tokens, which should be joined with two-words
# can be fixed by recoding after tokenization
names <- c("don kibarou",
           "don kibaru",
           "abdoulaye diop",
           # politicians
           "modibo keita",
           "vladimir poutine",
           #"poutine",
           #"vladimir",
           "joe biden",
           #"joe",
           "abdoulaye maiga",
           "choguel kokalla maiga",
           "choguel kokala maiga",
           "choguel maiga",
           #"choguel",
           "oumar mariko",
           "sekou tounkarasi",
           "aboubacar sidiki fomba",
           "diakaridia dao bla", # URD activist
           "diakaridia dao",
           "gouagnon coulibaly", # URD leader
           "abdoulaye maga",
           "assimi goita", # interim Mali president
           "assimi gota",
           "assimi goïta",
           #"assim",
           #"damiba",
           "paul henri damiba",
           "ibrahim traore",
           "ibrahime traore",
           "alassane ouattara",
           "alassane dramane ouattara",
           "moussa traore",
           # Yerewolo
           "drissa meminta",
           "ben le cerveau",
           "adama ben diarra",
           "adama diarra",
           "adama ben",
           "ben diarra",
           "sidiki kouyate",
           # influencers/random
           "issa cisse",
           "aziz traore",
           "boubou mabel diawara",
           "boubou mabel",
           "franklin nyamsi",
           "biton coulibaly",
           "djo balla sitan den tro",
           "citoyen burkim biiya",
           "gandhi malien",
           "balayira officiel",
           "balayira la",
           "niambele sadio",
           "souley deparis b",
           "aziz maiga ne ment",
           "seydou oumar traore",
           "malick konate",
           "boubou lah",
           # Religious, sheiks, imams, etc.
           "masjid salam badalabougou",
           "cheick oumar keita",
           "keita cheick oumar",
           "cheick abdoulaye kounta",
           "cheick aboubacar kourechi",
           "cheick modibo diarra",
           "cheick mamadou konate",
           "cheick mahi ibrahim",
           "cheick oumar konare",
           "cheick tihami haidara",
           "cheick abdoul aziz coulibaly",
           "cheick abdoul aziz",
           "cheick farouk",
           "imam mahamoud dicko",
           "imam mahmoud dicko",
           "imam dicko",
           # dont know
           "kati sebenikoro" # journalist? https://www.youtube.com/watch?v=g2liUeCpz-Q&ab_channel=OuvertureM%C3%A9dia-OM
           
)

countries <- c("cte divoire",
               "cote divoire",
               "cote d ivoire",
               "cote ivoir",
               "cote ivoire",
               "burkina faso",
               "burkina fasso",
               # "burkina",
               # "france",
               # "russia",
               # "guinee",
               # "russie",
               # "allemagne",
               "union africaine",
               "etats unis",
               "union europeene",
               #"mali",
               "nations unies")

groups <- c("coalition des mouvements de lazawad",
            "yerewolo debout sur les remparts",
            "lac debo tv",
            "joliba fm",
            #"wagner",
            "ouverture media",
            "actuel media",
            "amina media",
            "ad media mali",
            "media info afrique",
            "songhoi media",
            "mouvement m naaba wobgo",
            "media presse mali officiel",
            "baoule fm  mhz   bamako",
            "bmd media")

phrases <- c(#"was live"
)

# Create named vector
strings_to_recode <- c(names, countries, groups, phrases) %>%
  tibble(old = .) %>%
  mutate(new = gsub(" ", "_", old)) %>%
  # to named vector
  deframe()

## Recode function certain strings before tokenizing ----
# Ensures that these strings are kept together
# Includes names (first and last name for example), organisations,
# countries, etc.
# Replace spaces with "_" to keep each as one token
recode_strings_before_tokenizing <- function(strings_to_recode, input){
  
  # Replace
  input %>%
    mutate(text = str_replace_all(text, strings_to_recode))
  
}

## Find and remove repeated substrings ----
# find_and_remove_repeated_substring <- function(input){
#   
#   #input <- "assemblee generale de yerewoloassemblee generale de yerewolo"
#   
#   gsub("^(.*)\\1$", "\\1", input)
#   
# }

## Create data.table dt ----
# To work faster
convert_to_dt <- function(input){
  
  input %>%
    mutate(document = paste0("text", row_number())) %>%
    as.data.table()
  
}

## Create and clean tokens ----
create_dfm_speciale <- function(dt){
  
  # Create corpus
  corpus <- corpus(dt$text, docvars = dt)
  
  # Tokenise tweets
  # Remove various characters
  tokens <- quanteda::tokens(corpus,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_url = TRUE)
  
  # Lower case
  tokens <- tokens_tolower(tokens)
  
  # Split into two
  tokens_no_stem <- tokens_select(tokens,
                                  strings_to_recode,
                                  select = "keep")
  
  tokens_stem <- tokens_select(tokens,
                               strings_to_recode,
                               select = "remove")
  
  # Stem one, dont stem the other
  tokens_stem <- tokens_wordstem(tokens_stem, language = "fr")
  
  # To dfm
  dfm_stem <- dfm(tokens_stem, remove = stopwords(language = "fr"))
  dfm_no_stem <- dfm(tokens_no_stem, remove = stopwords(language = "fr"))
  
  # Bind dfms
  dfm <- cbind(dfm_stem, dfm_no_stem)
  
  # Return
  dfm
  
}


convert_dfm_to_tibble <- function(dfm, dt){
  
  # Convert from dfm to data.table
  tokens_dt <- tidytext::tidy(dfm) %>%
    rename(token = term,
           token_count = count) %>%
    as.data.table()
  
  # Join docvars back by document number
  # Then to tibble
  tokens_master <- merge(dt, tokens_dt,
                         all.x = TRUE, by = "document") %>%
    tibble()
  
  # Return
  tokens_master
  
}

## Recode tokens post ----
# Does two things:
# (1) Replace previously imposed "_" underscores with spaces (like before)
# (2) Recode misspellings
post_recode_tokens <- function(input){
  
  #input <- tokens_twitter
  
  # Remove self-imposed underscores "_"
  output <- input %>%
    mutate(#underscore_true = grepl("_", token),
      token = gsub("_", " ", token))
  
  # Create named vector
  # First is removed
  # Second is replacement
  tokens_to_recode <- c("🇲🇱" = "mal",
                        "🇨🇮" = "cote divoire",
                        "🇧🇫" = "burkina faso",
                        "burkina fasso" = "burkina faso",
                        "burkina" = "burkina faso",
                        "burkinafaso" = "burkina faso",
                        "🇫🇷" = "franc",
                        "🇷🇺" = "russ",
                        "🇺🇳" = "nations unies",
                        "🇬🇳" = "guinee",
                        "🇪🇺" = "union europeenne",
                        "lafriqu" = "afriqu",
                        "afrik" = "afriqu",
                        "dafriqu" = "afriqu",
                        # organisations
                        "minusm" = "minusma",
                        # names
                        "diakaridia dao" = "diakaridia dao bla",
                        "assim" = "assimi goita",
                        "don kibaru" = "don kibarou",
                        "keita cheick oumar" = "cheick oumar keita",
                        "choguel kokala maiga" = "choguel kokalla maiga",
                        "choguel" = "choguel kokalla maiga",
                        "choguel maiga" = "choguel kokalla maiga",
                        "boubou mabel" = "boubou mabel diawara",
                        "vladim" = "poutin",
                        # ben le cerveau
                        "adama ben diarra" = "ben le cerveau",
                        "adama diarra" = "ben le cerveau",
                        "adama ben" = "ben le cerveau",
                        "ben diarra" = "ben le cerveau",
                        "ouattar" = "alassane ouattara",
                        "yerewolo" = "yerewolo debout sur les remparts",
                        "imam mahamoud dicko" = "imam dicko",
                        "imam mahmoud dicko" = "imam dicko")
  
  names(tokens_to_recode) <- paste0("^", names(tokens_to_recode), "$")
  
  # Replace
  output <- output %>%
    mutate(token = str_replace_all(token, tokens_to_recode))
  
  # Other recoding
  # Vector of less interesting tokens
  tokens_less_interesting <- c("cest", "tout", "grand", "at", "tre", "plus", "dun", "fait",
                               "tous", "invit", "san", "ete", "mr", "entre", "ecout", "si",
                               "fin", "apre", "an", "ca", "dit", "partag", "contact",
                               "direct", "mem", "jour", "quil", "sous", "autr", "va",
                               "nest", "deux", "non") %>%
    paste0("^", ., "$") %>%
    paste0(., collapse = "|")
  
  tokens_more_interesting <- c("paix", "urgent", "macron",
                               "guerr", "dieu", 
                               "developp", "mercenair", "gouvern",
                               # organisations or groups
                               "urd",
                               "cedeao", "cemac", "sadec", "minusm", "corem",
                               "militair", "bamako", "afriqu", "russ", "radio", "mort",
                               "econom", "independ", "souverainet", "zelensky", "ukrain",
                               "accord", "mobilis", "occidental", "allah",
                               "sahel", "menac", "cfa", "milit", "islam", "alger") %>%
    paste0("^", ., "$") %>%
    paste0(., collapse = "|")
  
  # Run recoding, three columns
  output <- output %>%
    mutate(token_less_interesting = grepl(tokens_less_interesting, token),
           token_was_recoded = grepl(strings_to_recode %>%
                                       paste0("^", ., "$") %>%
                                       paste0(., collapse = "|") %>%
                                       gsub("_", " ", .),
                                     token),
           token_more_interesting = grepl(tokens_more_interesting, token))
  
  output
  
}

## Join lexicon to tokens ----
# add_sentiment_to_tokens <- function(tokens_master, afinn_stem){
#   
#   # Join sentiment dictionary to tokens
#   senti_tokens <- merge(tokens_master %>% as.data.table(),
#                         afinn_stem %>% rename(token = stem) %>% as.data.table(),
#                         all.x = TRUE,
#                         by = "token") %>%
#     tibble()
#   
#   # Return
#   senti_tokens
#   
# }


# Run functions ----

## For Twitter ----
twitter_dt <- twitter_master %>%
  mutate(text = remove_patterns_in_post(text),
         text = remove_accents(text),
         text = find_and_remove_repeated_substring(text)) %>%
  recode_strings_before_tokenizing(strings_to_recode, .) %>%
  convert_to_dt()

twitter_dfm <- twitter_dt %>%
  create_dfm_speciale()

twitter_tokens <- convert_dfm_to_tibble(twitter_dfm, twitter_dt) %>%
  post_recode_tokens()

## Radio ----
radio_dt <- radio_master %>%
  mutate(text = remove_patterns_in_post(text),
         text = remove_accents(text),
         text = find_and_remove_repeated_substring(text)) %>%
  recode_strings_before_tokenizing(strings_to_recode, .) %>%
  convert_to_dt()

radio_dfm <- radio_dt %>%
  create_dfm_speciale()

radio_tokens <- convert_dfm_to_tibble(radio_dfm, radio_dt) %>%
  post_recode_tokens()

# Final recoding ----
# Remove single-character tokens
twitter_tokens <- twitter_tokens %>%
  filter(nchar(token) > 1)

radio_tokens <- radio_tokens %>%
  filter(nchar(token) > 1)

# Print or view ----
# Print tokens with highest counts

## Twitter ----
twitter_tokens %>%
  group_by(token) %>%
  #group_by(token, token_less_interesting, token_more_interesting, token_was_recoded) %>%
  summarise(n = n()) %>%
  #filter(n > 50) %>%
  #filter(!token_less_interesting & !token_was_recoded) %>%
  arrange(-n) #%>% view("count")

# Check individual tokens with view()
twitter_tokens %>% filter(token == "wagn") %>%
  arrange(text_nchar) #%>% view("token")


## Radio ----
radio_tokens %>%
  group_by(token) %>%
  summarise(n = n()) %>%
  arrange(-n)





