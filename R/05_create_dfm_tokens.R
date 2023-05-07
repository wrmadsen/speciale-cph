# Create tokens from text

# Create functions ----

# Strings to recode and avoid stemming ----
# Only those with more than one word included here
# Others 1-word tokens, which should be joined with two-words
# can be fixed by recoding after tokenization
names <- c("don kibarou",
           "don kibaru",
           "abdoulaye diop",
           # politicians
           "modibo keita",
           "vladimir poutine",
           #"poutine", # uncomment to test (comment out later)
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
            "ndjoni sango",
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
  mutate(new = gsub(" ", "_", old),
         new = paste0(new, "_")
  ) %>%
  # to named vector
  deframe()

## Recode function certain strings before tokenizing
# Ensures that these strings are kept together
# Includes names (first and last name for example), organisations,
# countries, etc.
# Replace spaces with "_" to keep each as one token
recode_strings_before_tokenizing <- function(strings_to_recode, input){
  
  # Replace
  # Remove double "__"
  input %>%
    mutate(text = str_replace_all(text, strings_to_recode),
           text = gsub("__", "_", text))
  
}

## Find and remove repeated substrings
find_and_remove_repeated_substring <- function(input){
  
  #input <- "assemblee generale de yerewoloassemblee generale de yerewolo"
  
  gsub("^(.*)\\1$", "\\1", input)
  
}

## Create data.table dt
# To work faster
convert_to_dt <- function(input){
  
  input %>%
    arrange(group, date) %>%
    mutate(document = paste0("text", row_number())) %>%
    as.data.table()
  
}

# Create tokens ----

## Create and clean tokens
create_tokens <- function(dt){
  
  # Test
  #dt <- master_dt
  
  # Create corpus
  corpus <- corpus(dt$text, docvars = dt)
  
  # Tokenise tweets
  # Remove various characters
  tokens <- quanteda::tokens(corpus,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_url = TRUE)
  
  # Format stopwords (remove accents)
  french_stopwords <- stopwords(language = "fr") %>%
    stri_trans_general(str = ., id = "Latin-ASCII")
  
  # Remove stopwords before stemming the tokens
  tokens <- tokens_remove(tokens, pattern = french_stopwords)
  
  tokens
  
}

tokens_to_recode <- c(
  # countries
  "🇲🇱" = "mal",
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
  "vladimir poutine" = "poutin",
  #"vladim" = "poutin",
  # ben le cerveau
  "adama ben diarra" = "ben le cerveau",
  "adama diarra" = "ben le cerveau",
  "adama ben" = "ben le cerveau",
  "ben diarra" = "ben le cerveau",
  "ouattar" = "alassane ouattara",
  "yerewolo" = "yerewolo debout sur les remparts",
  "imam mahamoud dicko" = "imam dicko",
  "imam mahmoud dicko" = "imam dicko")

# Replace/recode tokens
replace_tokens <- function(tokens, tokens_to_recode){
  
  # Test
  #tokens <- master_tokens
  
  # Change names of named vector to replace
  names(tokens_to_recode) <- paste0("^", names(tokens_to_recode), "$")
  
  # Create replacement tokens
  # No underscores, no whitespace at the end
  replacement_tokens <- types(tokens) %>%
    stringi::stri_replace_all_regex(., "\\_", " ") %>%
    stringi::stri_replace_all_regex(., " $", "") %>%
    str_replace_all(., tokens_to_recode)
  
  tokens <- tokens_replace(tokens, 
                           pattern = types(tokens), 
                           replacement = replacement_tokens,
                           valuetype = "fixed")
  
  # Return
  tokens
  
}

# Create dfm ----
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

# Format before running functions ----
# Bind digital and radio ----
# And add political orientation variable
russian_outlets <- c("Radio Lengo Songo", "Ndjoni Sango")

master_text <- bind_rows(radio_master, digital_master) %>%
  mutate(orient = if_else(sub_group %in% russian_outlets, "Pro-Russia", "Other")) %>%
  select(orient, everything())

# Join spikes periods
master_text <-master_text %>%
  left_join(spike_periods_to_join) %>%
  mutate(spike_no = if_else(is.na(spike_no), 0, spike_no), 
         spike_binary = if_else(spike_no > 0, 1, 0) %>% as.factor)

# Add observations that are pro-Russia or Non-Russian total
# Remove this?
# This doubles the number of rows, making the model process slower
# master_text <- bind_rows(master_text %>%
#                            filter(orient != "Pro-Russia") %>%
#                            mutate(sub_group = "Non-Russian total"),
#                          master_text %>%
#                            filter(orient == "Pro-Russia") %>%
#                            mutate(sub_group = "Pro-Russian total")) %>%
#   bind_rows(master_text)



# Run functions ----
master_dt <- master_text %>%
  mutate(text = remove_patterns_in_post(text),
         text = remove_accents(text),
         text = tolower(text),
         text = find_and_remove_repeated_substring(text)) %>%
  recode_strings_before_tokenizing(strings_to_recode, .) %>%
  convert_to_dt()

# Remove certain patterns from text
patterns_to_remove <- c("var player = new mediaelementplayerplayer")

patterns_to_remove <- patterns_to_remove %>%
  paste0(., collapse = "|")

master_dt <- master_dt %>%
  mutate(text = gsub(patterns_to_remove, "", text))

master_tokens <- master_dt %>%
  create_tokens() %>%
  # At least two character length
  tokens_select(., min_nchar = 2)

# Stem tokens
master_tokens_stemmed <- master_tokens %>%
  tokens_wordstem(., language = "fr")

# Replace tokens
master_tokens_stemmed <- master_tokens_stemmed %>%
  replace_tokens(., tokens_to_recode) #%>%
# Remove certain tokens
#tokens_remove(., total_to_be_removed)

master_dfm <- master_tokens %>%
  create_dfm() %>%
  # Remove tokens by count
  # Must have minimum term count
  # But maximum document count
  dfm_trim(.,
           min_termfreq = 50,
           termfreq_type = "count",
           max_docfreq = 1000,
           docfreq_type = "count")

master_dfm_tf_idf <- master_dfm %>%
  dfm_tfidf()

master_tokens_tbl <- master_dfm %>%
  convert_dfm_to_tibble(., master_dt)

# Print or view ----

## View most popular tokens and phrases ----
popular_tokens <- topfeatures(master_dfm, n = 1000) %>%
  data.frame() %>%
  rownames_to_column("token") %>%
  tibble() %>%
  rename("count" = ".")

reference_tokens <- tibble(non_stemmed = master_tokens %>% as.character(),
                           stemmed = master_tokens_stemmed %>% as.character()) %>%
  distinct()

popular_tokens <- left_join(popular_tokens, reference_tokens, by = c("token" = "stemmed"))

popular_tokens

master_tokens_tbl %>%
  group_by(token) %>%
  summarise(n = n()) %>%
  arrange(-n) #%>% view("count")

# Check individual tokens with view()
master_tokens_tbl %>% filter(token == "ndjon") %>%
  arrange(text_nchar) #%>% view("token")




