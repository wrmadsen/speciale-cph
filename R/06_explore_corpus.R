# Explore corpus


# keywords-in-context
master_dfm %>%
  filter()
kwic(., pattern = "russi")

ntoken(master_dfm)


# Lexical diversity
ntoken(master_dfm) %>%
  tibble()
ntype(master_dfm)

plot(ntoken(master_dfm),
     ntype(master_dfm),
     xlab = "N tokens",
     ylab = "N types")

# token-to-type ratio
master_ttr <- ntoken(master_tokens)/ntype(master_tokens)

plot(docvars(master_tokens)$group,
     master_ttr,
     xlab = "Year",
     ylab = "Token-Type Ratio")

master_tokens


# Calculate similarity

m_dfm_tf_idf <- twitter_dfm %>% dfm_tfidf()
radio_dfm_tf_idf <- radio_dfm %>% dfm_tfidf()

topfeatures(twitter_dfm_tf_idf)
topfeatures(radio_dfm_tf_idf)


# Cosine similarity -----
cosine_sim <- function(a, b){
  
  # Calculate the inner product of the two vectors
  numerator <- sum(a * b)
  
  # Calculate the magnitude of the first vector
  magnitude_a <- sqrt(sum(a^2))
  
  # Calculate the magnitude of the second vector
  magnitude_b <- sqrt(sum(b^2))
  
  # Calculate the denominator
  denominator <- magnitude_a * magnitude_b
  
  # Calculate the similarity
  cos_sim <- numerator/denominator
  
  return(cos_sim)
  
}

topfeatures(master_dfm)

topfeatures(master_dfm_tf_idf)

# Calculate the cosine similarity
cosine_sim <- textstat_simil(x = master_dfm_tf_idf, 
                             y = master_dfm_tf_idf[docvars(master_dfm_tf_idf)$group == "Twitter",],
                             method = "cosine")

# Assign variable to data.frame
master_dfm_tf_idf$cosine_sim <- as.numeric(cosine_sim)


# Fightin' words ----
fightin_words <- function(dfm_input, covariate, group_1 = "Twitter", group_2 = "Radio", alpha_0 = 1){
  
  # Subset DFM
  fw_dfm <- dfm_subset(dfm_input, get(covariate) %in% c(group_1, group_2)) 
  fw_dfm <- dfm_group(fw_dfm, get(covariate))
  fw_dfm <- fw_dfm[,colSums(fw_dfm)!=0]
  dfm_input_trimmed <- dfm_match(dfm_input, featnames(fw_dfm))
  
  # Calculate word-specific priors
  alpha_w <- (colSums(dfm_input_trimmed))*(alpha_0/sum(dfm_input_trimmed))
  
  for(i in 1:nrow(fw_dfm)) fw_dfm[i,] <- fw_dfm[i,] + alpha_w
  fw_dfm <- as.dfm(fw_dfm)
  mu <- fw_dfm %>% dfm_weight("prop")
  
  # Calculate log-odds ratio
  lo_g1 <- log(as.numeric(mu[group_1,])/(1-as.numeric(mu[group_1,])))
  lo_g2 <- log(as.numeric(mu[group_2,])/(1-as.numeric(mu[group_2,])))
  fw <- lo_g1 - lo_g2
  
  # Calculate variance
  
  fw_var <- as.numeric(1/(fw_dfm[1,])) + as.numeric(1/(fw_dfm[2,]))
  
  fw_scores <- data.frame(score = fw/sqrt(fw_var),
                          n = colSums(fw_dfm),
                          feature = featnames(fw_dfm))
  
  return(fw_scores)
  
}

# Calculate fw scores
# Higher score, more similar to group_1 (Twitter)
fw_scores <- fightin_words(master_dfm, covariate = "group", "Twitter", "Radio") %>%
  tibble()

fw_scores %>%
  arrange(score) %>% view



