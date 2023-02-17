# Explore corpus


# Keywords-in-context
master_tokens %>%
  kwic(., pattern = "russ", window = 3)


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

topfeatures(master_dfm_tf_idf)

topfeatures(master_dfm)

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
# Higher score, more similar to group_1 (here, this is Twitter)
fw_scores <- fightin_words(master_dfm, covariate = "group", "Twitter", "Radio") %>%
  tibble()

fw_scores %>%
  arrange(-score)# %>% view



# Dates -----

master_tokens_tbl %>%
  filter(group == "Twitter") %>%
  .$date %>% summary()






