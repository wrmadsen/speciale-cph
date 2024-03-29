## Master

# Preparation ----
source("R/01_packages.R")

source("R/01_utils.R")

# Load data ----
source("R/01_load_raw_data.R", verbose = TRUE)

# Scrape data ----
# source("R/02_scrape_radio_ndeke.R")
# 
# source("R/02_scrape_radio_lengo.R")
# 
# source("R/02_scrape_ndjoni_sango.R")
# 
# source("R/02_scrape_radio_reseau.R")

# Format covariates ----
source("R/03_format_covariates.R")

# Format other -----
# 
# source("R/04_format_radio_and_digital.R")
# 
# source("R/05_create_dfm_tokens.R")
# 
# save(master_dt, master_dfm, master_tokens, master_tokens_tbl, afinn, popular_tokens,
#      file = "data-formatted/formatted_data.RData")

# Calculations and analysis ----
load("data-formatted/formatted_data.RData")

#source("R/13_fit_stm.R")

#source("R/14_optimal_k.R")

source("R/15_analyse_stm.R")

source("R/16_calculate_sentiment.R")

source("R/17_analyse_sentiment.R")

source("R/18_count_documents.R")








