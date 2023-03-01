## Master

# Preperation ----
source("R/01_packages.R")

source("R/01_utils.R")

# Load and scrape data ----

source("R/01_load_raw_data.R", verbose = TRUE)

#source("R/02_scrape_radio_ndeke.R")

#source("R/02_scrape_radio_lengo.R")

#source("R/03_data_to_get_twitter_keywords.R")

# Format data ----

source("R/03_format_covariates.R")

source("R/04_format_radio.R")

source("R/04_format_twitter_keywords.R")

#source("R/04_find_twitter_users.R")

source("R/05_create_dfm_tokens.R")

# Calculations and analysis ----

source("R/07_calculate_similarity.R")

#source("R/08_calculate_sentiment.R")

# Plot ----

source("R/20_plot_descriptive.R")

source("R/21_plot_media_spikes.R")

source("R/22_prepare_token_plots.R")

source("R/23_plot_tokens.R")

