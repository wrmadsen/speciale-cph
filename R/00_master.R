## Master

# Preparation ----
source("R/01_packages.R")

source("R/01_utils.R")

# Load and scrape data ----

source("R/01_load_raw_data.R", verbose = TRUE)

#source("R/02_scrape_radio_ndeke.R")

#source("R/02_scrape_radio_lengo.R")

# Format data ----

source("R/03_format_covariates.R")

source("R/04_format_radio_and_digital.R")

source("R/05_create_dfm_tokens.R")

# Save formatted data ----
objects_to_save <- list(master_dfm_tf_idf, master_dfm, master_tokens, master_tokens_tbl, feel, token_frequency)
#save(objects_to_save, file = "data-formatted/formatted_data.RData")

# Calculations and analysis ----
load("data-formatted/formatted_data.RData")

#source("R/07_calculate_similarity.R")

#source("R/08_calculate_sentiment.R")

#source("R/09_run_diff_in_diff.R")

#source("R/10_structural_topic_model.R")

# Plot ----

#source("R/20_plot_descriptive.R")

source("R/21_plot_media_spikes.R")

source("R/22_prepare_token_plots.R")

source("R/23_plot_tokens.R")

