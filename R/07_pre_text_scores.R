# Pre-text scores

library(preText)

documents <- master_text %>%
  slice_sample(n = 100) %>%
  corpus()

documents

# Pre-process
preprocessed_documents <- factorial_preprocessing(documents,
                                                  use_ngrams = TRUE,
                                                  infrequent_term_threshold = 0.05,
                                                  verbose = FALSE)

# preText
preText_results <- preText(preprocessed_documents,
                           dataset_name = "CAR media",
                           distance_method = "cosine",
                           num_comparisons = 20,
                           verbose = FALSE)

# Plot
preText_score_plot(preText_results)

regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)

