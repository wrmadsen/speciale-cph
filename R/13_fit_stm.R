# Structural topic models (STMs)

# Fit with covariates ----
# Values of K to find optimal one
values_of_k <- c(10, 15, 20, 25, 30, 35, 40, 45)

# Fit models with parallel approach
# furrr, progressr, future

# Set up parallel sessions
plan(multisession)

fit_stm <- function(values_of_k){
  
  p <- progressor(steps = length(values_of_k))
  
  future_map(values_of_k,
             ~{
               p()
               Sys.sleep(.2)
               stm::stm(documents = master_dfm,
                        prevalence = ~sub_group,
                        K = .x,
                        seed = 12345)
             },
             seed = TRUE)
  
}

# Run
many_models <- with_progress({
  
  result <- fit_stm(values_of_k)
  
})


# Save models ----
#save(many_models, file = "data-formatted/many_models.Rdata")

# Fit and save single model to get it done quick ----
# K = 20 based on the results of 14_optimal_K.R
master_stm <- stm::stm(documents = master_dfm,
                       prevalence = ~sub_group,
                       K = 20,
                       seed = 12345)

save(master_stm, file = "data-formatted/master_stm.Rdata")

# Save and load
# (master_stm <- many_models[[3]])
# save(master_stm, file = "data-formatted/master_stm.Rdata")






