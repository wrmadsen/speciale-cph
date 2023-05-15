# Structural topic models (STMs)

# Fit with covariates ----
# Values of K to find optimal one
values_of_k <- c(20, 30, 40, 50, 60, 70, 80, 100)

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


# Save models
save(many_models, file = "data-formatted/many_models.Rdata")


# Fit by content --------
# master_stm_content <- stm(documents = master_dfm,
#                           content = ~sub_group,
#                           K = 30,
#                           seed = 12345)
# 
# save(master_stm_content, file = "data-formatted/master_stm_content.Rdata")
# 
# 
# plot(master_stm_content, 
#      topics = c(3),
#      type = "perspectives")



