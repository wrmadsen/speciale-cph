# Choose optimal K value

# Load STM data ----
values_of_k <- c(10, 15, 20, 25, 30, 35, 40, 45)

load("data-formatted/many_models.Rdata")

# Optimal K ----
## Calculate ----
results_of_k <- data.frame(K = values_of_k) %>%
  mutate(topic_model = many_models)

heldout <- make.heldout(master_dfm)

results_of_k <- results_of_k %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, master_dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, master_dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
  select(-topic_model)

# Save
#save(results_of_k, file = "data-formatted/results_of_k.Rdata")

# Load K results
load("data-formatted/results_of_k.Rdata")

# Calculate mean
results_of_k_sub <- results_of_k %>%
  tibble() %>%
  transmute(K,
            #residuals = map_dbl(residual, "dispersion"),
            exclusivity = map_dbl(exclusivity, mean),
            #eval_heldout = map_dbl(eval_heldout, "expected.heldout"),
            semantic_coherence = map_dbl(semantic_coherence, mean))

results_of_k_sub

## Numbers 20 and 30 ----
results_of_k_sub %>%
  filter(K %in% c(20, 30)) %>%
  pivot_longer(cols = c(2,3)) %>%
  mutate(value = abs(value)) %>%
  pivot_wider(names_from = K, values_from = value) %>%
  clean_names() %>%
  mutate(x20_to_x30 = ((x30 - x20)/x30)*100,
         x30_to_x20 = ((x20 - x30)/x20)*100
  )

## Plot ----
# Facets
results_of_k_sub %>%
  transmute(K,
            #Residuals = residuals,
            #`Held-out likelihood` = eval_heldout,
            `Semantic coherence` = semantic_coherence,
            Exclusivity = exclusivity) %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -K) %>%
  ggplot(aes(x = K,
             y = Value,
             group = Metric)) +
  geom_line(aes(colour = Metric),
            linewidth = 2, alpha = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 20, linewidth = 1) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_colour_manual(name = "", values = c("Exclusivity" = redd_speciale, "Semantic coherence" = bluel_speciale)) +
  labs(title = "Figure X. Exclusivity and semantic coherence for topic models",
       x = "K (the number of topics)",
       y = NULL) +
  theme_speciale

save_plot_speciale("output/analysis_stm_exclusivity_semantic_coherence.png")



