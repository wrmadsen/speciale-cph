# Utils

# Map df function with a progress bar
map_df_progress <- function(.x, .f, ..., .id = NULL) {
  
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  
  purrr::map_df(.x, f, ..., .id = .id)
  
}