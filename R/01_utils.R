# Utils

# Various functions ----
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

# Remove accents from strings
remove_accents <- function(input){
  
  stri_trans_general(str = input, id = "Latin-ASCII")
  
}


# Theme -----
# speciale theme

# Date labels ----
dateformat <- function(){
  function(x)
  {
    m <- format(x,"%b")
    y <- format(x, "'%y")
    ifelse(duplicated(y), m, paste(m, y))
  }
}

# Colours -----

# General
orange_speciale <- "#FF9233"
brown_speciale <- "#79481f"
greend_speciale <- "#898a74"
greenm_speciale <- "#467548"
greenl_speciale <- "#1b990f"
purple_speciale <- "#522175"
blued_speciale <- "#2e00c4"
bluel_speciale <- "#2A6DBE"
red_speciale <- "#B42418"
pink_speciale <- "#FFBFBE"
grey_speciale <- "#DFE0DF"
black_speciale <- "#1F1926"
yellow_speciale <- "#F9D71C"
gold_speciale <- "#D4AF37"
crimson_red <- "#DC143C"

# Groups-specific
colours_groups <- c("Radio Ndeke Luka" = greenm_speciale,
                    "Radio Lengo Songo" = gold_speciale,
                    "Reseau des journalistes" = bluel_speciale,
                    "Non-Russian total" = grey_speciale)

# Annotation size ----
# when text size
text_size <- 15
anno_size <- text_size*(5/14)

# Font -----
theme_font <- "Garamond"

#font_import()
#loadfonts()

# Theme -----
theme_speciale <- theme(axis.text = element_text(size = unit(text_size, "mm"),
                                            family = theme_font, colour = "black"),
                   axis.title = element_text(size = unit(text_size, "mm"), family = theme_font),
                   #axis.title.y = element_blank(),
                   axis.ticks = element_blank()) +
  theme(plot.title = element_text(size = unit(text_size*1.5, "mm"), family = theme_font),
        plot.subtitle = element_text(size = unit(text_size, "mm"), family = theme_font),
        plot.caption=element_text(size = unit(text_size, "mm"), family = theme_font)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray50", linewidth = 0.2)
  ) +
  theme(legend.position = "top",
        legend.text = element_text(family = theme_font, size = unit(text_size, "mm")),
        legend.title = element_text(family = theme_font, size = unit(text_size, "mm")),
        legend.key = element_blank()) +
  theme(strip.text = element_text(family = theme_font, size = unit(text_size, "mm")),
        strip.background = element_rect(fill = "white"))

# Save function ----
save_plot_speciale <- function(name, width = 28.6, height = 17.9){
  
  # Use heights of macbook for ease
  #width <- 28.6 # 50
  
  #height <- 17.9 # 30
  
  #plot_width/plot_height # 1.666667
  
  # Save pdf with empty first page
  # use ragg and tiff to deal with font problem?
  ggsave(plot = last_plot(),
         filename = name,
         width = width, height = height, unit = "cm"#, dpi = 72
  )
  
}

