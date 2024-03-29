# Utils

# Specific events ----
specific_events <- tibble("date" = as.Date(c("2020-12-27",
                                             "2021-03-15", # circa
                                             "2021-11-15", # circa
                                             "2022-02-24")),
                          "text" = c("Touadéra wins election.",
                                     "Republican Dialogue initiated.",
                                     "Opposition leave Dialogue.",
                                     "Russia invades Ukraine."
                          ))

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

## Remove patterns and URLs
remove_patterns_in_post <- function(input){
  
  # Special characters
  # And l'
  random <- c("@", "#", "\\.", "\\,", ":", ";",
              "\\/", "\\(", "\\)",
              #"[^\x01-\x7F]", # remove all non-ASCII, emojis
              '"', "\\'",
              "\\!", "\\?", "・",
              "l'", "L'", "L’", "l’",
              "d'", "D'", "D’", "d’",
              "<", "[", ".", "]", ">",
              "«", "»",
              "=")
  
  # Quotes
  quotes <- c("['‘’”“]")
  
  # http, URLs
  urls <- c("http.*", "https.*")
  
  # Combine those that are to be removed completely
  remove_completely <- c(random, quotes, urls) 
  remove_completely <- paste0(remove_completely, collapse = "|")
  
  # Replace some with space
  replace_w_spaces <- c("\\_", "\\-", "\\—")
  replace_w_spaces <- paste0(replace_w_spaces, collapse = "|")
  
  # Remove and replace
  output <- input %>%
    gsub(remove_completely, "", .) %>%
    gsub(replace_w_spaces, " ", .)
  
  output
  
}


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
greend_speciale <- "#013220"
greenm_speciale <- "#467548"
greenl_speciale <- "#90ee90"
purple_speciale <- "#522175"
blued_speciale <- "#2e00c4"
bluel_speciale <- "#2A6DBE"
bluel2_speciale <- "#ADD8E6"
red_speciale <- "#B42418"
redd_speciale <- "#8B0000"
redl_speciale <- "#FFCCCB"
pink_speciale <- "#FFBFBE"
grey_speciale <- "#DFE0DF"
black_speciale <- "#1F1926"
yellow_speciale <- "#F9D71C"
gold_speciale <- "#D4AF37"
crimson_red <- "#DC143C"

# Groups-specific
colours_groups <- c("Ndjoni Sango (RUS)" = redd_speciale,
                    "Radio Lengo Songo (RUS)" = redd_speciale,
                    "Radio Ndeke Luka" = bluel2_speciale,
                    "RJDH" = bluel2_speciale,
                    "Pro-Russian media" = redd_speciale,
                    "Benchmark media" = bluel2_speciale)

# http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
lines_group <- c("Ndjoni Sango (RUS)" = "solid",
                 "Radio Lengo Songo (RUS)" = "dashed",
                 "Radio Ndeke Luka" = "solid",
                 "RJDH" = "dashed")

# http://sape.inf.usi.ch/quick-reference/ggplot2/shape
points_group <- c("Ndjoni Sango (RUS)" = 16,
                  "Radio Lengo Songo (RUS)" = 17,
                  "Radio Ndeke Luka" = 5,
                  "RJDH" = 6,
                  "Pro-Russian media" = 16,
                  "Benchmark media" = 5)

# Annotation size ----
# when text size
text_size <- 15
anno_size <- text_size*(5/14)

# Font -----
theme_font <- "Arial"

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
        legend.key = element_blank(),
        legend.key.size =  unit(0.5, "in")) +
  theme(strip.text = element_text(family = theme_font, size = unit(text_size, "mm")),
        strip.background = element_rect(fill = "white")) +
  theme(legend.key.size =  unit(0.7, "in"))



# Table theme ----
# flextable
set_flextable_defaults(
  font.size = 14,
  theme_fun = theme_alafoli, #theme_booktabs,
  font.family = theme_font,
  cwidth = 1.2,
  cheight = 0.5,
  padding = 0,
  layout = "autofit",
  width = 0.8,
  background.color = "white")


# Save function ----
save_plot_speciale <- function(name, width = 28.6, height = 17.9){
  
  # Use heights of macbook for ease
  #width <- 28.6 # 50
  #height <- 17.9 # 30
  
  # Save pdf with empty first page
  # use ragg and tiff to deal with font problem?
  ggsave(plot = last_plot(),
         filename = name,
         width = width, height = height, unit = "cm"#, dpi = 72
  )
  
}

