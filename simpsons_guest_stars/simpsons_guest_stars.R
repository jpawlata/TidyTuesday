  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(png)
  library(grid)
  library(ggimage)
  
  # Import data
  simpsons <- simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")
  
  # Remove NA and rows with 'Movie':
  simpsons <- na.omit(simpsons)
  simpsons <- subset(simpsons, simpsons$season != 'Movie')
  
  # Add the 'Season' text to the season column
  simpsons$season <- paste("Season:", simpsons$season, sep = " ")
  
  # How many times a guest star shows in a season:
  guest_per_season <- simpsons %>% group_by(season, guest_star) %>% summarise(freq = n()) 
  
  # Total sum of guest stars per season
  sum_per_season <- aggregate(freq ~ season, data = guest_per_season, FUN = sum) %>% rename(sum = freq) %>% arrange(desc(sum))
  
  # The max number of times the top guest star shows in a season
  max_per_season <- aggregate(freq ~ season+guest_star, data = guest_per_season, FUN = max) %>% rename(top_guest_star = guest_star) %>% arrange(desc(freq)) %>% distinct(season, .keep_all = TRUE)
  
  # Join above data by season column
  simpsons_df <- inner_join(sum_per_season, max_per_season, by = 'season')
  
  
  
  # Plot
  
  # Caption text
  caption_text = expression(paste("Data source: ", bold("Wikipedia"), " & ", bold("Andrew Collier"), " | Graphic: ", bold("Justyna Pawlata")))
  
  # Fonts
  font_family = 'Verdana'
  
  # Colors:
  bar_color = '#FED41D'
  text_color = '#595959'
  
  # Image - TODO !!! - change path
  # (img: https://imgbin.com/png/ZwdSQDYa/bart-simpson-homer-simpson-marge-simpson-lisa-simpson-maggie-simpson-png)
  image <- readPNG('/home/justyna/Projects/Rscripts/tidytuesday/simpsons_guest_stars/image/simpson_tr.png')
  
  
  plot <- ggplot() + 
    geom_bar(data = simpsons_df, 
             stat = 'identity', 
             mapping = aes(x = reorder(season, -sum), y = sum), 
             fill = bar_color) + 
    geom_point(data = simpsons_df, 
               mapping = aes(x = reorder(season, -sum), y = freq), 
               color = text_color, 
               shape = 18, 
               size = 3) +
    geom_text(data = simpsons_df, 
              mapping = aes(x = reorder(season, -sum), y = freq, label = top_guest_star), 
              hjust = -0.1, 
              fontface = 4, 
              size=3.5, 
              color = text_color, 
              angle = 90, 
              family = 'Verdana') +
    geom_point(data=data.frame(x = 15.5, y = 53), 
               mapping = aes(x = x,y = y), 
               color = text_color, 
               shape = 18, 
               size = 3) +
    geom_text(data=data.frame(x = 16, y = 53), 
              mapping = aes(x = x,y = y), 
              color = text_color, 
              hjust = 0, 
              fontface = 4, 
              size=4, 
              family = 'Verdana', 
              label = 'Top guest star per season \n(mark shows the number of appearances)') + 
    annotation_custom(rasterGrob(image, interpolate=TRUE), 
                      xmin = 18, 
                      xmax = 38, 
                      ymin = 46, 
                      ymax = 66) + 
    theme_minimal() + 
    theme(
      plot.title = element_text(hjust = 0, 
                                face = 'bold', 
                                size = 20, 
                                family = font_family, 
                                color = text_color),
      plot.subtitle = element_text(hjust = 0, 
                                   family = font_family, 
                                   color = text_color),
      plot.caption = element_text(hjust = 1, 
                                  family = font_family, 
                                  color = text_color),
      axis.title.y = element_text(family = font_family, 
                                  color = text_color),
      axis.text.x = element_text(angle = 45, 
                                 family = font_family)
    ) +
    labs(
      title = 'Simpsons Guest Stars',
      subtitle = 'Total sum of guest stars per season',
      caption = caption_text,
      x = element_blank(),
      y = 'Sum of guest stars per season'
    )
  
  plot