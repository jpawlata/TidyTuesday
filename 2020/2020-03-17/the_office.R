library(tidyverse)
library(magick)
library(transformr)
library(gganimate)
library(RColorBrewer)
library(gifski)
library(ggtext)



# Data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

office_ratings$season <- paste("Season:", office_ratings$season, sep = " ")

office_ratings_m <- mean(office_ratings$imdb_rating)

office_ratings_max <- office_ratings %>%
  group_by(season) %>% arrange(desc(imdb_rating), .by_group = TRUE) %>%
  top_n(1, imdb_rating)

office_ratings_m_val <- office_ratings %>%
  group_by(season) %>% arrange(imdb_rating, .by_group = TRUE) %>%
  summarise(
    max_r=last(imdb_rating), 
    min_r=first(imdb_rating), 
    max_episode=last(episode), 
    min_episode = first(episode),
    max_title = last(title),
    min_title = first(title)
    )
  
  

# Plot

# Caption text
caption_text = expression(paste("Data source: ", bold("schrute"), " & ", bold("IMDB"), " | Graphic: ", bold("Justyna Pawlata")))

# Fonts
font_family = 'Verdana'

# Colors:
text_color = '#595959'


plot <- ggplot(office_ratings, aes(episode, imdb_rating, group = season, color = as.factor(season))) +
  geom_line() +
  geom_hline(yintercept = office_ratings_m, linetype = 2, color = 'red') + 
  geom_text(aes(x = 1, y = office_ratings_m, label = round(office_ratings_m, digits = 2)), vjust = -0.3, color = 'red', family = font_family) +
  scale_y_continuous() + 
  geom_segment(aes(xend = 26, yend = imdb_rating), linetype = 2, colour = 'grey') + 
  geom_point(aes(group = seq_along(episode))) +
  geom_text(aes(x = max(episode) + 0.2, label = season), hjust=0) +
  xlim(1, 27) + 
  scale_color_viridis_d() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = 'bold', size = 28, family = font_family, color = text_color),
    plot.subtitle = element_markdown(hjust = 0, family = font_family, color = text_color, lineheight = 1.4, size = 14, margin=margin(15,0,0,0)),
    plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
    axis.title.y = element_text(family = font_family, color = text_color),
    axis.title.x = element_text(family = font_family, color = text_color),
    legend.position = "none"
  ) + 
  transition_reveal(episode) + 
  coord_cartesian(clip = 'off') + 
  labs(
    title = "'The Office' Series - IMDB Rating",
    subtitle = "**Season 8: Episode 19** has the lowest IMDB rating (Title: *Get the Girl*, rating: *6.7*)<br />
    The highest rating belongs to two episodes: **Season 7 Episode 21** (Title: *Goodbye, Michael*, rating: *9.7*) <br />
    and **Season 9 Episode 23** (Title: *Finale*, rating: *9.7*)<br />
    (Each point shows the rating of one episode, <span style='color: red;'>red line</span> shows the average rating for all 'The Office' episodes)",
    caption = caption_text,
    x = "Episode", 
    y = "IMDB Rating"
  )


# Plot
the_office_gif <- animate(plot, 50, fps = 10, duration = 30, width = 850, height = 650, renderer = gifski_renderer())
# Save as gif
anim_save("2020/2020-03-17/the_office.gif", animation = the_office_gif)

