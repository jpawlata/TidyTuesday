library(tidyverse)
library(tidytext)
library(fishualize)
library(ggtext)
    
    
# Get the Data
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
    
# Words to remove
data("stop_words")
title = c("animal", "crossing", "new", "horizons")
    
    
df <- critic %>% arrange(desc(grade))
    
df_words <- df %>%
  select(c(text, grade, publication)) %>%
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>%
  mutate(line_no = row_number()) %>%
  mutate(text = tolower(text))
    
df_tokens <- df_words %>%
  unnest_tokens(words, text) %>%
  anti_join(stop_words, by = c("words" = "word")) %>%
  filter(!words %in% title) %>%
  count(words, sort = TRUE) %>%
  slice(1:25) %>%
  rename(number = n)
    
# Plot
    
# Caption text
caption_text = expression(paste(italic("Data source: "), bold("Villager DB"), italic(" | Graphic: "), bold("Justyna Pawlata")))
    
# Fonts
font_family = 'Verdana'
    
# Colors:
text_color = '#595959'
    
plot <- ggplot() + 
  geom_bar(data = df_tokens, 
           stat = 'identity', 
           mapping = aes(x = reorder(words, number), y = number, fill = reorder(words, number)),
         ) + 
  coord_flip() + 
  scale_fill_fish_d(option = "Naso_lituratus") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', size = 25, family = font_family, color = text_color, margin = margin(10, 0, 10, 0)),
    plot.subtitle = element_markdown(hjust = 0, family = font_family, color = text_color, lineheight = 1.3, size = 12, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 1, family = font_family, color = text_color, face = "italic"),
    axis.title.y = element_text(family = font_family, color = text_color),
    axis.title.x = element_text(family = font_family, color = text_color, margin = margin(10, 0, 10, 0)),
    legend.position = "none"
  ) +
  labs(
    title = 'What do we know about the game from the critics review?',
    subtitle = '**Animal Crossing - New Horizons** is a <span style=\'color: #f3e915;\'>**nintendo**</span> <span style=\'color: #e9ef6f;\'>**switch**</span> <span style=\'color: #ff9100;\'>**game**</span>, another title in the <span style=\'color: #feb400;\'>**series**</span>.<br />
    Game takes place on the <span style=\'color: #fbcd00;\'>**island**</span>, where <span style=\'color: #6d7bae;\'>**crafting**</span> is one of our main activities.',
    caption = caption_text,
    x = element_blank(),
    y = 'The most common words in the "Animal Crossing - New Horizons" critics reviews'
  )
    
plot
