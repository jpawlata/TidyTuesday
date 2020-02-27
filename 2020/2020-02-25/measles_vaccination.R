library(tidyverse)
library(usmap)



measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

df <- measles %>%
  select(c(state, mmr)) %>%
  filter(!mmr == -1)

states_mmr <- aggregate(list(mmr = df$mmr), by = list(state = df$state), FUN = mean) %>% arrange(mmr)

## Plot

# Caption text:
caption_text = expression(paste("Data source: ", bold("The Wallstreet Journal"), " | Graphic: ", bold("Justyna Pawlata")))

subtitle_text = ~ atop(paste(bold('Arizona'), " has the lowest Measles, Mumps, and Rubella vaccination rate among children while almost all children are vaccinated in ", bold("Illinois.")), paste(italic("Data: around 46,412 schools across 32 US States.")))

# Fonts:
font_family = 'Verdana'

# Colors:
text_color = '#595959'

  plot_usmap(data = states_mmr, values = "mmr", color = "light grey", labels = TRUE, label_color = "light grey") +
    scale_fill_continuous(low = "#D4B28D", high = "#9E3E3C", name = "School\'s MMR vaccination rate: ") + 
    theme_void() + 
    theme(
      plot.title = element_text(hjust = 0, face = 'bold', size = 22, family = font_family, color = text_color),
      plot.subtitle = element_text(hjust = 0, family = font_family, color = text_color),
      plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
      legend.text = element_text(family = font_family, color = text_color),
      legend.title = element_text(family = font_family, color = text_color, size = 10, face = "bold"),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.justification = "right",
      legend.position = c(1, 0),
      legend.direction="horizontal",
      legend.box.margin=margin(c(0,0,75,0))
      ) + 
    labs(
      title = 'Vaccination rate among children in the United States',
      subtitle = subtitle_text,
      caption = caption_text
      )
