library(tidyverse)
library(RColorBrewer)
library(scales)
library(ggtext)



# Data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


tbi_age_df <- tbi_age %>%
  filter(injury_mechanism == 'Motor Vehicle Crashes') %>%
  filter(!age_group == '0-17') %>%
  filter(!age_group == 'Total') %>%
  select(-c(rate_est, injury_mechanism)) %>%
  mutate(age_group_2 = factor(age_group, levels = c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75+'))) %>%
  select(-c(age_group)) %>%
  rename(age_group = age_group_2)



# Plot

# Caption text
caption_text = expression(paste("Data source: ", bold("CDC"), " | Graphic: ", bold("Justyna Pawlata")))

# Fonts
font_family = 'Verdana'

# Colors:
text_color = "grey80"

plot <- ggplot(tbi_age_df, aes(x = age_group, y = number_est, fill = type)) +
  geom_bar(stat = "identity", position="fill") + 
  scale_y_continuous(labels=percent) + 
  theme_minimal() +
  scale_fill_brewer(palette="Blues", direction = -1) + 
  theme(
    plot.background = element_rect(fill = "#787878", color = "#787878"),
    panel.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(hjust = 0, face = 'bold', size = 22, family = font_family, color = text_color),
    plot.subtitle = element_markdown(hjust = 0, family = font_family, color = text_color, lineheight = 1.4, size = 14),
    plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
    axis.title.x = element_text(family = font_family, color = text_color, margin = margin(c(20, 0, 0, 0)), face = 'bold'),
    axis.title.y = element_text(family = font_family, color = text_color, face = 'bold', margin = margin(c(0, 10, 0, 0))),
    axis.text.x = element_text(family = font_family, color = text_color, size = 10, face = 'bold'), 
    axis.text.y = element_text(family = font_family, color = text_color, face = 'bold'),
    legend.position = "top",
    legend.justification = "center",
    legend.direction="horizontal",
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    legend.text = element_text(family = font_family, color = text_color)
    ) + 
  labs(
    title = 'Traumatic Brain Injury - Motor Vehicle Crashes (2014)',
    subtitle = 'Starting from children age 5-14 percent of <span style=\'color: #deeaf6;\'>**TBI Hospitalizations**</span> caused by motor vehicle crashes increases.<br />
    Percent of <span style=\'color: #3282bd;\'>**Deaths**</span> increases as well.',
    caption = caption_text,
    x = "Age",
    y = 'Percent of cases'
  )
    
plot
