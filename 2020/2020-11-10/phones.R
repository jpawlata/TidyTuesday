library(tidyverse)

# Get the Data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')


df <- mobile %>%
  select(c(entity, code, year, total_pop, mobile_subs, continent)) %>%
  filter(continent == "Europe") %>%
  rename(country = entity) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(mobile_subs)) %>%
  select(-c(continent)) %>%
  group_by(country, year)


# Find top 9 countries with the biggest difference between first and last mobile_subs
#df_temp <- aggregate(mobile_subs ~ country, df, FUN = function(i) max(i) - min(i)) - # old solution)
df_temp <- df %>%
  group_by(country) %>%
  summarize(diff_by_country = last(mobile_subs) - first(mobile_subs)) %>%
  arrange(desc(diff_by_country)) %>%
  top_n(9)


# Keep the top 9 countries only
df <- df %>%
  filter(country %in% df_temp$country)



### Plot

# Caption text
caption_text = expression(paste(italic("Data source: "), bold("Mobile vs Landline subscriptions"), italic(" | Graphic: "), bold("Justyna Pawlata")))

# Fonts
font_family = 'Verdana'

# Colors:
text_color = '#595959'
bar_color = 'steelblue'
back_color = '#ffffff'

plot <- ggplot(data = df, aes(x = year, y = mobile_subs)) + 
  geom_bar(stat = 'identity', fill = bar_color) + 
  facet_wrap(~ country, ncol = 3) + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = back_color),
    plot.title = element_text(hjust = 0, face = 'bold', size = 28, family = font_family, color = text_color, margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.subtitle = element_text(hjust = 0, family = font_family, color = text_color, margin = margin(10, 0, 10, 0, unit = "pt"), size = 13),
    plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
    axis.title.y = element_text(family = font_family, color = text_color, size = 12, face = "bold", margin = margin(0, 10, 0, 10, unit = "pt")),
    axis.title.x = element_text(family = font_family, color = text_color, size = 12, face = "bold", margin = margin(10, 0, 10, 0, unit = "pt")),
    axis.text.x = element_text(angle = 0, family = font_family),
    axis.text.y = element_text(angle = 0, family = font_family, size = 11),
    legend.position = "none", 
    strip.text = element_text(face = "bold", size = 10, family = font_family, color = text_color)
  ) +
  labs(
    title = 'Countries with the highest increase in number of mobile subs in Europe',
    subtitle = 'Top 9 European countries with the highest mobile subscriptions increase between the years 1990 and 2017',
    caption = caption_text,
    x = 'Year',
    y = 'Mobile subscriptions (per 100 people)'
  )

plot
ggsave("2020/2020-11-10/phones.png", plot = plot, width = 50, height = 20, units = "cm")

