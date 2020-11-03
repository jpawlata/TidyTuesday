library(tidyverse)

# Get the Data
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# Remove first column
df <- ikea %>%
  select(-c(X1))

na_check_cat <- sum(is.na(df$category)) # no na
na_check_price <- sum(is.na(df$price))  # no na

# Mean price by category
mean_price_cat <- df %>%
  group_by(category) %>%
  summarize(mean_price = mean(price)) %>%
  arrange(desc(mean_price))


### Plot

# Caption text
caption_text = expression(paste(italic("Data source: "), bold("Ikea "), italic("& "), bold("Kaggle"), italic(" | Graphic: "), bold("Justyna Pawlata")))

# Fonts
font_family = 'Verdana'

# Colors:
text_color = '#595959'
bar_color = '#0051ba'
back_color = '#ffda1a'

plot <- ggplot() + 
  geom_bar(data = mean_price_cat, 
           stat = 'identity', 
           mapping = aes(x = reorder(category, mean_price), y = mean_price),
           fill = bar_color) +
  coord_flip() + 
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = back_color),
    panel.grid.major = element_line(size = 0.3, linetype = 'solid', colour = "white"), 
    panel.grid.minor = element_line(size = 0.2, linetype = 'solid', colour = "white"),
    plot.title = element_text(hjust = 0, face = 'bold', size = 28, family = font_family, color = text_color, margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.subtitle = element_text(hjust = 0, family = font_family, color = text_color, margin = margin(10, 0, 10, 0, unit = "pt"), size = 13),
    plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
    axis.title.y = element_text(family = font_family, color = text_color),
    axis.title.x = element_text(family = font_family, color = text_color, size = 11, face = "bold", margin = margin(10, 0, 10, 0, unit = "pt")),
    axis.text.x = element_text(angle = 0, family = font_family),
    axis.text.y = element_text(angle = 0, family = font_family, size = 11),
    legend.position = "none"
  ) +
  labs(
    title = 'Prices in IKEA',
    subtitle = 'Mean price comparison for IKEA products by the furniture category:',
    caption = caption_text,
    x = element_blank(),
    y = expression(paste('mean price (in SAR, 1SAR', ""%~~% 0.27, ' USD)'))
  )

plot

ggsave("2020/2020-11-03/ikea.png", plot = plot, width = 30, units = "cm")
