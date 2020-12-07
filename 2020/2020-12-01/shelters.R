  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(fishualize)
  library(ggtext)

  
  # Get the Data
  shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')
  
  shelters$sector_f = factor(shelters$sector, levels = c("Youth", "Women", "Men", "Families", "Co-ed"))
  
  shelters_df <- shelters %>%
    select(-c(id, shelter_address, shelter_city, shelter_province, shelter_postal_code)) %>%
    filter(capacity != 0) %>%
    mutate(occupancy_date2 = lubridate::ymd(occupancy_date)) %>%
    mutate(year = year(occupancy_date2)) %>%
    mutate(month = month(occupancy_date2)) %>%
    mutate(year_month = as.Date(as.yearmon(paste0(year, "-", month), "%Y-%m"))) %>%
    mutate(occupancy_rate = (occupancy / capacity) * 100) %>%
    drop_na() 
  
  cr_df <- aggregate(occupancy_rate ~ year_month+sector_f, data = shelters_df, FUN = mean)
  
  sector_mean <- aggregate(occupancy_rate ~ sector_f, data = cr_df, FUN = mean) %>%
    arrange(desc(occupancy_rate))
  # Families, Women, Youth, Men, Coeducational
  
  shelters_df <- shelters_df %>%
    mutate(sector_f = recode_factor(sector_f, "Families" = "Families", "Women" = "Women", "Youth" = "Youth", "Men" = "Men", "Co-ed" = "Coeducational")) %>%
    group_by(sector_f) %>%
    filter(!is.nan(occupancy_rate)) %>%
    arrange(occupancy_date2)
  
  cr_df <- aggregate(occupancy_rate ~ year_month+sector_f, data = shelters_df, FUN = mean)
  
  ### Plot

  # Caption text
  caption_text = expression(paste(italic("Data source: "), bold("opendatatoronto"), italic(" | Graphic: "), bold("Justyna Pawlata")))
  
  # Fonts
  font_family = 'Verdana'
  
  # Colors:
  text_color = '#595959'
  bar_color = '#e8e8e8'
  back_color = '#ffffff'
  
  plot <- ggplot(data = cr_df, aes(x = year_month, y = occupancy_rate, colour = sector_f)) + 
    geom_bar(stat = 'identity', fill = bar_color, colour = bar_color) +
    geom_line(lwd = 1) + 
    geom_point() + 
    geom_hline(yintercept = 90, linetype = "dashed", color = "red", lwd = 0.2) +
    scale_color_fish_d(option = "Coris_gaimard") +
    facet_wrap(~ sector_f, ncol = 1) +
    coord_cartesian(ylim = c(75, 125)) +
    theme_minimal() + 
    theme(
      plot.background = element_rect(fill = back_color),
      plot.title = element_text(hjust = 0, face = 'bold', size = 28, family = font_family, color = text_color, margin = margin(10, 0, 0, 0, unit = "pt")),
      plot.subtitle = element_markdown(hjust = 0, family = font_family, color = text_color, margin = margin(10, 0, 10, 0, unit = "pt"), size = 13, lineheight = 1.4),
      plot.caption = element_text(hjust = 1, family = font_family, color = text_color),
      axis.title.y = element_text(family = font_family, color = text_color, size = 12, face = "bold", margin = margin(0, 10, 0, 10, unit = "pt")),
      axis.title.x = element_text(family = font_family, color = text_color, size = 12, face = "bold", margin = margin(10, 0, 10, 0, unit = "pt")),
      axis.text.x = element_text(angle = 0, family = font_family),
      axis.text.y = element_text(angle = 0, family = font_family, size = 11),
      legend.position = "none", 
      strip.text = element_text(face = "bold", size = 9, family = font_family, color = text_color)
    ) +
    labs(
      title = 'Toronto Shelters - shelters\' occupation rate by sector',
      subtitle = 'Four out of five sectors\' occupancy is <strong>above 90%</strong> (years 2017 - 2019).<br />The two highest occupancy rate belong to the sectors: <span style="color: #006BFF;"><strong>Families</strong></span> and <span style="color: #00957E;"><strong>Women</strong></span>, <span style="color: #7C6D46;"><strong>Coeducational</strong></span> shelters have the lowest occupancy rate in all sectors.',
      caption = caption_text,
      x = 'Year',
      y = 'Occupation rate (%)'
    )
  
  plot
  ggsave("2020/2020-12-01/shelters.png", plot = plot, width = 50, height = 20, units = "cm")
  