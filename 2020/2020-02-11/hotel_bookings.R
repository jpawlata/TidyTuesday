library(tidyverse)
library(waffle)
library(ggplot2)
library(viridis)  # Color palette
library(ggthemes)

# Get the data:
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
hotels$arrival_date_month <-  factor(hotels$arrival_date_month, levels = rev(months), ordered=TRUE)

arrival_day <- drop_na(hotels) %>%
  select(arrival_date_year, arrival_date_month, arrival_date_day_of_month) %>%
  count(arrival_date_year, arrival_date_month, arrival_date_day_of_month) %>%
  rename(sum_arrival_day = n) %>%
  arrange(arrival_date_year)


# Plot function:
# https://vuorre.netlify.com/post/2016/03/24/github-style-waffle-plots-in-r/
# (adapted and modified)

gh_waffle <- function(arrival_day, pal = "D", dir = -1){
  
  p <- ggplot(arrival_day, aes(x = arrival_date_day_of_month, y = arrival_date_month, fill = sum_arrival_day)) +
    scale_fill_viridis(name="Arrival Day", 
                       option = pal,  # Variable color palette
                       direction = dir,  # Variable color direction
                       na.value = "grey93",
                       limits = c(0, max(arrival_day$sum_arrival_day))) +
    geom_tile(color = "white", size = 0.4) +
    facet_wrap("arrival_date_year", ncol = 1) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 31, length = 4),
      labels = c('1', '11', '21', '31')) +
    theme_minimal() + 
    #theme_tufte(base_family = "Verdana") +
    theme(axis.ticks = element_blank(),
          legend.position = "right",
          legend.key.width = unit(0.5, "cm"),
          legend.title = element_text(color = '#595959', size = 11),
          strip.text = element_text(hjust = 0.01, face = "bold", size = 10, lineheight = 1.1, color = '#595959'),
          plot.title = element_text(hjust = 0, face = 'bold', size = 20, margin=margin(20,0,12,0), color = '#595959'),
          plot.subtitle = element_text(hjust = 0, size = 11, lineheight = 1.2, face = "italic", color = '#595959'),
          plot.caption = element_text(hjust = 1, color = '#595959'),
          axis.title.x = element_text(hjust = 0.5, color = '#595959', size = 10)) + 
    labs(
      title = 'Which day of the month is the best to start your holidays?',
      subtitle = 'Each tile shows one day of a month. Colors of the tiles reflect total number of arrivals at the hotels. \n(Based on the 119386 arrival records form July 2015 to August 2017)',
      caption = caption_text,
      x = "Day of a month",
      y = element_blank()
    )
  
  print(p)
}

caption_text = expression(paste("Data source: ", bold("Antonio, Almeida, and Nunes, 2019")))
gh_waffle(arrival_day)
