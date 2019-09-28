library(tidyverse)
library(reshape2)
  
  
# Import data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
  
park_visits_sub <- park_visits %>% 
  select(year, region, state, unit_code, unit_name, unit_type, visitors) %>% 
  drop_na() %>% #drop rows with NA
  filter(year != "Total") %>%
  mutate(year = as.integer(year)) %>%
  arrange(year) %>%
  group_by(year, state) %>%
  summarise(visitors_total = sum(visitors))
  
# 5 states with the highest number of total visitors
top = 5
state_max <- aggregate(visitors_total ~ state, data = park_visits_sub, FUN = sum) %>% arrange(desc(visitors_total)) %>% head(n=top)

## TODO - create a function

state_1 <- park_visits_sub %>% filter(state == state_max$state[1]) %>% rename(!!paste(state_max$state[1], 'total_visitors', sep = '_') := visitors_total)
state_2 <- park_visits_sub %>% filter(state == state_max$state[2]) %>% rename(!!paste(state_max$state[2], 'total_visitors', sep = '_') := visitors_total)
state_3 <- park_visits_sub %>% filter(state == state_max$state[3]) %>% rename(!!paste(state_max$state[3], 'total_visitors', sep = '_') := visitors_total)
state_4 <- park_visits_sub %>% filter(state == state_max$state[4]) %>% rename(!!paste(state_max$state[4], 'total_visitors', sep = '_') := visitors_total)
state_5 <- park_visits_sub %>% filter(state == state_max$state[5]) %>% rename(!!paste(state_max$state[5], 'total_visitors', sep = '_') := visitors_total)
  
state_max_df <- state_1 %>%
  left_join(state_2, by = 'year') %>% 
  left_join(state_3, by = 'year') %>%
  left_join(state_4, by = 'year') %>%
  left_join(state_5, by = 'year') %>%
  select(-c('state.x','state.y', 'state.x.x', 'state.y.y', 'state'))

# Data reorganization for the line plot
state_max_dfmelt <- melt(state_max_df, id = "year")
 

## Plot
  
# Caption text
caption_text = expression(paste("Data source: ", bold("Data.world"), " | Graphic: ", bold("Justyna Pawlata")))
  
# Fonts
font_family = 'Verdana'
text_color = '#595959'

# Remove scientific notation on axis y
options(scipen=5)
multipl = 1000000 # 1 mln
  
plot <- ggplot(state_max_dfmelt, aes(x = year, y = value/multipl, group = variable, colour = variable)) + 
  geom_line() + 
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
    axis.text.x = element_text(family = font_family),
    
    # Legend
    legend.title = element_text(hjust = 0.5, 
                                family = font_family, 
                                color = text_color),
    legend.text = element_text(color = text_color,
                               family = font_family),
    legend.justification=c(0,0),
    legend.position=c(0.1,0.695),
    legend.box.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"),
    legend.key.width = unit(2,"cm"),
    legend.background = element_rect(fill = 'white', colour = 'transparent')
  ) +
  labs(
    title = 'National Park Visits',
    subtitle = 'The total number of visitors since 1904 till 2016\n(for the TOP 5 states with the highest number of parks\' guests)',
    caption = caption_text,
    x = element_blank(),
    y = expression(paste('Total number of visitors (', 10^{6}, ')'))
  ) + 
  #scale_color_discrete(name = 'States:', labels = c('CA', 'NC', 'DC', 'VA', 'NY'))
  scale_color_discrete(name = 'States:', labels = c('California', 'North Carolina', 'District of Columbia', 'Virginia', 'New York'))
  
  plot