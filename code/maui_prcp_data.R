source("code/local_weather.R")

library(ggtext)
library(scales)



## New plot analyzing precipitation trends (bc that data actually exists)
west_maui_prcp <- local_weather %>%
  select(date, prcp) %>%
  mutate(year = year(date),
         month = as.character(month(date)),
         day = day(date),
         is_this_year = year ==this_year) %>%
  filter(year != 1919) %>%
  group_by(year) %>%
  drop_na(prcp) %>%
  mutate(cum_prcp = cumsum(prcp)) %>% 
  ungroup() %>%
  mutate(new_date = ymd(glue("2024-{month}-{day}"))) 

west_maui_prcp %>%
  filter(year == 2024 & month(date) == 2) %>%
  arrange(-day(date)) %>%
  print(n=20)

this_year <- year(today())
this_month <- month(today(),label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))
today <- today()

annotation <- west_maui_prcp %>%
  slice_max(year(date)) %>%
  slice_max(month(date)) %>%
  slice_max(day(date)) 

## This plot has stopped working because cum_sum(prcp) stopped calculating
## Figured it out: prcp stopped being recorded causing cum_prcp to return NA

# m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10)
# d <- as.data.frame(m)
# d[is.na(d)] <- 0


w <- west_maui_prcp %>%
  ggplot(aes(x = new_date, y = cum_prcp, group = year,
             size = is_this_year)) +
    geom_line(aes(x = new_date, y = cum_prcp, group = year,
                color = is_this_year, size = is_this_year), show.legend = FALSE) +
    geom_text(data = annotation,
            aes(x = new_date, y = cum_prcp, label = glue('{year}'),
                color = is_this_year),
            inherit.aes = FALSE, vjust = -0.7, size =4)+

    scale_x_date(date_breaks = '2 months',
                 date_labels = '%B', expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, 2200, 300),
                     labels = seq(0, 220, 30),
                     expand = c(0, 0)) +
    scale_color_manual(name = NULL,
                     breaks = c(F, T),
                     values = c("lightgrey", "dodgerblue"),
                     labels = c("Past years", "Overall average"),
                     guide = guide_legend(override.aes =list(label = "",
                                                        shape=15, size=5,
                                                        fill = c("lightgrey",
                                                                 "dodgerblue")))
                     ) +
  
    scale_size_manual(breaks = c(F, T),
                    values = c(0.3, 1)) +
    scale_x_date(date_labels = "%B", date_breaks = "2 months", expand = c(0,0)) +
  
    labs(
      x = NULL,
      y = "Cumulative precipitation (cm)",
      title = glue("Through {this_month} {this_year}, the cumulative precipitation
      in West Maui, HI is <span style = 'color: dodgerblue'>above average</span>
                 for the year")
    ) +
  
    theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(margin = margin(b=10)),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(),
      # legend.background = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.25, 0.8),
      legend.text = element_text(color="black"),
      legend.title = element_text(color="black")
    )

legend <- data.frame(new_date = today - 6, cum_prcp = c(190, 165),
                     color = c('lightgrey', 'black'), size = 5)
w +
  geom_point(data = legend, aes(x = new_date, y = cum_prcp, color = after_scale(c('lightgrey', 'black'))),
             shape = 15, size = 0.1, group = 1) +
  geom_smooth(aes(group = 1), color = "black", size = 0.3,
              method ='gam', formula = y ~ s(x, bs = "cs")) 

ggsave("figures/cumulative_prcp.png", width = 6, height = 5, units = "in")

# guides(
#   fill = guide_legend(
#     title = "Legend Title",
#     override.aes = aes(label = "b", shape=18, size=5))) +
## So it looks like guides() does actually nothing whatsoever. Sick.


## Gonna try some more plotting exercises since I went through all the trouble
## of finding this data

