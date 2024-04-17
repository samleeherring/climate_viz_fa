library(tidyverse)
library(scales)
library(glue)
library(gganimate)
library(gifski)

bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N",
           "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"


zone_data <- read_csv(url) %>%
  select(year = Year, all_of(bands)) %>%
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>%
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone)) %>%
  mutate(rows = 1:nrow(.))

last_year <- zone_data %>%
  filter(year == 2023)

## Creating a column to list the years numerically for the animation
current_year <- zone_data %>%
  select(year) %>%
  mutate(year = factor(year),
         year_number = as.numeric(year))

## Returning the year column back into <dbl> so that I can merge DFs
year_nums <- current_year %>%
  select(year, year_number) %>%
  mutate(year = as.numeric(paste(year)))

## For the life of me I cannot get this join/merge to work unless it multiplies
## everything by 8, so I'm calling this one factor_8 and I'll reduce it by adding
## a row count column back up in zone_data
factor_8 <- left_join(zone_data, year_nums, by = intersect('year', 'year'),
                       relationship = 'many-to-many')

## FINAL DATA FRAME 
anim_df <- factor_8[!duplicated(factor_8$rows),] %>%
  arrange(year, zone, t_diff, zone_position, year_number, rows) %>%
  mutate(tail = last(as.integer(year_number) - as.integer(year_number)),
         # Make the lines solid for 1 frame then alpha 0.3
         shade_alpha = if_else(tail == 0, 1, 0.3),
         # Make the lines fade out over 20 frames
         segment_alpha = pmax(0, (20-tail)/20)) %>%
  ungroup()

## This was my original row counter, but I had to move it up for the join
# %>% mutate(step_number = 1:nrow(.))
   

a <- anim_df %>%
  ggplot(aes(x = t_diff, xend = t_diff,
             y = zone_position - 0.25, yend = zone_position + 0.25,
             group = year_number))+
  geom_segment(aes(color = t_diff), linewidth = 2,
               lineend = "round", show.legend = FALSE) +
  # exit_disappear(early = FALSE) +
  scale_alpha(range = c(0,1)) +
  # geom_segment(data = last_year,
  #              aes(color = t_diff), linewidth = 2, lineend = "round") +
  scale_y_continuous(breaks = 1:8,
                     labels = bands) +
  scale_x_continuous(breaks = seq(-3, 4, 1),
                     labels = seq(-3, 4, 1),
                     limits = c(-3, 4)) +
  scale_color_gradient2(low = "darkblue", mid = "white", high = "darkred",
                        midpoint = 0, guide = "none") +

  labs(
    x = "Temperature anomaly (\u00B0C)",
    y = NULL,
    title = "Annual temperature anomaly variation by latitude (1880-2024)"
  ) +

  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(face = "bold", size = 15),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "black"),
    panel.grid.major.x = element_line(color = "white"),
    panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_blank(),
    legend.title = element_blank()
  ) +
transition_manual(frames = year_number, cumulative = FALSE)


# aa <- a +
#   geom_segment(aes(color = t_diff, alpha = shade_alpha),
#                linewidth = 1) +
#   transition_manual(frames = year_number, cumulative = TRUE)


# ggsave("figures/latitude_anomaly.png", width = 6, height = 5, units = "in")

animate(a, width=6, height=6, units="in", res = 300)

anim_save("figures/latitude_anomaly_animation.gif")


