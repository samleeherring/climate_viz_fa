## READ.ME
## How this should work: 1 segment from each of the 8 bands should appear at the
## corresponding x-axis value for 2 frames and then disappear, leaving behind a
## smaller transparent segment before moving onto the next year's temp anomaly
## value so that we can see how they trend over time.

library(tidyverse)
library(scales)
library(glue)
library(gganimate)
library(gifski)

## Latitude zones to be mapped
bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N",
           "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

## Pulling all data needed from NASA site
zone_data <- read_csv(url) %>%
  select(year = Year, all_of(bands)) %>%
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>%
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone)) %>%
  mutate(rows = 1:nrow(.))

## This was for an initial test, but I don't need it anymore
# last_year <- zone_data %>%
#   filter(year == 2023)

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
  mutate(year_number = as.integer(year_number))
## Changing the year_number column to integer format in this DF as opposed to 
## below made it so that the fading alpha points actually functioned LET'S GOOO


## Ok for real final data frame stg

send_it <- anim_df %>%
  uncount(144, .id = "frame") %>%
  filter(year_number <= frame) %>%
  arrange(frame, year_number) %>%
  group_by(frame) %>%
  mutate(tail = last(year_number) - year_number,
         # Make the lines solid for 1 frame then alpha 0.3
         shade_alpha = if_else(tail == 0, 1, 0.35),
         # Make the lines fade out over 20 frames
         segment_alpha = pmax(0, (2-tail)/2)) %>%
  ungroup() 
## This was my original row counter, but I had to move it up for the join
# %>% mutate(step_number = 1:nrow(.))

## Shoutout Jon Spring 6851825 from StackOverflow for the uncount & alpha f()

## purveying the goods
send_it %>%
  select(year, zone, t_diff, frame, tail, shade_alpha, segment_alpha) %>% tail
   

a <- send_it %>%
  ggplot(aes(x = t_diff, xend = t_diff,
             y = zone_position - 0.25, yend = zone_position + 0.25,
             group = year_number))+
  geom_segment(aes(color = t_diff, alpha = segment_alpha), linewidth = 2,
               lineend = "round",
               show.legend = FALSE) +
  geom_segment(aes(x = t_diff, xend = t_diff, y = zone_position - 0.25,
                   yend = zone_position + 0.25, group = year_number,
                   color = t_diff, alpha = shade_alpha),
               linewidth = 1.5, show.legend = FALSE, inherit.aes = FALSE) +
  # geom_segment(data = last_year,
  #              aes(color = t_diff), linewidth = 2, lineend = "round") +

  scale_alpha(range = c(0,1)) +
  scale_y_continuous(breaks = 1:8,
                     labels = bands) +
  scale_x_continuous(breaks = seq(-3, 4, 1),
                     labels = seq(-3, 4, 1),
                     limits = c(-3, 4)) +
  scale_color_gradient2(low = "dodgerblue", mid = "white", high = "red",
                        midpoint = 0, limits = c(-2.7, 3.3), guide = "none") +

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
  transition_manual(frame) 

# ggsave("figures/latitude_anomaly.png", width = 6, height = 5, units = "in")

animate(a, width=6, height=6, units="in", res = 300)

anim_save("figures/latitude_anomaly_animation.gif")



parab <- send_it %>%
  ggplot(aes(x = t_diff, y = factor(zone_position, levels = seq(8, 1, by = -1)),
             fill = stat(x))) +
  geom_density_ridges_gradient(bandwidth = 0.3, scale = 1,
                      linewidth = 0.2, color = "white") +
  # geom_label(aes(label=year)) +
  scale_fill_viridis_c(option = 'C', guide = 'none', limits = c(-3,3)) +
  # coord_cartesian(xlim=c(-5, 5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-3, 3, 1)) +
  scale_y_discrete(labels = rev(bands), breaks = 1:8) +
  
  labs(
    y = NULL,
    x = "Temperature Anomaly (\u00B0C)",
    title = "Land Temperature Anomaly Distribution (1880-2024)"
  ) +
  
  theme(
    text = element_text(color = "white"),
    plot.title.position = 'panel',
    plot.background = element_rect(fill = "black", color = 'black'),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_blank()
  ) +
  transition_manual(frames = year_number, cumulative = FALSE) 

label_df_2 <- send_it %>%
  select(year, year_number) %>%
  ungroup() 

label_df_2 %>%
  slice_max(frame)
## checking and comparing frame compliance in data frame

label_text_2 <- data.frame(t_diff = 3.5, zone_position = 4.5,
                         label = label_df_2$year, frame = label_df_2$year_number)

label_text_2 %>%
  ggplot(aes(x=t_diff, y=zone_position, label=label)) +
  geom_text(size = 30) +
  transition_manual(frames = frame, cumulative = FALSE)

parab +
  geom_text(data = label_text_2, aes(x=3.5, y=4.5, label = label, color = 'white',
                                   size = 30), show.legend = FALSE)

animate(parab)

anim_save("figures/latitude_anomaly_ridge_animation_1.gif")


## Here's all of my failed attempts to get the animation how I wanted lol

## gganimate transitions & behaviors, literally never even got to see them work
# shadow_mark(alpha = alpha/3, past = TRUE, future = FALSE) +
# enter_fade() +
# enter_grow() +
# exit_disappear(early = FALSE) +

## subsetting data frames in an attempt to create layers?
# ghosts = a +
#   geom_segment(aes(color = t_diff, alpha = shade_alpha),
#                linewidth = 1) +
#   transition_manual(frames = year_number, cumulative = TRUE)
# 
# monsters = a +
#   geom_segment(aes(color = t_diff),
#                linewidth = 2, lineend = "round",
#                show.legend = FALSE) +
#   transition_manual(frames = year_number, cumulative = FALSE)
  
## Attempting to fuse layers + have them behave differently
# aa <- ghosts +
#   monsters +
#   # transition_manual(frames = year_number, cumulative = TRUE) +
#   transition_layers(layer_length = 1, transition_length = 2,
#                     from_blank = FALSE, keep_layers = c(Inf, 0)) 
#   # shadow_mark(past = TRUE, future = FALSE)
# animate(aa)

# a +
#   geom_segment(aes(x = t_diff, xend = t_diff,
#                    y = zone_position - 0.25, yend = zone_position + 0.25,
#                    color = t_diff), linewidth = 2, lineend = "round",
#                show.legend = FALSE, inherit.aes = FALSE) +
#   # exit_disappear(early = FALSE) + doesn't seem to make a difference
#   shadow_mark(past = TRUE, future = FALSE, exclude_layer = NULL) +
#   transition_manual(frames = year_number, cumulative = FALSE)

# aa = a +
#   geom_segment(aes(color = t_diff), linewidth = 2, lineend = "round",
#                show.legend = FALSE) +
#   exit_disappear()
# 
# animate(aa)


## Welp, we got there eventually lmao