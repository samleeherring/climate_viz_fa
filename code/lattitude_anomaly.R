library(tidyverse)

bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N",
           "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"


zone_data <- read_csv(url) %>%
  select(year = Year, all_of(bands)) %>%
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>%
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone))

last_year <- zone_data %>%
  filter(year == 2023)

zone_data %>%
  ggplot(aes(x = t_diff, xend = t_diff,
             y = zone_position - 0.25, yend = zone_position + 0.25))+
  geom_segment(color = "white", alpha = 0.5) +
  geom_segment(data = last_year,
               aes(color = t_diff), linewidth = 2, lineend = "round") +
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
    title = "Variation in annual temperature anomaly by \n lattitude (1880-2024",
    subtitle = "Bars for 2023 are colored by the size of the anomaly"
  ) +
  
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey", size = 8),
    panel.background = element_rect(fill = "black"),
    panel.grid.major.x = element_line(color = "white"),
    panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

ggsave("figures/lattitude_anomaly.png", width = 6, height = 5, units = "in")
