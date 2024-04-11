library(tidyverse)
library(scales)
library(glue)
library(gganimate)
library(gifski)
library(htmlwidgets)

## Installing the gifski package made this so much easier (at all possible)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***")%>%
  select(year = Year, month.abb)%>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

grid_labels <- tibble(
  x = c(-5, -4, 0, 1),
  y = 2030,
  labels = c("+1\u00B0C", "0\u00B0C", "0\u00B0C", "+1\u00B0C")
)

year_labels <- tibble(
  x = -2,
  y = c(seq(1880, 2000, by=20), 2024)
)

t_data %>%
  filter(month == "Apr" | month == "Oct") %>%
  pivot_wider(names_from = "month", values_from = "t_diff") %>%
  mutate(ave_t=(Oct+Apr)/2) %>%
  ggplot(aes(x= -4 - Oct, xend=Apr, y=year, yend=year, color=ave_t))+
  geom_vline(xintercept = c(-5, -4, 0, 1), color="gold")+
  geom_label(data = grid_labels, aes(x=x, y=y, label=labels),
            fontface="bold", fill="black", color="gold",
            label.size=0, size=3,
            inherit.aes = FALSE)+
  geom_segment(size = 0.9, lineend = "round")+
  geom_text(data = year_labels, aes(x=x, y=y, label=y),
            inherit.aes = FALSE, color="#00A86B", fontface="bold", size=3.5)+
  scale_color_gradient2(low="blue", high="red", mid="white", midpoint=0,
                        guide = "none")+
  
  scale_y_continuous(limits = c(NA, 2030), expand = c(0,0))+
  coord_cartesian(clip = "off")+
  
  labs(x=NULL,
       y=NULL,
       title=NULL 
  )+
  
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill="black", color="black"),
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank(),
  )
  
ggsave("figures/climate_tornado.png", width=4.5, height=3.5, units="in")

  