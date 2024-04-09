library(tidyverse)
library(scales)
library(glue)
library(gganimate)
library(gifski)

## Installing the gifski package made this so much easier (at all possible)

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***")%>%
  select(year = Year, month.abb)%>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

next_jan <-t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

## originally had next_jan as +1 and last_dec as -1, but switched them so that 
## the bars would start from the right spot

t_data <- bind_rows(t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  filter(year != 1879) %>%
  mutate(step_number = 1:nrow(.))

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 1,
  y = c(1, 0, -1),
  labels = c("+1\u00B0C", "0\u00B0C", "-1\u00B0C")
)

month_labels <- tibble(
  x = 1:12,
  labels = toupper(month.abb),
  y = 1.5
)

gridlines <- tibble(
  x = c(1.3, 1.3, 1.5),
  xend = c(12.7, 12.7, 12.5),
  y = c(1, 0, -1),
  yend = y
)

a <- t_data %>%
  ggplot(aes(x=month_number, y=t_diff, group=year, color=t_diff))+
  geom_label(aes(x =1, y=-1.7, label = year),
             fill = "black", label.size = 0, size = 6)+
  geom_point(data = annotation, aes(x=month_number, y=t_diff, color=year),
             size =2,
             inherit.aes = FALSE)+
  geom_line()+
  geom_segment(data = gridlines, aes(x=x, y=y, xend=xend, yend=yend),
               color=c("yellow", "green", "yellow"), size=0.8,
               inherit.aes = FALSE)+
  geom_text(data = temp_lines, aes(x=x, y=y, label=labels),
             color = c("yellow", "green", "yellow"), size=2.3, fontface="bold",
             inherit.aes = FALSE)+
  geom_text(data = month_labels, aes(x=x, y=y, label=labels),
            color = "yellow",
            inherit.aes = FALSE)+
  
  scale_y_continuous(limits = c(-2.0, 1.5),
                     expand = c(0, -0.3))+
  scale_color_gradient2(
    low="blue", mid="white", high="red", midpoint=0,
    guide="none", limits=c(-1, 1.5)
    )+
  ## For some reason, scale_color_gradient2() is only taking the mid color argument
  ## Figured it out (on my own too, wowww) have to set closer equidistant limits
  
  coord_polar(start = 0)+
  
  labs(
    x = element_blank(),
    y = element_blank(),
    title = NULL
  )+
  
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", size=1),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    axis.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank()
  )+
  transition_manual(frames = year, cumulative = TRUE)

#ggsave("figures/nasa_climate_spiral.png")
  
## changed from transition_reveal and action from 'along' to counter easing
## also changed from step_number to year


animate(a
        #, width=4.155, height=4.5, unit="in", res=300,
        #nframes = nrow(t_data),
        #fps = nrow(t_data)/12/60/60
)

anim_save("figures/nasa_climate_spiral_animation.gif")



