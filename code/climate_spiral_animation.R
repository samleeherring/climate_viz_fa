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

# last_dec <- t_diff %>%
#   filter(month == "Dec") %>%
#   mutate(year = year + 1,
#          month = "last_Dec")
# 
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
  x = 12,
  y = c(1.5, 2.0),
  labels = c("1.5\u00B0C", "2.0\u00B0C")
)

month_labels <- tibble(
  x = 1:12,
  labels = month.abb,
  y = 2.7
)
  
a <- t_data %>%
  ggplot(aes(x=month_number, y=t_diff, group=year, color=year))+
  geom_col(data = month_labels, aes(x=x + 0.5, y=2.4), fill = "black",
           width = 1,
           inherit.aes = FALSE)+
  geom_col(data = month_labels, aes(x=x + 0.5, y=-2), fill = "black",
           width = 1,
           inherit.aes = FALSE)+
  geom_hline(yintercept = c(1.5, 2.0), color="red")+
  geom_label(data = temp_lines, aes(x=x, y=y, label=labels),
             color = "red", fill = "black", label.size = 0, size = 3.5,
             inherit.aes = FALSE)+
  geom_text(data = month_labels, aes(x=x, y=y, label=labels),
          color = "white",
          inherit.aes = FALSE,
          angle = seq(360 - 360/12, 0, length.out =12))+
  geom_label(aes(x =1, y=-1.3, label = year),
             color ="white", fill = "black",
             label.padding = unit(20, "pt"),
             label.size = 0, size = 6)+
  geom_point(data = annotation, aes(x=month_number, y=t_diff, color=year),
             size =2,
             inherit.aes = FALSE)+
  geom_line()+
  scale_x_continuous(breaks=1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 3.0), expand = c(0, -0.7),
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  # scale_size_manual(breaks = c(FALSE, TRUE),
  #                   values = c(0.25, 1), guide = "none")+
  scale_color_viridis_c(breaks = seq(1850, 2024, 20),
                        guide = "none")+
  coord_polar(start = 2*pi/12)+
  
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Global temperature change (1850-2024)"
  )+
  
  theme(
    plot.background = element_rect(fill = "#444444", color = "#444444"),
    panel.background = element_rect(fill = "#444444"),
    panel.grid = element_blank(),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
    axis.text = element_blank(),
    #axis.title = element_text(color="white", size=13),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
  )+
  
  transition_manual(frames = year, cumulative = TRUE)
## changed from transition_reveal and action from along to counter easing
## also changed from step_number to year

  #transition_reveal(along = step_number)

 animate(a, width=4.5, height=4.5, units="in", res=300
#         #nframes = nrow(t_data),
#         #fps = nrow(t_data)/12/60/60
         )

#animate(a)

anim_save("figures/climate_spiral_animation.gif")

## Need to figure out a couple of things:
## 1) make the final point the last frame of animation
## 2) learn to navigate fps and nframes so I don't get errors

  