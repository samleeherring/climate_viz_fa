library(tidyverse)
library(scales)
library(glue)

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***")%>%
  select(year = Year, month.abb)%>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

last_dec <- t_diff %>%
  filter(month == "Dec") %>%
  mutate(year = year + 1,
         month = "last_Dec")

next_jan <-t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

## originally had next_jan as +1 and last_dec as -1, but switched them so that 
## the bars would start from the right spot

t_data <- bind_rows(last_dec, t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1,
         this_year = year == 2024)
  
annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)
  
  
t_data %>%
  ggplot(aes(x=month_number, y=t_diff, group=year, color=year,
             size=this_year))+
  geom_line()+
  geom_text(data = annotation,
            aes(x=month_number, y=t_diff, label=year, color=year),
            inherit.aes = FALSE,
            hjust = 0, size =5, nudge_x = 0.15, fontface = "bold")+
  geom_hline(yintercept = 0, color="white")+
  scale_x_continuous(breaks=1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none")+
  scale_color_viridis_c(breaks = seq(1850, 2024, 20),
                        guide = guide_colorbar(frame.colour = "white",
                                               frame.linewidth = 1))+
  coord_cartesian(xlim=c(1,12))+
  theme(
    plot.background = element_rect(fill = "#444444"),
    panel.background = element_rect(fill = "black", color="white", size=1),
    panel.grid = element_blank(),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(fill=NA),
    legend.key.height = unit(55, "pt"),
    legend.text = element_text(color="white"),
    axis.text = element_text(color="white", size=10),
    axis.title = element_text(color="white", size=13),
    axis.ticks = element_line(color="white"),
    axis.ticks.length = unit(-5, "pt")
  )+
  
  labs(
    x = element_blank(),
    y = "Temperature change since pre-industrial times [\u00B0C]",
    title = "Global temperature changes since 1850 by month")

ggsave("figures/temperature_lines.png", width=8, height=4.5)

  