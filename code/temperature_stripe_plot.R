library(tidyverse)
library(scales)
library(glue)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***")%>%
  select(year = Year, t_diff= 'J-D')%>%
  drop_na()

glimpse(t_data)

t_data %>%
  ggplot(aes(x = year, y = 1, fill = t_diff))+
  geom_tile(show.legend = FALSE)+
  scale_fill_stepsn(colors=c("#2171b5", "white", "#cb181d"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=12)+
  coord_cartesian(expand=FALSE)+
  scale_x_continuous(breaks=seq(1890, 2023, 30))+
  labs(title=glue("Global temperature change ({min(t_data$year)}-{max(t_data$year)})"))+
  theme_void()+
  theme(
    axis.text.x = element_text(color="white",
                               margin = margin(t=5, b=10, unit="pt")),
    plot.background = element_rect(fill="black"),
    plot.title = element_text(color="white",
                               margin = margin(t=10, b=5, unit="pt"),
                              hjust=0.05)
    
  )

## ended up veering away from original sampled min/max hexcode in line 14 because
## it was too drab and I didn't feel it illustrated the theme of warming properly

## note to self: learn how to alter the vibrance levels of color scales

ggsave("figures/warming_stripes.png", width=8, height=4.5)
