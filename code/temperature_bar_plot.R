library(tidyverse)
library(scales)
library(glue)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip=1, na="***")%>%
  select(year = Year, t_diff= 'J-D')%>%
  drop_na()

annotation <- t_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x=year + c(-5.5, 5.5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall = 1)

t_data %>%
  ggplot(aes(x=year, y=t_diff, fill=t_diff))+
  geom_col(aes(x=year, y=t_diff, fill=t_diff))+
  geom_text(data = annotation, aes(x=x, label = year), color="white")+
  geom_text(x=1880, y=1, hjust=0,
            label=glue("Global temperatures have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"),
            color="white", size = 4.3)+
  #scale_fill_gradient2(low="darkblue", mid="white", high="darkred",
  #                     midpoint=0, limits =c(-0.5, 1.5))+
  #scale_fill_gradientn(colors=c("darkblue", "white", "darkred"),
  #                     values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                     limits = c(min(t_data$t_diff), max(t_data$t_diff)))+
  scale_fill_stepsn(colors=c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks=9, guide = guide_colorbar(frame.colour = "white",
                                                       frame.linewidth = 0.3))+
  ## decided to add in a legend for scale and reposition things better
  theme_void()+
  theme(
    plot.background = element_rect(fill="black"),
    legend.text = element_text(color="white"),
    legend.position = "inside",
    legend.position.inside = c(0.12, 0.6)
  )

## played around with scale_fill functions in lines 24-32 to find which best mimicked
## the example from showyourstripes.info

## lack of scale in this bar chart makes it kind of vague and unprofessional,
## but the objective was just to copy something that was already there, so w/e

ggsave("figures/temperature_bar_plot.png", width=7, height=4, limitsize = FALSE)
