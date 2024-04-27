source("code/local_weather.R")

library(ggtext)
library(scales)



## New plot analyzing precipitation trends (bc that data actually exists)
this_year <- year(today())
this_month <- month(today(),label = TRUE, abbr = FALSE)
this_day <- ordinal(day(today()))

last_dec <- local_weather2 %>%
  filter(month(date) == '12') %>%
  mutate(year = year + 1,
         month = "last_Dec")

next_jan <-local_weather2 %>%
  filter(month(date) == '1') %>%
  mutate(year = year - 1,
         month = "next_Jan")

annotation <- local_weather2 %>%
  slice_max(year(date)) %>%
  slice_max(month(date)) %>%
  slice_max(day(date))

local_weather3 <- bind_rows(last_dec, local_weather2, next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1,
         this_year = year == 2024) 

local_weather2 <- local_weather %>%
  select(date, prcp) %>%
  mutate(year = year(date),
         month = as.character(month(date)),
         day = day(date),
         is_this_year = year ==this_year) %>%
  filter(year != 1919) %>%
  group_by(year) %>%
  mutate(cum_prcp = cumsum(prcp)) %>%
  ungroup() %>%
  mutate(new_date = ymd(glue("2024-{month}-{day}"))) 

  
local_weather2 %>%
  ggplot(aes(x = new_date, y = cum_prcp, group = year,
             color = is_this_year, size = is_this_year)) +
    geom_line(aes(x = new_date, y = cum_prcp, group = year,
                color = is_this_year, size = is_this_year), show.legend = FALSE) +
    geom_text(data = annotation,
            aes(x = new_date, y = cum_prcp, label = glue('{year}'),
                color = is_this_year),
            inherit.aes = FALSE, vjust = -0.7, size =4)+
    geom_smooth(aes(group = 1), color = "black", size = 0.3,
              method ='gam', formula = y ~ s(x, bs = "cs")) +
    # geom_text(data = NULL, aes(label = 'Overall average', color = 'black'),
    #           inherit.aes = TRUE) +
    scale_y_continuous(breaks = seq(0, 2200, 300),
                     labels = seq(0, 220, 30),
                     expand = c(0, 0)) +
    scale_color_manual(name = NULL,
                     breaks = c(F, T),
                     values = c("lightgrey", "dodgerblue"),
                     labels = c("Past years", "Current year"),
                     guide = guide_legend(override.aes =list(label = "",
                                                        shape=18, size=10,
                                                        fill = c("lightgrey",
                                                                 "dodgerblue")))
                     ) +
    # guides(
    #   fill = guide_legend(
    #     title = "Legend Title",
    #     override.aes = aes(label = "b", shape=18, size=5))) +
  ## So it looks like guides() does actually nothing whatsoever. Sick.
  
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


ggsave("figures/cumulative_prcp.png", width = 6, height = 5, units = "in")

## Gonna try some more plotting exercises since I went through all the trouble
## of finding this data
