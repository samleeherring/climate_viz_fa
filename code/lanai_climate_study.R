source('code/local_weather.R')

library(ggtext)
library(scales)

## Exploring alternate weather station a bit further from Lahaina w/ more temp data
## 

lanai_climate <- alt_local_weather_scaled 

lanai_climate %>% 
  ggplot(aes(x=date, y=prcp)) +
  geom_point()

cor.test(lanai_climate$prcp, lanai_climate$tmax)


lanai_labels <- c('avg_temp' = "Avg monthly temperatures\non Lana'i (\u00B0C)",
                   'yr_prcp' = 'Average amount of\nprecipitation by month (cm)')

temp_vs_prcp <- lanai_climate %>% 
  filter(prcp < 130) %>%
  mutate(year = year(date),
         month = as.character(month(date)),
         day = day(date)) %>%
  group_by(year, month) %>%
  summarise(avg_temp = (mean(tmax)+mean(tmin))/2,
            yr_prcp = mean(prcp),
            .groups = 'drop') %>%
  pivot_longer(cols = c(avg_temp, yr_prcp)) %>%
  mutate(name = factor(name, levels = c('avg_temp', 'yr_prcp')))
  
temp_vs_prcp %>%
ggplot(aes(x = year, y = value)) +
  geom_path(aes(color=name), show.legend = FALSE) +
  geom_smooth(se=TRUE, aes(color=name), show.legend = FALSE) +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'left', 
             labeller = labeller(name = lanai_labels)) +
  scale_y_continuous(expand = c(0,NA)) + 
  # scale_x_date(date_breaks = '10 years',
  #              date_labels = '%Y', expand = c(0,NA)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10),expand = c(0,NA)) +
  scale_color_manual(breaks = waiver(),
                     values = c('#E32636', '#00ACAB')) +
  coord_cartesian(clip = 'off') +
  
  labs(
    x = NULL,
    y = NULL,
    title = glue("Average <span style = 'color: #E32636'>temperature</span>
                 vs average <span style = 'color: #00ACAB'>precipitation</span>
                 by month on Lana'i, HI (1954 - 2024)"),
    subtitle = "Excludes 3 significant outliers of average precipitation over 13cm",
    tag = 'Data from NCEI NOAA'
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5)),
    plot.subtitle = element_text(color = 'darkgrey', size = 10),
    strip.placement = 'outside',
    strip.background = element_rect(fill = 'lightgrey'),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag.location = 'panel',
    plot.tag.position = 'right',
    plot.tag = element_text(size = 6, color = 'darkgrey')
    
  )

ggsave('figures/lanai_temp_vs_prcp.png', width = 6, height = 5, units = 'in')


#dataframe for finding the avg prcp during the wet season (Nov-Mar)
wet_szn <- lanai_climate %>%
  select(date, prcp) %>%
  mutate(month = month(date)) %>%
  arrange(month) %>%
  filter(month > 10 | month < 4,
         prcp > 0) ## mean(wet_szn$prcp) avg = 6.9mm

#dataframe for the high avg of wet season prcp
wet_szn_high <- wet_szn %>%
  select(date, prcp) %>%
  filter(prcp >= 6.9)

#dataframe for finding the avg prcp during the dry season (Apr-Oct)
dry_szn <- lanai_climate %>%
  select(date, prcp) %>%
  mutate(month = month(date)) %>%
  arrange(month) %>%
  filter(month > 3 & month < 11,
         prcp > 0) ## avg = 6.3mm
## Daily prcp rate plot
lanai_climate %>% 
  filter(prcp < 120) %>%
  ggplot(aes(x = date, y = prcp)) +
  geom_line(color = "darkgreen") +
  geom_smooth(data = wet_szn_high,
              aes(color = "cyan")) +
  geom_smooth(data = wet_szn,
              aes(color = "#77C3E3")) +
  geom_smooth(data = dry_szn,
              aes(color = "pink")) +
  scale_x_date(expand=c(-0,0))+
  scale_y_continuous(breaks=seq(20, 120, 20), limits=c(0, 130), expand=c(-0,NA)) +
  scale_color_manual(name = NULL,
                     values=c("#77C3E3", "cyan", "pink"),
                     labels=c("Wet season avg", "High wet season avg", 
                              "Dry season avg")) +
  
  labs(
    x = NULL,
    y = "Precipitation (mm)",
    title = "Daily precipitation rates for Lana'i, HI (1955 - 2024)",
    subtitle = "Excludes 1 extreme outliers of over 150mm of daily rainfall",
    tag = "Data from\nNCEI.NOAA.gov"
  ) +
  
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "#3B3B3B", color = "black"),
    plot.title = element_text(face = "bold", size = 15),
    plot.margin = margin(0.5,1,0.5,0.5, "cm"),
    panel.background = element_rect(fill = "#FFFFF0"),
    panel.grid.major.y = element_line(color = "darkgrey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(color="white"),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.69, 0.88),
    legend.text = element_text(color="black"),
    plot.tag.location = 'panel',
    plot.tag.position = 'topright',
    plot.tag = element_text(size = 6, color = 'darkgrey')
  )

ggsave('figures/lanai_daily_prcp.png', width = 6, height = 4, units = 'in')
