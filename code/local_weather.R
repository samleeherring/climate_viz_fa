library(tidyverse)
library(glue)
library(lubridate)


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url,
           col_names = c("station", "lat", "lon", "variable", "start", "end")
           )
## Finding the NOAA station closest to my hometown of Lahaina, Hawai'i
my_lat <- 20.878525 *2*pi/360
my_lon <- -156.683746 *2*pi/360

## =acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*6371
## formula found online, doing all distance units in kilometers

my_station <- inventory %>%
  mutate(lat_r = lat * 2 * pi/360,
         lon_r = lon * 2 * pi/360,
         #converting to radians
         d = 6371 * acos((sin(lat_r)*sin(my_lat))+cos(lat_r)*cos(my_lat)
                  *cos(my_lon-lon_r))) %>%
  filter(start < 1960 & end > 2020) %>%
  # arrange(d) %>%
  # print(n=20)
  ## checking more stations to find the best fit
  top_n(n = -1, d) %>%
  distinct(station) %>%
  pull(station)
  
## USC00515715 seems to have only recorded temp in the 60s? PRCP is up to date though
## same with USC00512450, USC00519376, USC00519275, USC00513045, and USC00519315
## so that's... a little concerning... guess we're working with precipitation then

## Found this station only 9.63km from Lahaina that goes from 1919 until now
         
station_daily <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USC00515715.csv.gz"

local_weather <- read_csv(station_daily,
                      col_names = c("station_id", "date", "element", "value", "m_flag", 
                                    "q_flag", "s_flag", "time")) %>%
  select(date, element, value) %>%
  pivot_wider(names_from = "element", values_from = "value",
              values_fill = 0) %>%
  select(date, TMAX, TMIN, PRCP, SNWD) %>%
  mutate(date = ymd(date),
         TMAX = TMAX /10,
         TMIN = TMIN /10,
         PRCP = PRCP/10) %>%
  rename_all(tolower)

#dataframe for finding the avg prcp during the wet season (Nov-Mar)
wet_season <- local_weather %>%
  select(date, prcp) %>%
  mutate(month = month(date)) %>%
  arrange(month) %>%
  filter(month > 10 | month < 4,
         prcp > 0) ## avg = 11.8mm

#dataframe for the high avg of wet season prcp
big_wet <- wet_season %>%
  select(date, prcp) %>%
  filter(prcp >= 11.8)

#dataframe for finding the avg prcp during the dry season (Apr-Oct)
dry_season <- local_weather %>%
  select(date, prcp) %>%
  mutate(month = month(date)) %>%
  arrange(month) %>%
  filter(month > 3 & month < 11,
         prcp > 0) ## avg = 6.3mm
  

local_weather %>%
  ggplot(aes(x = date, y = prcp)) +
  geom_line(color = "darkgreen") +
  geom_smooth(data = wet_season,
              aes(color = "lightblue")) +
  geom_smooth(data = dry_season,
              aes(color = "pink")) +
  geom_smooth(data = big_wet,
              aes(color = "#77C3E3")) +
  scale_x_date(expand=c(-0,0))+
  scale_y_continuous(breaks=seq(20, 160, 20), limits=c(0, 160), expand=c(-0,0)) +
  scale_color_manual(name = NULL,
                     values=c("#77C3E3", "lightblue", "pink"),
                     labels=c("High avg", "Wet season avg",
                              "Dry season avg")) +
  
  labs(
    x = NULL,
    y = "Precipitation (mm)",
    title = "Daily precipitation rates for Lahaina, HI (1919 - 2024)",
    subtitle = "Excludes 11 extreme outliers of over 150mm of daily rainfall"
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
    legend.position.inside = c(0.69, 0.89),
    legend.text = element_text(color="black")
  )

## Yeah not really sure what happened between 2000-2005, but I do remember a bunch
## of government strikes at the time... will investigate

ggsave("figures/local_weather.png", width=7, height=4, units = "in")


