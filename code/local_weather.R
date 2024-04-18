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
         
station_daily <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{my_station}.csv.gz"

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
  

local_weather %>%
  ggplot(aes(x = date, y=prcp)) +
  geom_line()  


