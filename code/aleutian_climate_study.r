## I'm following along with Riffomonas and for this model we're working with snow
## of which there is none in my hometown, so I'm gonna find another station

library(tidyverse)
library(glue)
library(lubridate)


inventory_url <- "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

inventory <- read_table(inventory_url, 
                        col_names = c("station", "lat", "lon", "variable", "start", "end")
)
## Finding the NOAA station closest to my hometown of Lahaina, Hawai'i
# my_lat <- 20.878525 *2*pi/360
# my_lon <- -156.683746 *2*pi/360

## Finding a new NOAA station somewhere interesting that has snow
new_lat <- 53.8726 *2*pi/360
new_lon <- -166.5376 *2*pi/360
## Picking Unalaska in the Aleutians to stick with theme of Pacific archipelagos

## =acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*6371
## formula found online, doing all distance units in kilometers

new_cold_station <- inventory %>%
  mutate(lat_r = lat * 2 * pi/360,
         lon_r = lon * 2 * pi/360,
         #converting to radians
         d = 6371 * acos((sin(lat_r)*sin(new_lat))+cos(lat_r)*cos(new_lat)
                         *cos(new_lon-lon_r))) %>%
  filter(start < 1960 & end > 2020) %>%
  arrange(d) %>%
  distinct(d, .keep_all = TRUE) %>%
  print(n=20)
  ## checking more stations to find the best fit
  # top_n(n = -1, d) %>%
  # distinct(station) %>%
  # pull(station) ## for glue()
## There is only one station on the island & the rest are hundreds of miles away
## Let's hope this one has good data

## Found USC00502587 only 2.5km from Unalaska that goes from 1915 until now

station_daily <- glue("https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/{new_cold_station}.csv.gz")

cold_weather <- read_csv(station_daily,
                          col_names = c("station_id", "date", "element", "value", "m_flag", 
                                        "q_flag", "s_flag", "time")) %>%
  select(date, element, value) %>%
  pivot_wider(names_from = "element", values_from = "value") %>%
  select(date, TMAX, TMIN, PRCP, SNWD) %>%
  mutate(date = ymd(date),
         TMAX = TMAX /10,
         TMIN = TMIN /10,
         PRCP = PRCP/10) %>%
  rename_all(tolower)

## Time to verify some data baby YEAHHHH

tail(cold_weather) ## Looks like it's up to date

## Monthly means for a better glance at trends
cold_months <- cold_weather %>%
  select(date, tmax, tmin, prcp, snwd) %>%
  mutate(month = month(date),
         day = day(date),
         year = year(date)) %>%
  drop_na(tmax, tmin, prcp, snwd) %>%
  group_by(month, year) %>%
  summarise(mean_prcp = mean(prcp),
            mean_tmax = mean(tmax),
            mean_snwd = mean(snwd) /10,
            mean_tmin = mean(tmin),
            .groups = 'drop') %>%
  mutate(date = ym(glue('{year}-{month}'))) %>% 
  arrange(date)
  

# t <- 
  # cold_weather %>%                  checking to see monthly avgs work
  # select(date, prcp, snwd) %>%
  # mutate(month = month(date)) %>%
  # filter(year(date) == 1918,
  #        month(date) == 01) %>% group_by(date) %>% 
  #          summarise(mean_prc = mean(prcp)) 
 
cold_weather %>%
 filter(if_any(snwd, is.na)) ## hmmm 9,000 out of 26,000 are NA values...

cold_weather %>%
  ggplot(aes(x = date, y = snwd, color = snwd)) +
  geom_point() +
  geom_jitter() ## ok so it's looking like there is about ~30y of missing data
ggsave('figures/aleutian_test_plot1.png', width = 5, height = 3, units = 'in')

cold_weather %>%
  select(everything()) %>%
  mutate(year = year(date))
  # drop_na() 16,748 rows containing reel data, zero and nonzero
  # arrange(-snwd) damn they've gotten like 5ft of snow in a day several times
  # filter(snwd == 0) %>% 
  # count(year(date)) %>%
  # arrange(-n) %>% 
  # print(n=20) Looks like the NAs are pretty well dispersed, moreso in the 20s
  # filter(year < 1982 & year > 1956) thought I did sth wrong but nope, no data between these years

## checking significant outliers: only 7 months > 50cm, 16 > 30, 23 > 20, 59 > 10
cold_months %>% 
  filter(year(date) > 1982,
         mean_snwd > 0.1)
## There were 84 months before 1956 with avg snwd over 10mm, and 203 after 1982

cold_months %>% 
  # mutate(snwd = if_else(snwd == 0, NA_real_, snwd)) %>%
  # drop_na() %>%
  filter(mean_snwd <= 20 & mean_snwd >= 0.1) %>%
  ggplot(aes(x = date, y = mean_snwd)) +
  geom_line() +
  geom_smooth(se=TRUE, method = 'gam') +
  scale_x_date(expand=c(0,0))+
  labs(
    x = NULL,
    y = 'Average monthly snowfall (cm)',
    title = "Observations of monthly avg snowfall above 10mm in\nUnalaska, Aleutian Islands",
    subtitle = "Excludes 23 significant outliers of averages above 20cm"
  ) 
ggsave('figures/aleutian_test_plot2.png', width = 5, height = 4, units = 'in')
## We can just barely see the trend of the months that DID HAVE mean_snwd > 0
## have had a drop off from about 6cm to just under 4cm over the last century

## I have come to the conclusion that I must split this data frame
## The 26 years of NA data is making it difficult to see any real trends
## probably start with 1982 on because it has more than double the observations



