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
## https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USC00502587.csv.gz

station_daily <- 'https://www.ncei.noaa.gov/pub/data/ghcn/daily/by_station/USC00502587.csv.gz'

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
 filter(if_any(snwd, is.na)) %>% filter(snwd != 0) ## hmmm 9,000 out of 26,000 are NA values...

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
cold_weather %>% 
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


cold_40 <- cold_months %>%
  filter(year > 1982) %>%
  filter(mean_snwd <= 20)  
## New DF with only monthly means from 1983 onward
## This shows that we're missing about 5 months of data from within the range
## minus the 13 filtered out
cold_40 %>%
count(month) ## looks like the missing months are well dispersed, so we good

snow_labels <- c('prob_snwd' = 'Probability of snowfall',
                   'mean_snwd' = 'Average amount of\nsnowfall by day (cm)',
                   'mean_event' = 'Average amount of\nsnowfall by event (cm)')

today_month <- month(today())
today_day <- day(today())
today_date <- ymd(glue("2024-{today_month}-{today_day}"))
today_pretty <- today_date %>%
  format.Date(format = '%B %dth %Y')

c %>%  filter(if_any(value, is.na)) %>% tail()

c %>% filter(month > 5 & month < 11)

cold_weather %>% filter(month(date) > 5 & month(date) < 11)
mutate_all(~replace(., is.na(.), 0))

c <- cold_weather %>%
  select(date, snwd) %>%
  filter(year(date) > 1982) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  drop_na(snwd) %>% 
  group_by(month, day) %>%
  summarise(prob_snwd = (mean(snwd > 0)*100)/10,
            mean_snwd = (mean(snwd))/10,
            mean_event = (mean(snwd[snwd > 0]))/10,
            .groups = 'drop') %>%
  mutate(date = ymd(glue('2024-{month}-{day}'))) %>%
  pivot_longer(cols = c(prob_snwd, mean_snwd, mean_event)) %>%
  mutate(name = factor(name, levels = c('prob_snwd', 'mean_snwd', 'mean_event'))) %>%

ggplot(aes(x = date, y = value, label = today_date)) +
  geom_line() +
  # geom_hline(yintercept = 0) +
  geom_vline(color = 'red', xintercept = today_date, linewidth = 1) +
  geom_smooth(se=FALSE) +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'left', 
             labeller = labeller(name = snow_labels)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0,0)) + 
  scale_x_date(date_breaks = '2 months',
               date_labels = '%B', expand = c(0,0)) +
  coord_cartesian(clip = 'off') +
  
  labs(
    x = NULL,
    y = NULL,
    title = "Probability of avg snowfall in Unalaska, Aleutian Islands (1983 - 2024)",
    subtitle = "Excludes 13 significant outliers of averages above 20cm"
  ) +
  
  theme(
    plot.title = element_text(),
    plot.subtitle = element_text(color = 'grey'),
    strip.placement = 'outside',
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in')
    
  ) 
ann_txt <- data.frame(date = (today_date + 30), value = 7,label = today_pretty,
                       name = 'mean_snwd')
c+  
  geom_text(data = ann_txt,label = ann_txt$label, color = 'blue', size = 3)

ggsave('figures/snow_prob_amount.png', width = 6, height = 5, units = 'in')
## Faceted probability and averages plots don't really work when there is no snow

prcp_snow <- cold_weather %>%
  filter(year(date) > 1982) %>%
  drop_na() %>%
  filter(snwd > 0) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(prcp = sum(prcp),
            snow = sum(snwd)) %>%
  filter(year != 1983 & year != 2024)
  
prcp_snow %>%
  pivot_longer(-year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1, scales = 'free_y')

prcp_snow %>%
  ggplot(aes(x=prcp, y=snow, color = year)) +
  geom_point()
  
# Pearson's product-moment correlation
cor.test(prcp_snow$prcp, prcp_snow$snow)
# t = 6.3493, df = 38, p-value = 1.891e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5228169 0.8410178
# sample estimates:
#   cor:
# 0.7174764 

prcp_snow_daily <- cold_weather %>%
  filter(year(date) > 1982) %>%
  drop_na() %>%
  filter(snwd > 0 & tmax <= 0) %>%
  filter(prcp < 60) %>%
  mutate(year = year(date),
         snow = snwd/10) %>%
  filter(year != 1983 & year != 2024) 

snow_model <- lm(snwd~tmax*prcp+0, data = prcp_snow_daily)
summary(snow_model)

predict(snow_model, prcp_snow_daily)

prcp_snow_daily %>% 
  mutate(predicted = predict(snow_model, prcp_snow_daily)) %>%
  ggplot(aes(x=prcp, y=snwd)) +
  geom_point(color = 'lightgrey') +
  geom_smooth(aes(color = 'simple'), formula = 'y~x+0', method = 'lm', se = FALSE) +
  # geom_abline(intercept = 0, slope = 10, size = 1) +
  geom_segment(x=0, y=0,
               xend = max(prcp_snow_daily$prcp),
               yend = (max(prcp_snow_daily$prcp))*10, size = 0.5,
               aes(color = 'rule_of_thumb')) +
  geom_smooth(aes(y=predicted, color = 'advanced'), se = FALSE)+
  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1500), breaks = seq(0, 1500, 250)) +
  scale_color_manual(name = NULL,
                     breaks = c('rule_of_thumb', 'simple', 'advanced'),
                     labels = c('10:1 rule of thumb',
                                'Simple model',
                                'Advanced model'),
                     values = c('black', 'dodgerblue', 'red')) +
  
  labs(
    x = 'Total daily precipitation (mm)',
    y = 'Total daily snowfall (mm)',
    title = 'Model of ratio between precipitation & snowfall in the\nAleutian Islands (1984 - 2023)'
  ) +
  theme_classic()

ggsave('figures/aleutian_model_snow_ratio.png', width = 6, height = 4, units = 'in')


# Pearson's product-moment correlation
cor.test(prcp_snow_daily$prcp, prcp_snow_daily$snwd)
# t = 4.2149, df = 546, p-value = 2.924e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.0951738 0.2574486
# sample estimates:
#   cor 
# 0.1775176 


snow_data <- cold_weather %>%
  select(date, snwd) %>%
  drop_na(snwd) %>%
  mutate(cal_year = year(date),
         month = month(date),
         snow_year = if_else(date < ymd(glue('{cal_year}-07-01')),
                             cal_year - 1,
                             cal_year)) %>% 
  select(month, snow_year, snwd) %>%
  filter(snow_year != 1917)

snow_data %>%
  select(snow_year, snwd) %>%
  filter(snow_year > 1952 & snow_year < 1980) %>% tail(n=20)
## BRUH. What is with these 30y gaps in like every data set?? Checking parent DF

cold_weather %>%
  select(date, snwd) %>%
  filter(year(date) > 1952 & year(date) < 1980) %>% tail(n=20)
## Ugh yup, wasn't a calculation error, there's just no data from the 50s-80s

snow_data %>%
  group_by(snow_year) %>%
  summarise(total_snow = sum(snwd)) %>%
  ggplot(aes(x = snow_year, y = total_snow)) +
  geom_line()


dummy_df <- crossing(snow_year = 1918:2023,
                     month = 1:12) %>%
  mutate(dummy = 0)

snow_data %>% 
  right_join(., dummy_df, by = c('snow_year', 'month')) %>%
  mutate(snow = if_else(is.na(snwd), dummy, snwd)) %>%
  group_by(snow_year, month) %>%
  summarise(sum_snow = sum(snow), .groups = 'drop') %>%
  mutate(month = factor(month, levels = c(8:12, 1:7)),
         is_this_year = 2023 == snow_year) %>% 
  filter(sum_snow < 15000) %>% ## removes 3 extreme outlier months
  ggplot(aes(x=month, y=sum_snow, group = snow_year, color = is_this_year,
             size = is_this_year)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c(T, F),
                     values = c("dodgerblue", "grey")) +
  scale_size_manual(name = NULL,
                    breaks = c(T, F),
                    values = c(1.5, 0.6)) +
  scale_x_discrete(breaks = c(9, 11, 1, 3, 5),
                   labels = months.abb[c(9, 11, 1, 3, 5)],
                   expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 13000, 1000))
theme(
  panel.background = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line()
)

## aaaaand the data has either begun updating or been deleted from the source,
## because every cold_weather$snwd frame is either NA or 0 (2024/29/04)

