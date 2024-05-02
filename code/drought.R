source('code/local_weather.R')

library(slider)
library(ggtext)

drought_data <- alt_local_weather %>%
  select(date, prcp) %>%
  mutate(prcp = if_else(is.na(prcp), 0 , prcp)) %>%
  filter(prcp <70) %>%
  mutate(prcp = prcp/10) %>%
  arrange(date) %>%
  mutate(window_prcp = slide_dbl(prcp, ~sum(.x), .before = 99, .complete = TRUE)) %>%
  drop_na(window_prcp) %>%
  mutate(start = date - 29) %>%
  select(start, end = date, window_prcp) %>%
  mutate(end_month = month(end),
         end_day = day(end),
         end_year = year(end)) %>%
  group_by(end_month, end_day) %>%
  mutate(threshold = quantile(window_prcp, prob = 0.05)) %>%
  ungroup() 
# %>% 
  # filter(window_prcp < threshold) %>% #tail(n=20) aummer-autumn 2023
  # filter(end_year == 2023)
  
drought_line <- drought_data %>%
  select(end_month, end_day, threshold) %>%
  distinct() %>%
  mutate(fake_date = ymd(glue('2020-{end_month}-{end_day}')))

drought_data %>%
  mutate(fake_date = ymd(glue('2020-{end_month}-{end_day}'))) %>%
  select(-start, -end) %>%
  mutate(is_drought_year = end_year == 2012,
         end_year = fct_reorder(factor(end_year), is_drought_year)) %>%
  ggplot(aes(x=fake_date, y=window_prcp, group=end_year, color = is_drought_year,
             size = is_drought_year)) +
  geom_line(show.legend = FALSE) +
  geom_smooth(data = drought_line, aes(x=fake_date, y=threshold),
            inherit.aes = FALSE, color = 'red', size = 1, se = FALSE) +
  scale_color_manual(breaks = c(T,F),
                     values = c('#00ACAB', 'grey')) +
  scale_size_manual(breaks = c(T,F),
                    values = c(1, 0.3))+
  scale_x_date(date_breaks = "2 months", date_labels = "%B", expand = c(0,NA)) +
  
  labs(x = NULL,
       y = "Total precipitation over previous 100 days (cm)",
       title = "The rainy season on Lana'i of <span style='color:#00ACAB'>2012</span> had less precipitation than <span style='color:red'>95% of previous years</span> dating back to 1905",
       subtitle = "Excludes 20 outliers of extremely high precipitation over 60cm") +
  theme(
    plot.title = element_textbox_simple(margin = margin(b=10, t=5)),
    plot.subtitle = element_text(color = 'darkgrey')
  )

ggsave('figures/drought_plot.png', width = 5.5, height = 4.5, units = 'in')


x <- 1:10
slide(x, ~.x, .before = 2)
slide(x, ~.x, .after = 2, .complete = TRUE)

slide(x, ~sum(.x), .before = 2, .complete = TRUE)
slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE)

tibble(x=1:10) %>%
  mutate(total = slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE))


# mutate(one_day_lag = lag(prcp),
#        two_day_lag = lag(prcp, n=2),
#        one_day_lead = lead(prcp),
#        two_day_lead = lead(prcp, n=2))

## Returning to the lag/lead method to make a similar plot on drought

threshold <- 0

drought_by_year <- alt_local_weather %>% 
  select(date, prcp) %>% 
  filter(year(date) > 1953) %>%
  # replace_na(list(prcp = 0)) # replace function needs to use list()
  mutate(prcp = replace_na(prcp, 0)) %>% # same same but different
  filter(prcp > threshold) %>% 
  mutate(prev_date = lag(date, n=1)) %>%
  drop_na() %>% 
  mutate(drought_length = as.numeric(date - prev_date)-1,
         year = year(date)) %>% 
  select(year, length=drought_length)
# filter(year(date) > 1923 & year(date) < 1955) %>% print(n=Inf)
## used this to figure out that there were no records of prcp between the dates  
  
drought_by_year %>%
  filter(year == 1954,
         length <= 35) %>% arrange(length) %>% print(n=Inf)
## just learned about quantiles & quartiles. Formula for upper: n*0.25 
  
## further work on quantiles within sample vector
# samp_vect <- c(0,0,0,0,0,0,0,1,1,1,2,2,3,3,3,4,4,5,5,6)
# samp_vect %>% 
#   quantile(prob = 0.75)
  
  
drought_facets <- drought_by_year %>%
  filter(year != 2024) %>% # current year (incomplete)
  group_by(year) %>%
  summarise(n = n(),
            median = median(length),
            mean = mean(length),
            max = max(length),
            uquartile = quantile(length, prob = 0.75)) 

drought_facets %>%
  ggplot(aes(x=year, y=mean)) +
    geom_line() +
    geom_smooth(se=FALSE, color = 'red') +
    labs(
      x = NULL,
      y = 'Average number of days\nbetween rain events',
      title = "The length of rain drought has seen an overall <span style = 'color:red'>increase</span> during the last 70 years on Lana'i, HI"
    ) +
    scale_x_continuous(breaks = seq(1955, 2023, 10), expand = c(0,NA)) +
    theme_classic() +
    theme(
      plot.title.position = 'panel',
      plot.title = element_textbox_simple(size = 13, margin = margin(b=10))
    )
  ggsave('figures/drought_length.png', width = 5, height = 3, units = 'in')

## verifying what was wrong with the initial faceted plot, 30y of missing data
# drought_facets %>%
#   ggplot(aes(x=year, y=max))+ geom_line()
#   select(year, max) %>%
#   filter(year > 1920 & year < 1956)
# 
# drought_by_year %>%
#   select(year, length) %>%
#   filter(year > 1924 & year < 1956) %>%
#   arrange(year)

drought_labels <- c('max' = 'Max amount of days\nbetween rain events',
                    'mean' ='Avg amount of days\nbetween rain events',
                    'median' = 'Typical amount of days\nbetween rain events',
                    'n' = 'Number of recorded\nrain events by year',
                    'uquartile' = '75th percentile of\ndrought length')  
  
dr_fa <- drought_facets %>%
  pivot_longer(-year) %>%
ggplot(aes(x=year, y=value)) +
  geom_path() +
  geom_smooth(se = FALSE, linewidth = 0.5, span = 0.3, color='#E32636') +
  facet_wrap(~name, ncol = 1, scales = 'free_y', strip.position = 'left', 
             labeller = labeller(name = drought_labels)) +
  scale_x_continuous(breaks = seq(1955, 2023, 10), expand = c(0,0)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Study of the prevalence of drought-like conditions on Lana'i, HI (1954-2023)",
    subtitle = "Data shows that overall rainfall events on the island have begun to trend downward",
    tag = 'Data sourced from\nNCEI.NOAA.gov'
  ) +
  
  theme(
    plot.title = element_textbox_simple(size = 14, margin = margin(b=5)),
    plot.title.position = 'plot',
    plot.subtitle = element_textbox_simple(color = 'darkgrey',
                                           margin = margin(b=5)),
    strip.placement = 'outside',
    # strip.background = element_blank(),
    # panel.background = element_blank(),
    # panel.grid = element_blank(),
    # axis.line = element_line(),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.05, 'in'),
    plot.tag.location = 'panel',
    plot.tag.position = 'bottomright',
    plot.tag = element_text(size = 6, color = 'darkgrey')
    
  )

 ggsave('figures/drought_data.png', width = 6, height = 8, units = 'in')

