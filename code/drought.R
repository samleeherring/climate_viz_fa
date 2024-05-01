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

  # mutate(one_day_lag = lag(prcp),
  #        two_day_lag = lag(prcp, n=2),
  #        one_day_lead = lead(prcp),
  #        two_day_lead = lead(prcp, n=2))

x <- 1:10
slide(x, ~.x, .before = 2)
slide(x, ~.x, .after = 2, .complete = TRUE)

slide(x, ~sum(.x), .before = 2, .complete = TRUE)
slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE)

tibble(x=1:10) %>%
  mutate(total = slide_dbl(x, ~sum(.x), .before = 2, .complete = TRUE))
  