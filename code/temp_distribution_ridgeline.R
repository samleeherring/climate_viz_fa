library(tidyverse)
library(R.utils)
library(ncdf4)
library(data.table)
library(lubridate)
library(ggridges)

## Run only once then comment out
# url <- "https://data.giss.nasa.gov/pub/gistemp/gistemp250_GHCNv4.nc.gz"
# download.file(url, destfile = "gistemp250_GHCNv4.nc.gz")
# gunzip("gistemp250_GHCNv4.nc.gz")

nc_data <- nc_open("gistemp250_GHCNv4.nc")
# Save the print(nc) dump to a text file
{
  sink('gistemp250_GHCNv4.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

t_anomaly.array <- ncvar_get(nc_data, "tempanomaly") # store the data in a 3-dimensional array
dim(t_anomaly.array) 

fillvalue <- ncatt_get(nc_data, "tempanomaly", "_FillValue")
t_anomaly.array[t_anomaly.array == fillvalue$value] <- NA

t_data <- as.data.table(t_anomaly.array) %>%
  as_tibble() %>%
  select(longitude = V1, lattitude = V2, time = V3, t_diff = value) %>%
  mutate(longitude = lon[longitude],
         lattitude = lat[lattitude],
         time = t[time] + as.Date("1800-01-01"),
         year = year(time)) %>%
  group_by(year, longitude, lattitude) %>%
  summarise(t_diff = mean(t_diff), .groups = "drop") %>%
  filter(year >= 1950 & year < 2024) %>%
  group_by(year) %>%
  mutate(t_ave = mean(t_diff)) %>%
  view(t_data)

t_data %>%
  ggplot(aes(x = t_diff,
             y = factor(year, levels = seq(2023, 1950, -1)),
             fill = t_ave)) +
  geom_density_ridges(bandwidth = 0.3, scale = 3,
                      linewidth = 0.2, color = "white") +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
                       midpoint = 0, guide = "none") +
  coord_cartesian(xlim=c(-5, 5)) +
  scale_x_continuous(breaks = seq(-4, 4, 2)) +
  scale_y_discrete(breaks = seq(1950, 2023, 10)) +
  
  labs(
    y = NULL,
    x = "Temperature Anomaly (\u00B0C)",
    title = "Land Temperature Anomaly Distribution"
  ) +
  
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_blank()
  )

ggsave("figures/temp_distibution_ridgeline.png", width = 4, height = 6, units = "in")

lat_zones <- c("64N-90N", "44N-64N", "24N-44N", "EQU-24N",
                   "24S-EQU", "44S-24S", "64S-44S", "90S-64S")

anom_ridges <- t_data %>%
  select(year, lattitude, t_diff, t_ave) %>%
  rename(lat = lattitude) %>%
  mutate(arctic = lat > 63,
         tmprt_n = lat > 43 & lat < 64,
         subtrp_n = lat > 23 & lat < 44,
         tropic_n = lat >= 0 & lat < 24,
         tropic_s = lat < 0 & lat > -23,
         subtrp_s = lat < -24 & lat > -43,
         tmprt_s = lat < -44 & lat > -63,
         antarctic = lat < -64) %>%
  arrange(year, -lat) %>%
  # ungroup() %>%
  # select('64N-90N':'64S-90S') %>%
  mutate(arctic = case_when(arctic == TRUE ~ t_diff,
                            arctic == FALSE ~ NA,
                            TRUE ~ NaN),
         tmprt_n = case_when(tmprt_n == TRUE ~ t_diff,
                            tmprt_n == FALSE ~ NA,
                            TRUE ~ NaN),
         subtrp_n = case_when(subtrp_n == TRUE ~ t_diff,
                              subtrp_n == FALSE ~ NA,
                            TRUE ~ NaN),
         tropic_n = case_when(tropic_n == TRUE ~ t_diff,
                              tropic_n == FALSE ~ NA,
                              TRUE ~ NaN),
         tropic_s = case_when(tropic_s == TRUE ~ t_diff,
                              tropic_s == FALSE ~ NA,
                              TRUE ~ NaN),
         subtrp_s = case_when(subtrp_s == TRUE ~ t_diff,
                              subtrp_s == FALSE ~ NA,
                              TRUE ~ NaN),
         tmprt_s = case_when(tmprt_s == TRUE ~ t_diff,
                              tmprt_s == FALSE ~ NA,
                              TRUE ~ NaN),
         antarctic = case_when(antarctic == TRUE ~ t_diff,
                              antarctic == FALSE ~ NA,
                              TRUE ~ NaN)) %>% 
  # select(year, arctic:antarctic) %>%
  pivot_longer(arctic:antarctic, names_to = 'zones', values_to = 't_zones') %>%
  drop_na() %>%
  group_by(year, zones) %>%
  summarise(t_avg = mean(t_zones)) %>%
  arrange(year, match(zones, c('arctic', 'tmprt_n', 'subtrp_n', 'tropic_n',
                               'tropic_s', 'subtrp_s', 'tmprt_s', 'antarctic')))
  # mutate(tropic_s = if_else(isFALSE(tropic_s), NA, t_diff))
  # mutate(tropic_s = as.double(case_match(tropic_s, 'TRUE' ~ t_diff,
  #                               'FALSE' ~ NaN,
  #                               .default = NA)))

plot_dat <- anom_ridges %>%
  uncount(144, .id = "frame") %>%
  group_by(year) %>%
  mutate(zone_pstn = as.integer(as.factor(zones)),
         year = as.factor(year),
         frame = as.integer(year)) %>%
  arrange(year, zones, match(zone_pstn, c('1', '2', '3', '4',
                               '5', '6', '7', '8')))
  

parabs <- plot_dat %>%
  ggplot(aes(x = t_avg, y = factor(zone_pstn, levels = rev(c(2,5,3,7,8,4,6,1))),
             fill = stat(x), group = zone_pstn)) +
  geom_density_ridges_gradient(bandwidth = 0.4, scale = 1,
                               linewidth = 0.2, color = "white") +
  # geom_label(aes(label=year)) +
  scale_fill_viridis_c(option = 'C', guide = 'none', limits = c(-3,4)) +
  # coord_cartesian(xlim=c(-5, 5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-3, 3, 1)) +
  scale_y_discrete(labels = bands, breaks = 1:8) +
  
  labs(
    y = NULL,
    x = "Temperature Anomaly (\u00B0C)",
    title = "Land Temperature Anomaly Distribution (1880-2024)"
    # subtitle = 'Yearly temperature averages over the 8 latitude zones show drastic change\nat the poles and steady trend towards warming closer to the equator'
  ) +
  
  theme(
    text = element_text(color = "white"),
    plot.title.position = 'panel',
    plot.background = element_rect(color = 'black', fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color="white"),
    axis.ticks = element_line(color="white"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(color="white"),
    axis.line.y = element_blank()
  ) +
  transition_manual(frames = frame, cumulative = FALSE)

label_df <- plot_dat %>%
  select(year, frame, zone_pstn) %>%
  ungroup() 

label_text <- data.frame(t_avg = 3.5, zone_pstn = 7,label = label_df$year)

parabs +
  geom_label(data = label_text, aes(x=3.5, y=7, label = label))
## Gonna try and get this label to work later on

animate(parabs)
         
anim_save("figures/latitude_anomaly_ridge_animation_2.gif")

# lat_bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N",
#                "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))
# 
# # ra <- 
# t_data %>%
#   add_count(t_ave, name = 'obsrvtns') %>% 
#   mutate()
#   # aggregate(cbind(count = t_ave) ~ year,     (base R)
#   #           FUN = function(x){NROW(x)}) %>%
#   ggplot(aes(x = t_diff, y = t_ave, group = year)) +
#   geom_density_ridges(bandwidth = 0.3, scale = 3,
#                       linewidth = 0.2, color = "white") +
#   scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred",
#                        midpoint = 0, guide = "none", limits = c(-0.14, 3.7)) +
#   coord_cartesian(xlim=c(-5, 5)) +
#   scale_x_continuous(breaks = seq(-4, 4, 2)) +
#   # scale_y_discrete(breaks = seq(1950, 2023, 10)) +
#   
#   labs(
#     y = NULL,
#     x = "Temperature Anomaly (\u00B0C)",
#     title = "Land Temperature Anomaly Distribution"
#   ) +
#   
#   theme(
#     text = element_text(color = "white"),
#     plot.background = element_rect(fill = "black"),
#     panel.background = element_rect(fill = "black"),
#     panel.grid = element_blank(),
#     axis.text = element_text(color="white"),
#     axis.ticks = element_line(color="white"),
#     axis.ticks.y = element_blank(),
#     axis.line.x = element_line(color="white"),
#     axis.line.y = element_blank()
#   )


min(t_data$t_ave)

nc_close(nc_data)
unlink("gistemp250_GHCNv4.nc")
unlink("gistemp250_GHCNv4.txt")

