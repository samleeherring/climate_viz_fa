source("code/local_weather.R")


## This plot confirms that there were no temperature measurements taken between
## 1943 and 1957, and then again since 1963, even though the station is still
## updating data today. Strange.

local_weather_scaled %>%
  ggplot(aes(x = year(date), y = tmax)) +
  geom_point() +
  scale_x_continuous(limits = c(1941, 2024)) +
  labs(
    x = "Year",
    y = "Maximum temperature (\u00B0C)",
    title = "Temperature data of West Maui 1942-2024",
    subtitle = "Data from NOAA"
  )
ggsave("figures/maui_temp_data.png", width=7, height=4, units = "in")

## This plot is scaled to show where recordings actually took place

## I turned all NA values to 0 earlier, and it's simply not possible that the 
## maximum temperature anywhere on the island except the peak of Haleakala would
## ever be 0 degrees celsius

local_weather_scaled %>%
  ggplot(aes(x = year(date), y = tmax)) +
  geom_point() +
  scale_x_continuous(limits = c(1941, 1965)) +
  labs(
    x = "Year",
    y = "Maximum temperature (\u00B0C)",
    title = "Temperature data of West Maui 1942-2024",
    subtitle = "Data from NOAA"
  )
ggsave("figures/maui_temp_data_scaled.png", width=7, height=4, units = "in")


## Alright, welp... looks like these datasets from NOAA are a bust for my home,
## but maybe I can check out the data they have on the capitol island of O'ahu.
## There's like 3 US military bases there, so they probably have better data

## If anything, this has been a good exercise in verifying data (silver lining)
