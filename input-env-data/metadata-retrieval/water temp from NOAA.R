library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# Function to fetch water temperature data
get_temperature_data <- function(station_id, start_date, end_date,
                                 product = "water_temperature",
                                 units = "metric", time_zone = "gmt") {
  
  base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  
  response <- GET(url = base_url, query = list(
    begin_date = start_date,
    end_date = end_date,
    station = station_id,
    product = product,
    units = units,
    time_zone = time_zone,
    format = "json",
    interval = "6"
  ))
  
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  if ("data" %in% names(data)) {
    return(data$data)
  } else {
    warning("No data found for the specified range.")
    return(NULL)
  }
}


station_id <- "8775237"
date_seq <- seq(as.Date("2007-01-01"), as.Date("2017-12-31"), by = "month")

temperature_data_list <- list()

for (i in seq_along(date_seq)) {
  start <- format(date_seq[i], "%Y%m%d")
  end <- format(date_seq[i] + days(29), "%Y%m%d")
  
  cat("Fetching temperature data from", start, "to", end, "\n")
  
  temp <- tryCatch({
    get_temperature_data(station_id, start, end)
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    NULL
  })
  
  if (!is.null(temp)) {
    temperature_data_list[[length(temperature_data_list) + 1]] <- temp
  }
}


temperature_data <- bind_rows(temperature_data_list)

# Convert temperature to numeric
temperature_data$v <- as.numeric(temperature_data$v)

head(temperature_data)

# Rename for clarity
colnames(temperature_data) <- c("timestamp", "temperature_C", "flag")

# Convert timestamp
temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz = "GMT")

# Save to CSV
write.csv(temperature_data, "input-data/port_aransas_water_temperature_6min_2007_2017.csv", row.names = FALSE)

# Preview
head(temperature_data)


hist(temperature_data$temperature_C)

colnames(temperature_data) <- c("timestamp", "temperature_C", "flag")

# Convert types
temperature_data$temperature_C <- as.numeric(temperature_data$temperature_C)
temperature_data$timestamp <- as.POSIXct(temperature_data$timestamp, tz = "GMT")

# Save to CSV
write.csv(temperature_data, "port_aransas_water_temperature_6min_2007_2017.csv", row.names = FALSE)

# View result
head(temperature_data)

getwd()





library(ggplot2)

ggplot(temperature_data, aes(x = timestamp, y = temperature_C)) +
  geom_line(color = "darkorange", size = 0.3, alpha = 0.6) +
  labs(
    title = "Port Aransas Water Temperature (6-minute intervals)",
    x = "Time", y = "Temperature (°C)"
  ) +
  theme_minimal()






jan_2010 <- temperature_data %>%
  filter(timestamp >= as.POSIXct("2010-01-01") &
           timestamp < as.POSIXct("2010-02-01"))

ggplot(jan_2010, aes(x = timestamp, y = temperature_C)) +
  geom_line(color = "blue") +
  labs(
    title = "Port Aransas Water Temperature (Jan 2010)",
    x = "Time", y = "Temperature (°C)"
  ) +
  theme_minimal()






library(dplyr)
library(lubridate)

temperature_data %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>%
  summarise(
    data_points = n(),
    first_record = min(timestamp),
    last_record = max(timestamp),
    min_temp = min(temperature_C, na.rm = TRUE),
    max_temp = max(temperature_C, na.rm = TRUE)
  )






library(dplyr)
library(lubridate)

weekly_avg <- temperature_data %>%
  mutate(week = floor_date(timestamp, "week")) %>%
  group_by(week) %>%
  summarise(avg_temp = mean(temperature_C, na.rm = TRUE))



library(ggplot2)

ggplot(weekly_avg, aes(x = week, y = avg_temp)) +
  geom_line(color = "darkblue", size = 0.6) +
  labs(title = "Weekly Average Water Temperature at Port Aransas",
       x = "Week", y = "Average Temperature (°C)") +
  theme_minimal()

