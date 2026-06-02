install.packages("devtools")

devtools::install_github("ropensci/rnoaa")

library(rnoaa)

library(httr)
library(jsonlite)
library(dplyr)

# Define function to get data from NOAA CO-OPS API
get_noaa_data <- function(station_id, start_date, end_date, product = "water_level", datum = "MLLW", units = "metric", time_zone = "gmt") {
  base_url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  
  response <- GET(url = base_url, query = list(
    begin_date = start_date,
    end_date = end_date,
    station = station_id,
    product = product,
    datum = datum,
    units = units,
    time_zone = time_zone,
    format = "json",
    interval = "6"  # 6-minute intervals
  ))
  
  # Parse and return data
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  if ("data" %in% names(data)) {
    return(data$data)
  } else {
    warning("No data found for the specified range.")
    return(NULL)
  }
}

# 🔁 Loop through years & months from Jan 2007 to Dec 2017
all_data <- list()
date_seq <- seq(as.Date("2007-01-01"), as.Date("2017-12-31"), by = "month")

for (i in seq_along(date_seq)) {
  start <- format(date_seq[i], "%Y%m%d")
  end <- format(as.Date(date_seq[i] + lubridate::days(29)), "%Y%m%d")  # Cap to 31-day window max
  
  cat("Fetching:", start, "to", end, "\n")
  
  temp <- tryCatch({
    get_noaa_data("8775237", start, end)
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    NULL
  })
  
  if (!is.null(temp)) {
    all_data[[length(all_data) + 1]] <- temp
  }
}

# Combine and save
final_data <- bind_rows(all_data)

head(final_data)
# Colum headers:
# t = timestamp
# v = MLLW
colnames(final_data) <- c("timestamp", "MLLW", "std_dev", "flag", "quality")
head(final_data)

write.csv(final_data, "input-data/port_aransas_water_levels_6min_2007_2017-4142025.csv", row.names = FALSE)


getwd()


NOAA_water_levels <- read.csv("Desktop/Hu lab IFCB project/NOAA/port_aransas_water_levels_6min_2007_2017.csv")


colnames(NOAA_water_levels) <- c("timestamp", "water_level_m", "std_dev", "flag", "quality")
head(NOAA_water_levels)

final_data$timestamp <- as.POSIXct(final_data$timestamp, tz = "GMT")



final_data$water_level_m <- as.numeric(final_data$water_level_m)

baseline <- mean(final_data$water_level_m, na.rm = TRUE)
print(paste("Calculated Baseline:", round(baseline, 4)))

library(dplyr)

final_data <- final_data %>%
  arrange(timestamp) %>%
  mutate(tide_relative_to_baseline = case_when(
    water_level_m > baseline ~ "Incoming",
    water_level_m < baseline ~ "Outgoing",
    TRUE ~ "At Baseline"
  ))


head(final_data[, c("timestamp", "water_level_m", "tide_relative_to_baseline")], 10)

library(ggplot2)

ggplot(final_data, aes(x = timestamp, y = water_level_m, color = tide_relative_to_baseline)) +
  geom_line(size = 0.6) +
  geom_hline(yintercept = baseline, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Incoming" = "blue", "Outgoing" = "red", "At Baseline" = "gray")) +
  labs(title = "Water Levels Relative to Baseline (Avg Height)",
       x = "Time", y = "Water Level (m)", color = "Tide Direction") +
  theme_minimal()









#Input from Dr. Henrichs

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load your NOAA CSV
# Replace this with your actual file path
noaa_data <- read_csv("Desktop/Hu lab IFCB project/NOAA/port_aransas_water_levels_6min_2007_2017.csv")

# Preview the column names
head(noaa_data)

colnames(noaa_data) <- c("timestamp", "water_level_m", "std_dev", "flag", "quality")


# Make sure the timestamp column is POSIXct
noaa_data <- noaa_data %>%
  mutate(timestamp = as.POSIXct(timestamp, tz = "GMT"))

# Sort by time (just in case) and calculate difference
noaa_data <- noaa_data %>%
  arrange(timestamp) %>%
  mutate(
    change = water_level_m - lag(water_level_m),
    tide_direction = case_when(
      change > 0 ~ "Incoming",
      change < 0 ~ "Outgoing",
      TRUE ~ "No Change"
    )
  )


#111111111
ggplot(noaa_data, aes(x = timestamp, y = water_level_m, color = tide_direction)) +
  geom_line() +
  scale_color_manual(values = c("Incoming" = "blue", "Outgoing" = "red", "No Change" = "gray")) +
  labs(title = "Tide Direction Inferred from Water Level",
       x = "Time", y = "Water Level (m)", color = "Tide Direction") +
  theme_minimal()


#222222222
ggplot(noaa_data, aes(x = timestamp, y = change)) +
  geom_line(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Water Level Change Over Time",
       x = "Time", y = "Change in Water Level (Δm)") +
  theme_minimal()

write.csv(noaa_data, "Desktop/Hu lab IFCB project/NOAA/noaa_water_levels.csv", row.names = FALSE)


test <- read.csv("Desktop/Hu lab IFCB project/NOAA/noaa_water_levels.csv")

