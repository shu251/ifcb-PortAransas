install.packages("dataRetrieval")
library(dataRetrieval)



# Check available data at the site
available_data <- whatNWISdata(siteNumber = "07374000")
head(available_data)



# Define your date range
start_date <- "2007-01-01"
end_date <- "2017-12-31"
site_number <- "07374000"

# Get all daily data (dv = daily values)
usgs_data <- readNWISdv(siteNumbers = site_number,
                        parameterCd = "00060",  # Discharge (cfs) – most common
                        startDate = start_date,
                        endDate = end_date)

# Check it
head(usgs_data)




# Get a list of all unique parameter codes available for daily data
unique_params <- unique(available_data$parm_cd[available_data$data_type_cd == "dv"])

# Fetch all available daily parameters
usgs_all_data <- readNWISdv(siteNumbers = site_number,
                            parameterCd = unique_params,
                            startDate = start_date,
                            endDate = end_date)


unique(available_data$parm_cd)


head(usgs_all_data)



library(dplyr)


usgs_clean <- usgs_all_data %>%
  rename(
    site_no = site_no,
    date = Date,
    water_temp_C = X_00010_00003,
    water_temp_code = X_00010_00003_cd,
    discharge_cfs = X_00060_00003,
    discharge_code = X_00060_00003_cd,
    gage_height_ft = X_00065_00003,
    gage_height_code = X_00065_00003_cd,
    conductivity_uScm = X_00095_00003,
    conductivity_code = X_00095_00003_cd,
    dissolved_oxygen_mgL = X_00300_00003,
    dissolved_oxygen_code = X_00300_00003_cd,
    turbidity_ntu = X_00480_00003,
    turbidity_code = X_00480_00003_cd,
    salinity_psu = X_63680_00003,
    salinity_code = X_63680_00003_cd,
    pH = X_99133_00003,
    pH_code = X_99133_00003_cd
  )

write.csv(usgs_clean, "input-data/usgs_07374000_cleaned.csv", row.names = FALSE)

head(usgs_clean)


