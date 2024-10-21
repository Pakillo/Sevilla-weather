library(readr)
library(dplyr)
library(tidyr)


# station readme
#   https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/readme-by_station.txt

# # data readme
# #   https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
# stations <- tempfile()
# #download.file("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt", stations)


# download the zipped file
temp <- tempfile()
download.file("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_station/SPE00120512.csv.gz",temp)

# unzip and read
ghcn <- read_csv(temp,
                 col_names = c("id", "yearmoda", "element", "value",
                               "mflag", "qflag", "sflag", "obs_time"),
                 col_types = "cccncccc")

# delete the zipped file
unlink(temp)

# subset and format
ghcn.wide <- ghcn %>%
  select(yearmoda, element, value) %>%
  filter(element %in% c("PRCP", "TMAX", "TMIN")) %>%
  separate(col = yearmoda, sep = c(4,6), into = c("year", "month", "day")) %>%
  pivot_wider(names_from = element, values_from = value) %>%
  # convert from tenths of mm to mm
  mutate(PRCP = PRCP/10) |>
         # SNOW = SNOW * 0.00393701,
         # SNWD = SNWD * 0.00393701) %>%
  # convert from tenths of degrees C to degrees
  mutate(TMAX = TMAX / 10,
         TMIN = TMIN / 10) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")),
         day_of_year = case_when(
           lubridate::leap_year(date) & lubridate::yday(date) == 60 ~ NA_real_,
           lubridate::leap_year(date) & lubridate::yday(date) > 60 ~ lubridate::yday(date) -1,
           TRUE ~ lubridate::yday(date))) %>%
  select(year, month, day, date, day_of_year, PRCP, TMAX, TMIN) 

write_csv(ghcn.wide, "data/GHCN_SPE00120512.csv")

