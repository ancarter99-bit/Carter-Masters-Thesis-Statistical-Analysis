library(ncdf4)
library(dplyr)
library(lubridate)

# --- 1. Open NetCDF files ---
nc_abio <- nc_open("temp_sal.nc")
nc_bio  <- nc_open("chl_light.nc")

# --- 2. Read coordinates and time ---
lon  <- ncvar_get(nc_abio, "longitude")
lat  <- ncvar_get(nc_abio, "latitude")
time <- ncvar_get(nc_abio, "time")  # seconds since 1970-01-01

# Convert to Date
dates <- as.Date(as.POSIXct(time, origin="1970-01-01", tz="UTC"))

# --- 3. Read variables ---
sal  <- ncvar_get(nc_abio, "so")       # salinity
temp <- ncvar_get(nc_abio, "thetao")   # temperature
chl  <- ncvar_get(nc_bio,  "chl")      # chlorophyll a
kd   <- ncvar_get(nc_bio,  "kd")       # light attenuation

# --- 4. Check dimensions ---
# Should be [lon, lat, time] for all four variables
dim(sal); dim(temp); dim(chl); dim(kd)

lon_bio <- ncvar_get(nc_bio, "longitude")
lat_bio <- ncvar_get(nc_bio, "latitude")

# Quick look
range(lon_bio)  # min and max longitude
range(lat_bio)  # min and max latitude

# --- 5. Define your sites ---
bolt_box <- list(lon_min = 11.1, lon_max = 11.3,
                 lat_min = 53.9, lat_max = 54.1)

wiek_box <- list(lon_min = 13.0, lon_max = 13.3,
                 lat_min = 54.5, lat_max = 54.65)

# --- 6. Helper function to compute mean over a box ---
mean_over_box <- function(data, lon, lat, box) {
  lon_idx <- which(lon >= box$lon_min & lon <= box$lon_max)
  lat_idx <- which(lat >= box$lat_min & lat <= box$lat_max)
  
  if(length(lon_idx) == 0 | length(lat_idx) == 0){
    stop("No grid cells fall inside the defined box!")
  }
  
  sapply(1:dim(data)[3], function(t) {
    mean(data[lon_idx, lat_idx, t], na.rm = TRUE)
  })
}

# --- 7. Compute daily site averages ---
sal_bolt  <- mean_over_box(sal, lon, lat, bolt_box)
temp_bolt <- mean_over_box(temp, lon, lat, bolt_box)
chl_bolt  <- mean_over_box(chl, lon, lat, bolt_box)
kd_bolt   <- mean_over_box(kd, lon, lat, bolt_box)

sal_wiek  <- mean_over_box(sal, lon, lat, wiek_box)
temp_wiek <- mean_over_box(temp, lon, lat, wiek_box)
chl_wiek  <- mean_over_box(chl, lon, lat, wiek_box)
kd_wiek   <- mean_over_box(kd, lon, lat, wiek_box)

# --- 8. Combine daily values into a data frame ---
daily_df <- data.frame(
  date = dates,
  sal_bolt, temp_bolt, chl_bolt, kd_bolt,
  sal_wiek, temp_wiek, chl_wiek, kd_wiek
)
library(dplyr)
get_env_means <- function(df, start_date, end_date) {
  df %>%
    dplyr::filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    dplyr::summarise(across(-date, ~ mean(.x, na.rm = TRUE)))
}
get_env_means(daily_df, "2025-04-08", "2025-05-08")

# --- 9. Aggregate daily values to monthly means ---
monthly_df <- daily_df %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarise(
    date = as.Date(paste(year, month, "01", sep="-")),
    sal_bolt  = mean(sal_bolt, na.rm=TRUE),
    temp_bolt = mean(temp_bolt, na.rm=TRUE),
    chl_bolt  = mean(chl_bolt, na.rm=TRUE),
    kd_bolt   = mean(kd_bolt, na.rm=TRUE),
    sal_wiek  = mean(sal_wiek, na.rm=TRUE),
    temp_wiek = mean(temp_wiek, na.rm=TRUE),
    chl_wiek  = mean(chl_wiek, na.rm=TRUE),
    kd_wiek   = mean(kd_wiek, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  select(date, sal_bolt, temp_bolt, chl_bolt, kd_bolt,
         sal_wiek, temp_wiek, chl_wiek, kd_wiek)
str(monthly_df)


monthly_df <- daily_df %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarise(
    sal_bolt  = mean(sal_bolt, na.rm = TRUE),
    temp_bolt = mean(temp_bolt, na.rm = TRUE),
    chl_bolt  = mean(chl_bolt, na.rm = TRUE),
    kd_bolt   = mean(kd_bolt, na.rm = TRUE),
    sal_wiek  = mean(sal_wiek, na.rm = TRUE),
    temp_wiek = mean(temp_wiek, na.rm = TRUE),
    chl_wiek  = mean(chl_wiek, na.rm = TRUE),
    kd_wiek   = mean(kd_wiek, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep="-"))) %>%
  select(date, sal_bolt, temp_bolt, chl_bolt, kd_bolt,
         sal_wiek, temp_wiek, chl_wiek, kd_wiek)

# --- 11. Check your final data frame ---
print(monthly_df)
dim(monthly_df)  # should be 9 x 11

# --- 12. Save to CSV ---
write.csv(monthly_df, "env_monthly_bolt_wiek.csv", row.names = FALSE)

# --- 13. Close NetCDFs ---
nc_close(nc_abio)
nc_close(nc_bio)
