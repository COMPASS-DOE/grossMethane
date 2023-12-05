## Pull TEMPEST flood event data for Troll 200s for Moses
##

# 1. Setup ---------------------------------------------------------------------

## Read in packages
require(pacman)
p_load(tidyverse,
       janitor,
       cowplot,
       lubridate)

## Set Dropbox path
#archive_path = "/Users/regi350/Dropbox (Personal)/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive"
archive_path = "field-measurements/Re_ SBB22065/"
baro_path <- "field-measurements/Re_ SBB22065/GCREW Met 2022.05-2022.09/GCREW Met 2022.05-2022.09/"
#baro_path <- "/Users/regi350/Library/CloudStorage/OneDrive-PNNL/Documents/projects/tempest/data_tempest/230303_SERC_DOCK_Rawdata_Loggernet"

## Set daterange of interest as a grepl-friendly format (eg "202205|202206")
grepl_dates = "202204|202205|202206|202207|202208|202209"

## Set ggplot theme
theme_set(theme_bw())

start_date = as.POSIXct("2022-05-01", tz = "UTC") # to match default tz
end_date = as.POSIXct("2022-08-31", tz = "UTC")

# 2. Functions -----------------------------------------------------------------

# read in AquaTroll 200 data the manual way
read_200 <- function(file) {

  read_delim(file,
             skip=1, delim=",", col_names=T) %>%
    slice(3:n()) %>% #remove junk header
    dplyr::mutate(datetime_raw = parsedate::parse_date(TIMESTAMP),
                  Temp = as.numeric(Temperature),
                  Specific_Conductivity = as.numeric(Specific_Conductivity),
                  Salinity = as.numeric(Salinity),
                  Density = as.numeric(Water_Density),
                  Pressure_psi = as.numeric(Pressure),
                  Pressure_mbar = Pressure_psi * 68.948,
                  Instrument = "TROLL200") %>%
    rename(Logger_ID = Statname) %>%
    clean_names() %>%
    dplyr::select(datetime_raw, temp, specific_conductivity, salinity,
                  density, pressure_mbar, instrument, logger_id)
}

## Read in barometric data for correcting water levels
read_baro <- function(filepaths) {
  ## read in column names
  df_names <- read_delim(filepaths, delim = ",", skip = 1, n_max = 0) %>% names()

  ## read in data
  read_delim(filepaths, delim = ",", skip = 4, col_names = df_names) %>%
    dplyr::mutate(datetime_raw = parsedate::parse_date(TIMESTAMP)) %>%
    #dplyr::mutate(datetime = parsedate::parse_date(TIMESTAMP)) %>%
    dplyr::group_by(datetime_raw) %>%
    dplyr::summarize(bp_mbar = mean(Barometric_Pressure_PB110B),
                     rain_int = mean(Rain_cm_Tot))
}

# 3. Read in Barmetric data ----------------------------------------------------

## Create list of files to read in
baro_files <- as_tibble(list.files(path = baro_path, pattern = "GCREW_MET", full.names = T)) %>%
  filter(grepl(grepl_dates, value)) %>%
  rename("filename" = value)

## Create the raw troll dataframe
baro_raw <- purrr::map(baro_files$filename, read_baro) %>%
  bind_rows() %>%
  clean_names()


# 4. Read in Troll 600 data ----------------------------------------------------

## Create list of files to read in
troll_files <- as_tibble(list.files(path = archive_path, pattern = "WaterLevel200", full.names = T)) %>%
  filter(grepl(grepl_dates, value)) %>%
  rename("filename" = value)

## Read in troll inventory (link loggers to plots, and well dimensions)
troll_inventory <- read_csv("field-measurements/Re_ SBB22065/aquatroll_inventory copy.csv") %>%
  clean_names()

## Create the raw troll dataframe
troll_raw <- purrr::map(troll_files$filename, read_200) %>%
  bind_rows() %>%
  clean_names()

## Now, combine troll data with troll metadata, then add barometric pressure,
## then calculate water levels
troll_all_cols <- left_join(troll_raw, troll_inventory %>%
                              select(logger_id, instrument, plot, dist_pressure_sensor_belowground_calc),
                            by = c("logger_id", "instrument")) %>%
  inner_join(baro_raw, by = "datetime_raw") %>%
  mutate(pressure_mbar_cor = pressure_mbar - bp_mbar,
         density_gcm3_cor = ifelse(density > 0.95, density, 1),
         pressurehead_m = (pressure_mbar_cor * 100) / (density_gcm3_cor * 1000 * 9.80665),
         wl_below_surface_m = pressurehead_m - (dist_pressure_sensor_belowground_calc / 100))

## Now make the semi-final dataset
troll_uncorrected_time <- troll_all_cols %>%
  select(datetime_raw, plot, temp, specific_conductivity, salinity, wl_below_surface_m, logger_id) %>%
  filter(datetime_raw >= start_date) %>%
  filter(datetime_raw <= end_date)

ggplot(troll_uncorrected_time, aes(datetime_raw, wl_below_surface_m)) + geom_line() + facet_wrap(~plot)

# 7. Correct timezones ---------------------------------------------------------

## Correct Troll for timezones (UTC and EST here). This is confusing, but data are
## actually in EST (as read from logger), but R assigns UTC automatically. Instead
## of forcing timezones above individually, I'm doing them all at the bottom to
## keep things consistent and consolidated in one place. Also, really stupid, but
## "Etc/GMT+5" is actually 5 hours BEHIND GMT. wtf....

troll <- troll_uncorrected_time %>%
  mutate(datetime_est = force_tz(datetime_raw, tzone = "Etc/GMT+5"),
         datetime_utc = with_tz(datetime_est, tzone = "UTC")) %>%
  select(-datetime_raw) %>%
  mutate(datetime_est = as.character(datetime_est),
         datetime_utc = as.character(datetime_utc)) %>%
  relocate(datetime_est, datetime_utc)


# 7. Export --------------------------------------------------------------------

write_csv(troll, "data_tempest/230303_TEMPEST_Troll200_220601_220701_Moses.csv")
# write_csv(teros, "data_tempest/221028_TEMPEST_Teros_220615_220701_Moses.csv")

max(troll$specific_conductivity)
