# DATA.R
#
# This script generates data/routes.rda, data/stops.rda and data/travel_times.rda.
#
# Ben Davies
# November 2019

# Load packages
library(dplyr)
library(readr)
library(stringr)

# Import Google Transit data
import_data <- function(x, ...) {
  readr::read_csv(paste0('data-raw/transit/', x, '.txt'), ...)
}
routes_raw <- import_data('routes')
stops_raw <- import_data('stops')
stop_times_raw <- import_data('stop_times', col_types = cols(.default = 'c'))
trips_raw <- import_data('trips', col_types = cols(.default = 'c'))

# Download station and complex lists
download_data <- function(x) {
  readr::read_csv(paste0('http://web.mta.info/developers/data/nyct/subway/', x, '.csv'))
}
stations_raw <- download_data('Stations')
complexes_raw <- download_data('StationComplexes')

# Identify routes
routes <- trips_raw %>%
  left_join(stop_times_raw) %>%
  filter(stop_sequence == '1') %>%
  count(route_id) %>%
  left_join(routes_raw) %>%
  select(route_id, route_name = route_long_name, route_color) %>%
  mutate(route_color = ifelse(!is.na(route_color), paste0('#', route_color), NA))

# Define function for converting HH:MM:SS strings to second counts
hms2sec <- function(x) {
  res <- vector('double', length(x))
  for (i in seq_along(x)) {
    hms <- stringr::str_split(x[i], ':')[[1]]
    res[i] <- crossprod(as.numeric(hms), c(3600, 60, 1))
  }
  res
}

# Compute route-specific minimum observed travel times between consecutive stops
observed_travel_times <- trips_raw %>%
  left_join(stop_times_raw) %>%
  mutate(stop_id = gsub('(N|S)$', '', stop_id)) %>%  # Ignore orientation
  mutate_at(c('direction_id', 'stop_sequence'), as.numeric) %>%
  mutate_at(c('arrival_time', 'departure_time'), hms2sec) %>%
  group_by(trip_id) %>%
  arrange(stop_sequence) %>%
  mutate(from_stop_id = lag(stop_id),
         travel_time = arrival_time - lag(departure_time)) %>%
  filter(!is.na(from_stop_id)) %>%  # Remove first stop
  group_by(from_stop_id, to_stop_id = stop_id, route_id, direction_id) %>%
  summarise(travel_time = min(travel_time)) %>%
  ungroup()

# Identify observed stops
observed_stops <- stops_raw %>%
  filter(location_type == 1) %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  filter(stop_id %in% c(observed_travel_times$from_stop_id, observed_travel_times$to_stop_id))

# Disambiguate stops
disambiguated_stops <- stations_raw %>%
  left_join(complexes_raw) %>%
  `colnames<-`(gsub(' ', '_', tolower(colnames(.)))) %>%
  select(gtfs_stop_id, station_id, stop_name, borough, complex_id, complex_name) %>%
  mutate(complex_name = ifelse(!is.na(complex_name), complex_name, stop_name)) %>%
  select(old_stop_id = gtfs_stop_id, stop_id = complex_id, stop_name = complex_name, borough) %>%
  arrange(old_stop_id) %>%
  left_join(observed_stops, by = c('old_stop_id' = 'stop_id')) %>%
  distinct() %>%
  group_by(stop_id) %>%
  mutate(stop_lat = mean(stop_lat),
         stop_lon = mean(stop_lon)) %>%
  ungroup() %>%
  mutate(stop_borough = case_when(borough == 'Bx' ~ 'Bronx',
                                  borough == 'Bk' ~ 'Brooklyn',
                                  borough == 'M' ~ 'Manhattan',
                                  borough == 'SI' ~ 'Staten Island',
                                  borough == 'Q' ~ 'Queens')) %>%
  select(old_stop_id, stop_id, stop_name = stop_name.x, stop_lat, stop_lon, stop_borough)

# Prepare stops for export
stops <- disambiguated_stops %>%
  distinct(stop_id, stop_name, stop_lat, stop_lon, stop_borough) %>%
  arrange(stop_id) %>%
  mutate_at(c('stop_lat', 'stop_lon'), round, 6)  # Preserve original precision

# Prepare travel times for export
travel_times <- observed_travel_times %>%
  left_join(disambiguated_stops, by = c('from_stop_id' = 'old_stop_id')) %>%
  left_join(disambiguated_stops, by = c('to_stop_id' = 'old_stop_id')) %>%
  group_by(source = stop_id.x, target = stop_id.y, route_id, direction_id) %>%
  summarise(travel_time = min(travel_time)) %>%
  ungroup() %>%
  # Remove directed route-specific "shortcuts"
  left_join(stops, by = c('source' = 'stop_id')) %>%
  left_join(stops, by = c('target' = 'stop_id')) %>%
  group_by(source, route_id, direction_id) %>%
  slice(which.min((stop_lat.x - stop_lat.y) ^ 2 + (stop_lon.x - stop_lon.y) ^ 2)) %>%
  ungroup() %>%
  select(-direction_id, -ends_with('x'), -ends_with('y')) %>%
  arrange(source, target, route_id) 

# Export data
write_csv(routes, 'data-raw/routes.csv')
write_csv(mutate_at(stops, c('stop_lat', 'stop_lon'), as.character), 'data-raw/stops.csv')
write_csv(travel_times, 'data-raw/travel_times.csv')
save(routes, file = 'data/routes.rda', version = 2)
save(stops, file = 'data/stops.rda', version = 2)
save(travel_times, file = 'data/travel_times.rda', version = 2)

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/data.log')
