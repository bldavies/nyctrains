# DATA.R
#
# This script generates data/edges.rda, data/nodes.rda and data/routes.rda.
#
# Ben Davies
# November 2019

# Load packages
library(dplyr)
library(readr)
library(stringdist)
library(stringr)

# Import Google Transit data
import_data <- function(x, ...) {
  readr::read_csv(paste0('data-raw/transit/', x, '.txt'), ...)
}
routes_raw <- import_data('routes')
shapes_raw <- import_data('shapes')
stops_raw <- import_data('stops')
stop_times_raw <- import_data('stop_times', col_types = cols(.default = 'c'))
transfers_raw <- import_data('transfers')
trips_raw <- import_data('trips', col_types = cols(.default = 'c'))

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
travel_times <- trips_raw %>%
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
stops <- stops_raw %>%
  filter(location_type == 1) %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  filter(stop_id %in% c(travel_times$from_stop_id, travel_times$to_stop_id))

# Identify stops that appear consecutively on any route and any direction
consecutive_stops <- tibble(
  stop_id.x = c(travel_times$from_stop_id, travel_times$to_stop_id),
  stop_id.y = c(travel_times$to_stop_id, travel_times$from_stop_id)
) %>%
  distinct(stop_id.x, stop_id.y)

# Clean transfer data
transfers <- transfers_raw %>%
  select(stop_id.x = from_stop_id, stop_id.y = to_stop_id) %>%
  # Add missing transfer between South Ferry and Whitehall St
  bind_rows(
    tribble(
      ~stop_id.x, ~stop_id.y,
      '142', 'R27',
      'R27', '142'
    )
  ) %>%
  # Remove out-of-system transfer between Lex. Av/59 St and Lex. Av/63 St
  filter(!(stop_id.x %in% c('629', 'R11') & stop_id.y == 'B08')) %>%
  filter(!(stop_id.x == 'B08' & stop_id.y %in% c('629', 'R11')))

# Disambiguate stops
disambiguated_stops <- tibble(
  stop_id.x = rep(stops$stop_id, nrow(stops)),
  stop_id.y = rep(stops$stop_id, each = nrow(stops))
) %>%
  left_join(stops, by = c('stop_id.x' = 'stop_id')) %>%
  left_join(stops, by = c('stop_id.y' = 'stop_id')) %>%
  # Remove Aqueduct Racetrack and Aqaeduct - N Conduit Av from consideration
  anti_join(consecutive_stops) %>%
  # Keep pairs that share an in-system transfer or close proximity
  left_join(mutate(transfers, has_transfer = T)) %>%
  mutate(has_transfer = !is.na(has_transfer),
         euclidean_dist = sqrt((stop_lat.x - stop_lat.y) ^ 2 + (stop_lon.x - stop_lon.y) ^ 2)) %>%
  filter(has_transfer | euclidean_dist < 0.001) %>%
  # Assign unique ID and name to each set of disambiguated stops
  distinct(stop_id.x, stop_id.y, stop_name.y, stop_lat.y, stop_lon.y) %>%
  group_by(old_stop_id = stop_id.x) %>%
  summarise(stop_id = min(stop_id.y),
            stop_name = paste(sort(unique(stop_name.y)), collapse = '&'),
            stop_lat = mean(stop_lat.y),  # Centroid
            stop_lon = mean(stop_lon.y)) %>%
  ungroup() %>%
  mutate(stop_id = as.numeric(as.factor(stop_id)),
         stop_name = sub('&([^&]*)$', ' and \\1', stop_name),
         stop_name = gsub('&', ', ', stop_name))

# Identify nodes
nodes <- disambiguated_stops %>%
  distinct(stop_id, stop_name, stop_lat, stop_lon) %>%
  arrange(stop_id) %>%
  mutate_at(c('stop_lat', 'stop_lon'), round, 6)  # Preserve original precision

# Identify edges
edges <- travel_times %>%
  left_join(disambiguated_stops, by = c('from_stop_id' = 'old_stop_id')) %>%
  left_join(disambiguated_stops, by = c('to_stop_id' = 'old_stop_id')) %>%
  group_by(source = stop_id.x, target = stop_id.y, route_id, direction_id) %>%
  summarise(travel_time = min(travel_time)) %>%
  ungroup() %>%
  # Remove directed route-specific "shortcuts"
  left_join(nodes, by = c('source' = 'stop_id')) %>%
  left_join(nodes, by = c('target' = 'stop_id')) %>%
  group_by(source, route_id, direction_id) %>%
  slice(which.min((stop_lat.x - stop_lat.y) ^ 2 + (stop_lon.x - stop_lon.y) ^ 2)) %>%
  ungroup() %>%
  select(-direction_id, -ends_with('x'), -ends_with('y')) %>%
  arrange(source, target, route_id) 

# Export data
write_csv(edges, 'data-raw/travel_times.csv')
write_csv(mutate_at(nodes, c('stop_lat', 'stop_lon'), as.character), 'data-raw/stops.csv')
write_csv(routes, 'data-raw/routes.csv')
save(edges, file = 'data/travel_times.rda')
save(nodes, file = 'data/stops.rda')
save(routes, file = 'data/routes.rda')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/data.log')
