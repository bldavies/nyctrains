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
library(tidyr)

# Import Google Transit data
import_data <- function(x, ...) {
  readr::read_csv(paste0('data-raw/transit/', x, '.txt'), ...)
}
calendar_raw <- import_data('calendar')
calendar_dates_raw <- import_data('calendar_dates')
routes_raw <- import_data('routes')
shapes_raw <- import_data('shapes')
stops_raw <- import_data('stops')
stop_times_raw <- import_data('stop_times', col_types = cols(.default = 'c'))
transfers_raw <- import_data('transfers')
trips_raw <- import_data('trips', col_types = cols(.default = 'c'))

# Count scheduled service occurrences during first week of November 2019
service_dates <- vector('list', nrow(calendar_raw))
for (i in seq_len(nrow(calendar_raw))) {
  service_dates[[i]] <- tibble(
    date = seq(calendar_raw$start_date[i], calendar_raw$end_date[i])
  ) %>%
    mutate(service_id = calendar_raw$service_id[i])
}
service_counts <- service_dates %>%
  bind_rows() %>%
  mutate(date = as.Date(as.character(date), format = '%Y%m%d')) %>%
  left_join(calendar_raw) %>%
  gather(key, value, -date, -service_id, -start_date, -end_date) %>%
  filter(key == tolower(weekdays(date)) & value == 1) %>%
  select(service_id, date) %>%
  mutate(date = as.numeric(gsub('-', '', date))) %>%
  bind_rows(select(calendar_dates_raw, service_id, date)) %>%
  filter(date >= 20191101 & date <= 20191107) %>%
  count(service_id, name = 'n_occurrences')

# Count observations by route
routes <- service_counts %>%
  inner_join(trips_raw) %>%
  left_join(stop_times_raw) %>%
  filter(stop_sequence == '1') %>%
  count(route_id, name = 'n_trips', wt = n_occurrences) %>%
  left_join(routes_raw) %>%
  select(route = route_id, name = route_long_name, color = route_color, n_trips)

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
travel_times <- service_counts %>%
  inner_join(trips_raw) %>%
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
  summarise(travel_time = min(travel_time),
            n_trips = sum(n_occurrences)) %>%
  ungroup()

# Identify observed stops
observed_stops <- service_counts %>%
  inner_join(trips_raw) %>%
  left_join(stop_times_raw) %>%
  {unique(gsub('(N|S)$', '', .$stop_id))}
stops <- stops_raw %>%
  filter(location_type == 1) %>%
  select(stop_id, name = stop_name, lat = stop_lat, lon = stop_lon) %>%
  filter(stop_id %in% observed_stops)

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
         euclidean_dist = sqrt((lat.x - lat.y) ^ 2 + (lon.x - lon.y) ^ 2)) %>%
  filter(has_transfer | euclidean_dist < 0.001) %>%
  # Assign unique ID and name to each set of disambiguated stops
  distinct(stop_id.x, stop_id.y, name.y, lat.y, lon.y) %>%
  group_by(stop_id = stop_id.x) %>%
  summarise(id = min(stop_id.y),
            name = paste(sort(unique(name.y)), collapse = '&'),
            lat = mean(lat.y),  # Centroid
            lon = mean(lon.y)) %>%
  ungroup() %>%
  mutate(id = as.numeric(as.factor(id)),
         name = sub('&([^&]*)$', ' and \\1', name),
         name = gsub('&', ', ', name)) %>%
  arrange(id, stop_id)

# Identify nodes
nodes <- disambiguated_stops %>%
  distinct(id, name, lat, lon) %>%
  mutate_at(c('lat', 'lon'), round, 6)  # Preserve original precision

# Identify edges
edges <- travel_times %>%
  left_join(disambiguated_stops, by = c('from_stop_id' = 'stop_id')) %>%
  left_join(disambiguated_stops, by = c('to_stop_id' = 'stop_id')) %>%
  group_by(source = id.x, target = id.y, route = route_id, direction_id) %>%
  summarise(travel_time = min(travel_time),
            n_trips = sum(n_trips)) %>%
  ungroup() %>%
  # Remove directed route-specific "shortcuts"
  left_join(nodes, by = c('source' = 'id')) %>%
  left_join(nodes, by = c('target' = 'id')) %>%
  group_by(source, route, direction_id) %>%
  slice(which.min((lat.x - lat.y) ^ 2 + (lon.x - lon.y) ^ 2)) %>%
  ungroup() %>%
  select(-direction_id, -ends_with('x'), -ends_with('y')) %>%
  arrange(source, target, route) 

# Export data
write_csv(edges, 'data-raw/edges.csv')
write_csv(mutate_at(nodes, c('lat', 'lon'), as.character), 'data-raw/nodes.csv')
write_csv(routes, 'data-raw/routes.csv')
save(edges, file = 'data/edges.rda')
save(nodes, file = 'data/nodes.rda')
save(routes, file = 'data/routes.rda')

# Save session info
options(width = 80)
write_lines(capture.output(sessioninfo::session_info()), 'data-raw/data.log')
