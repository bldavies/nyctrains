#' NYC Subway Routes
#' 
#' Data frame containing metadata on subway routes.
#' 
#' @docType data
#' 
#' @name routes
#' 
#' @usage data(routes)
#' 
#' @format Data frame with columns
#' \describe{
#' \item{route_id}{Route ID}
#' \item{route_name}{Route name}
#' \item{route_color}{Route color hex code}
#' }
#' 
#' @source \href{https://new.mta.info/developers/open-data}{Metropolitan Transport Authority}
"routes"

#' NYC Subway Stops
#' 
#' Data frame containing metadata on subway stops.
#' 
#' Some stops emcompass multiple stations within a complex.
#' Such stops have coordinates equal to the centroid of stations at the complex.
#' 
#' @docType data
#' 
#' @name stops
#' 
#' @usage data(stops)
#' 
#' @format Data frame with columns
#' \describe{
#' \item{stop_id}{Stop ID}
#' \item{stop_name}{Stop name}
#' \item{stop_lat,stop_lon}{Stop latitude/longitude coordinates}
#' \item{stop_borough}{Borough containing stop}
#' }
#' 
#' @source \href{https://new.mta.info/developers/open-data}{Metropolitan Transport Authority}
"stops"

#' NYC Subway Travel Times
#' 
#' Data frame containing minimum travel times between subway stops.
#' 
#' Each travel time represents the route-specific minimum difference between
#' the scheduled departure time from the source stop and the scheduled arrival
#' time at the target stop.
#' Travel times do not include delays between arrival and departure, nor
#' time taken to transfer between stations within a complex.
#' Therefore, summing travel times along a path provides a lower bound on the
#' total time required to traverse that path.
#' 
#' @docType data
#' 
#' @name travel_times
#' 
#' @usage data(travel_times)
#' 
#' @format Data frame with columns
#' \describe{
#' \item{source}{Source stop ID}
#' \item{target}{Target stop ID}
#' \item{route_id}{Route ID}
#' \item{travel_time}{Minimum scheduled travel time between stops, in seconds}
#' }
#' 
#' @source \href{https://new.mta.info/developers/open-data}{Metropolitan Transport Authority}
"travel_times"
