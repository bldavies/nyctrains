# nyctrains

nyctrains is an R package containing data on the New York City subway network.
The package provides the following three data frames.

* `routes`: subway route names and colors.
* `stops`: subway stop names and locations.
* `travel_times`: route-specific minimum travel times between consecutive stops.

These data are derived from static [GTFS](https://developers.google.com/transit/gtfs/) feeds published by the Metropolitan Transportation Authority [here](http://web.mta.info/developers/developer-data-terms.html#data).

## Installation

nyctrains can be installed from GitHub via [remotes](https://github.com/r-lib/remotes):

```r
library(remotes)
install_github('bldavies/nyctrains')
```

## License

CC0
