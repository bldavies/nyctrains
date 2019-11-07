# TRANSIT.R
#
# This script downloads the latest Google Transit data released by the MTA.
#
# Ben Davies
# November 2019

# Download and unzip data
tmp <- tempfile(fileext = '.zip')
download.file('http://web.mta.info/developers/data/nyct/subway/google_transit.zip', tmp)
unzip(tmp, files = unzip(tmp, list = T)$Name, exdir = 'data-raw/transit')

# Delete temporary file
unlink(tmp)

# Save session info
options(width = 80)
writeLines(capture.output(sessioninfo::session_info()), 'data-raw/transit.log')
