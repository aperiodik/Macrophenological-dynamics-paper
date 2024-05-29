# loading and data and set properties for aggregating time series

# shape file of the grid cells MTBs (locations)
print("reading shape file with ordnance survey maps")
MTB = readOGR("data/mtbschnitt.shp")

# shape file of Germany
print("reading shape file of Germany")
germany_0 = readOGR("data/gadm36_DEU_0.shp")

# read German census data 
print("reading census data")
zensus = read_csv("data/mtbschnitt_zensus2011.csv")

# read Flora Inkognita data
print("reading Flora Incognita")
dafa  = read_csv("data/fia_data1821.csv")
# with data variables:
#   doy = day of year of observation
#   NAME = observation location, i.e. grid number
#   species = species observed

## ----- aggregate time series with time window (TW) properties
# time series properties with TWs
wsize <- 90 # = window size = number of data pots per window
skp <- 7 # = number of days to skip
maxDay <- 366 # no. of days per year (includes leap year)
nowi <- length(seq(1,maxDay, by=skp))  # 53
# for Isomap
k <-  16 # no of kNN
