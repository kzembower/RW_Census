## This is a file of experiements using the tidycensus package. See
## the book at https://walker-data.com/census-r/index.html.

## Also see the books at
## https://r-spatial.github.io/sf/articles/sf1.html and
## https://r.geocompx.org/adv-map.html?#interactive-maps

library(tidycensus)
library(tidyverse)

library(tigris)
options(tigris_use_cache = TRUE)

library(sf)

library(crsuggest)

total_population_10 <- get_decennial(
  geography = "state", 
  variables = "P001001",
  year = 2010
)

aian_2020 <- get_decennial(
  geography = "state",
  variables = "P1_005N",
  year = 2020,
  sumfile = "pl"
)

born_in_mexico_1yr <- get_acs(
  geography = "state", 
  variables = "B05006_150", 
  survey = "acs1",
  year = 2021 #2022 doesn't work, as of 28 May 2023
)

md_income <- get_acs(
  geography = "county", 
  variables = "B19013_001", 
  state = "MD",
  year = 2020
)

BalCity_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "MD", 
  county = "Baltimore city",
  year = 2021
)


## Baltimore City 2020 Census Tracts
## Radnor-Winston
## Census Tract: 2711.01 (Bounds: Homewood Ave, York Road, E. Cold Spring Lane, N. Charles Street. Much area not in RW)
## Census Blocks:
## 3000 Bounds: Notre Dame Lane, York Road, Winston Ave, Norwood Road (Not RW: Properties along York Road, including McDonald's and Loyola Public service facility
## 3001 Bounds: Notre Dame Lane, Norwood Road, Winston Ave, Whiteford Ave
## 3002 Bounds: Notre Dae Lane, Whiteford Ave, Winston Ave, Underwood Road 
## 3005 Bounds: Winston Ave, The Alley, Rossiter Ave, Loyola fence
## 3006 Bounds: Winston Ave, York Road, Rossiter Ave, The Alley (Not RW: Properties along York Road, including Medical Center, Popeye's and Loyola business center)
## 3007 Bounds: Rossiter AVe, The Alley, Radnor Road, Loyola fence
## 3008 Bounds: Rossiter Ave, Crowson Ave, Radnor Road, The Alley
## 3009 Bounds: Rossiter Ave, York Road, Radnor Road, Crowson Ave
## 3010 Bounds: Radnor Road, York Road, Charter Oak Ave, Crowson Ave (Contains Loyola IT, Homeland Vocational Center)
## 3011 Bounds: Radnor Road, Crowson Ave, Charter Oad Ave, Norwood Road
## 3012 Bounds: Radnor Road, Norwood Road, Charter Oak Ave, York Road, Coldspring Lane, Kernway (Not RW: Properties along Kerneway and Coldspring Lane)

cimarron_blocks <- get_decennial(
  geography = "block",
  variables = "H1_001N",
  state = "OK",
  county = "Cimarron",
  year = 2020,
  sumfile = "pl"
)

bc_blocks <- get_decennial(
  geography = "block",
  variables = "H1_001N",
  state = "MD",
  county = "Baltimore city",
  year = 2020,
  sumfile = "pl"
)

## H1_001N: Number of households per census block?
## Maryland: FIPS code 24
## Baltimore city: FIPS code 510

ga <- get_acs(
  geography = "county",
  state = "Georgia",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  year = 2020
)

## B19013_001 is median household income
## B01002_001 is median age

bc <- get_acs(
  geography = "county",
  state = "MD",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  year = 2020
)

rw_blocks <- c(3000, 3001, 3002, 3005, 3006, 3007, 3008, 3009, 3010, 3011)

rw <- get_decennial(
  geography = "block",
  state = "MD",
  county = "Baltimore city",
  ## variables = rw_blocks,
  variables = c(housing_units = "H001001N"
                ## own_mort = "H4_002N",
                ## tot_pop = "P1_001N"               
                ),
  year = 2020,
  show_call = TRUE
)

load_variables(year = 2020, dataset = "pl")


## TIGRIS work
st <- states()
st

plot(st$geometry)

md_counties <- counties("MD")

plot(md_counties$geometry)

md_blocks <- blocks("MD")

## Don't run this: too slow
plot(md_blocks$geometry)

baltcity_blocks <- blocks("MD", "Baltimore city")

## Don't run this: too slow
plot(baltcity_blocks$geometry)

ggplot(baltcity_blocks) +
    geom_sf()

st_crs(baltcity_blocks)

suggest_crs(baltcity_blocks)

baltcity_blocks_proj <- st_transform(baltcity_blocks, crs = 6487)

head(baltcity_blocks_proj)

st_crs(baltcity_blocks_proj)

## This prints with degrees lat/long:
ggplot(baltcity_blocks) +
    geom_sf() +
    coord_sf(crs = 6487)

## This prints with feet? meters?
ggplot(baltcity_blocks) +
    geom_sf() +
    coord_sf(crs = 6487, datum = 6487)


## Chapter 6:

dc_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001", #Median household income
  state = "DC", 
  year = 2020,
  geometry = TRUE
)

plot(dc_income["estimate"])

us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry()

plot(us_median_age$geometry)

ggplot(data = us_median_age, aes(fill = estimate)) + 
    geom_sf()

ggplot(data = us_median_age, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) + 
  labs(title = "  Median Age by State, 2019",
       caption = "Data source: 2019 1-year ACS, US Census Bureau",
       fill = "ACS estimate") + 
    theme_void()

baltcity_zctas <- zctas(cb = TRUE, starts_with = "21212", year = 2020)

## RW data:

## Working:
bc <- get_decennial(
  geography = "block",
  state = "MD",
  county = "Baltimore city",
  variables = c(tot_HH = "H1_001N"),
  year = 2020,
  sumfile = "dhc",
  ## show_call = TRUE,
  geometry = TRUE
)

## Working:
bc <- get_decennial(
  geography = "block",
  state = "MD",
  county = "Baltimore city",
  variables = c(tot_HH = "H1_001N", #Total households
                tot_rent = "H10_010N"), #Total renters
  year = 2020,
  sumfile = "dhc",
  ## show_call = TRUE,
  geometry = TRUE,
  output = "wide"
)

rw_blocks <- c(3000, 3001, 3002, 3005, 3006, 3007, 3008, 3009, 3010, 3011)

rw <- bc %>%
    filter(substr(GEOID, 6, 11) == "271101" & substr(GEOID, 12, 15) %in% rw_blocks)

load_variables(2020, "dhc") #This works

## Getting whole tables
bc_P14 <- get_decennial(
  geography = "block",
  state = "MD",
  county = "Baltimore city",
  cache_table = TRUE,
  table = "P14",
  year = 2020,
  sumfile = "dhc",
  summary_var = "P14_001N",
  ## show_call = TRUE,
  ## geometry = TRUE
)

rw_P14 <- bc_P14%>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_blocks) %>%
    mutate(Block = substr(GEOID, 12, 15)) %>%
    select(! c(NAME, GEOID))

## Calculate sum for all RW blocks:
rw_P14 %>%
    group_by(variable) %>%
    summarize(total = sum(value)) %>%
    arrange(desc(total))

## Try to plot a base map with RW census blocks shown
rw_pop <- get_decennial(
    geography = "block",
    variables = "P1_001N",
    year = 2020,
    state = "MD",
    county = "Baltimore city",
    geometry = TRUE
) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_blocks)

mapview(rw_pop)
## This works very well, for a quick first attempt.

rw_pop_proj <- st_transform(rw_pop, crs = 6488)

## Reproducible example:
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)

rw_blocks <- c(3000, 3001, 3002, 3005, 3006, 3007, 3008, 3009, 3010, 3011)

rw_pop <- get_decennial(
    geography = "block",
    variables = "P1_001N",
    year = 2020,
    state = "MD",
    county = "Baltimore city",
    geometry = TRUE
) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_blocks)

(rw_pop_map <- rw_pop %>%
     st_transform('+proj=longlat +datum=WGS84') %>%
     leaflet() %>%
     fitBounds(-76.616, 39.352, -76.610, 39.346) %>%
     addTiles() %>%
     addPolygons()
)
    
rw_pop_map


## Getting just the block group 3:
blkgrp_pop <- get_decennial(
    geography = "block group",
    variables = "P1_001N",
    year = 2020,
    state = "MD",
    county = "Baltimore city",
    geometry = TRUE
) %>%
    filter(substr(GEOID, 6, 12) == "2711013") #Block group 3 of census
                                              #tract 2711.01
  
(blkgrp_map <- blkgrp_pop %>%
     st_transform('+proj=longlat +datum=WGS84') %>%
     leaflet() %>%
     addTiles() %>%
     addPolygons()
)

## Getting just census tract 2711.01::
tract_pop <- get_decennial(
    geography = "tract",
    variables = "P1_001N",
    year = 2020,
    state = "MD",
    county = "Baltimore city",
    geometry = TRUE
) %>%
    filter(substr(GEOID, 6, 11) == "271101") #tract 2711.01
  
(tract_map <- tract_pop %>%
     st_transform('+proj=longlat +datum=WGS84') %>%
     leaflet() %>%
     addTiles() %>%
     addPolygons()
)


## Using tigris to get just the geometry, not the census data
library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(leaflet)
library(leafem)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list) %>%
    st_transform('+proj=longlat +datum=WGS84')

rw_blocks %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons()

rw_blockgrp <- block_groups(state = "MD",
                            county = "Baltimore city",
                            year = "2020") %>%
    filter(TRACTCE == "271101" & BLKGRPCE == "3") %>%
    st_transform('+proj=longlat +datum=WGS84')

rw_blockgrp %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons()

rw_tracts <- tracts(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(TRACTCE == "271101") %>%
    st_transform('+proj=longlat +datum=WGS84')

rw_tracts %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons()

lopot = labelOptions(noHide = TRUE,
                     direction = 'top',
                     textOnly = TRUE)

rw_tracts %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(color = "#00E600") %>%
    ## addAwesomeMarkers(lng = -76.62, lat = 39.352,
    ##                   label = "2711.01",
    ##                   labelOptions(
    ##                       noHide = TRUE,
    ##                       direction = 'top',
    ##                       textOnly = TRUE,
    ##                       opacity = 1,
    ##                       textsize = "60px",
    ##                       style = list(
    ##                           "color" = "#FFFFFF", ## "#00E600",
    ##                           "font-weight" = "bold")
    ##                   ),
    ##                   options = markerOptions(
    ##                       ## interactive = FALSE,
    ##                       ## clickable = FALSE,
    ##                       ## draggable = FALSE,
    ##                       ## keyboard = FALSE,
    ##                   ),
    ## ) %>%
    addStaticLabels(
        lng = -76.63, lat = 39.36,
        label = "2711.01",
        style = list("color" = "#009900",
                     "font-weight" = "bold",
                     "font-size" = "60px")
    ) %>%
    addLabelOnlyMarkers(
        label = "2711.01",
        lng = -76.613, lat = 39.348,
        labelOptions(noHide = TRUE,
                     direction = 'top',
                     textOnly = TRUE,
                     opacity = 1,
                     textsize = "60px",
                     style = list("color" = "#FFFFFF", ## "#00E600",
                                  "font-weight" = "bold")
                     )
    )

%>%
    addPolygons(data = rw_blocks, color = "#06F") %>%
    addPolygons(data = rw_blockgrp, color = "#CC0066")


## Reproducible example
library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(leaflet)
library(leafem)

rw_tracts <- tracts(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(TRACTCE == "271101") %>%
    st_transform('+proj=longlat +datum=WGS84')

lopot = labelOptions(noHide = TRUE,
                     direction = 'top',
                     textOnly = TRUE)

rw_tracts %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(color = "#00E600") %>%
    ## addAwesomeMarkers(lng = -76.62, lat = 39.352,
    ##                   label = "2711.01",
    ##                   labelOptions(
    ##                       noHide = TRUE,
    ##                       direction = 'top',
    ##                       textOnly = TRUE,
    ##                       opacity = 1,
    ##                       textsize = "60px",
    ##                       style = list(
    ##                           "color" = "#FFFFFF", ## "#00E600",
    ##                           "font-weight" = "bold")
    ##                   ),
    ##                   options = markerOptions(
    ##                       ## interactive = FALSE,
    ##                       ## clickable = FALSE,
    ##                       ## draggable = FALSE,
    ##                       ## keyboard = FALSE,
    ##                   ),
    ## ) %>%
    addStaticLabels(
        lng = -76.63, lat = 39.36,
        label = "2711.01",
        style = list("color" = "#009900",
                     "font-weight" = "bold",
                     "font-size" = "60px")
    ) %>%
    addLabelOnlyMarkers(
        label = "2711.01",
        lng = -76.613, lat = 39.348,
        labelOptions(noHide = TRUE,
                     direction = 'top',
                     textOnly = TRUE,
                     opacity = 1,
                     textsize = "60px",
                     style = list("color" = "#FFFFFF", ## "#00E600",
                                  "font-weight" = "bold")
                     )
    )

## Changing direction, from leaflet() to something else, that doesn't
## include interactive features

## Maybe use osmdata, osmextract, ggmap, sf, osmar?
library(tidyverse)
library(OpenStreetMap)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)

lat_max <- 39.3525 #Distance from 39.35 to 39.34 = 0.691mi
long_max <- -76.617 #Distance from -76.61 to -76.62 = 0.5343 mi
lat_min <- 39.3455
long_min <- -76.6095
nw <- c(lat_max, long_max)
se <- c(lat_min, long_min)

rw_map <- openmap(nw, se,
                  type = "osm",
                  mergeTiles = TRUE) %>%
    openproj() %>%
    OpenStreetMap::autoplot.OpenStreetMap() +
    xlab("Long") + ylab("Lat") #Delete to get rid of borders

rw_tract <- tracts(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(TRACTCE == "271101") %>%
    st_transform('+proj=longlat +datum=WGS84')

## Doesn't work. Error: Aesthetics must be either length 1 or the same as the data
rw_map +
    annotate("text",
             x = -76.613,
             y = 39.350,
             label = "Some text",
             size = 20,
             color = "red") +
    geom_sf(data = rw_tracts$geometry) +
    ## addPolygons(data = rw_tracts$geometry) +
    ggtitle("Radnor Winston Neighhborhood")

## Produces map in degrees lat/long:
rw_map +
    annotate("text",
             x = -76.613,
             y = 39.350,
             label = "Some text",
             size = 20,
             color = "red") +
    ggtitle("Radnor Winston Neighhborhood")

rw_map

## Experiment, using sf:
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(OpenStreetMap)
## library(sp)
## library(ggplot2)

lat_max <- 39.3525 #Distance from 39.35 to 39.34 = 0.691mi
long_max <- -76.617 #Distance from -76.61 to -76.62 = 0.5343 mi
lat_min <- 39.3455
long_min <- -76.6095
nw <- c(lat_max, long_max)
se <- c(lat_min, long_min)

## Produces map in degrees lat/long:
rw_map <- openmap(nw, se,
                  type = "osm",
                  mergeTiles = TRUE) %>%
    openproj() %>%
    OpenStreetMap::autoplot.OpenStreetMap() +
    xlab("long") + ylab("lat")

rw_map

rw_tract <- tracts(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(NAME == "2711.01")
    ## openproj()
    ## st_transform('+proj=longlat +datum=WGS84')
    ## spTransform('osm')

OpenStreetMap::autoplot.OpenStreetMap(rw_tract, add = TRUE)

## produces map in degrees lat/long
rw_map + 
    annotate("text",
             x = -76.613,
             y = 39.350,
             label = "Some text",
             size = 20,
             color = "red") +
    xlab("Long") + ylab("Lat")   #Delete to get rid of borders
    ## geom_sf(rw_tract)

plot(rw_map)

## Below causes error: "Objects of class <gg> are not supported by autoplot"
## autoplot(rw_map)
dput(rw_map)

    ## xlab("Long") + ylab("Lat") #Delete to get rid of borders


rw_map +
    
rw_map <- openmap(nw, se,
                  type = "osm",
                  mergeTiles = TRUE) %>%
    openproj()

plot(rw_map)

## Using just ggplot
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(OpenStreetMap)
library(ggplot2)
library(maps)

## Get an Open Street Map:
rw_map <- openmap(nw, se,
                  type = "osm",
                  mergeTiles = TRUE) %>%
    openproj(projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Get an example census map:
rw_tract <- tracts(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(NAME == "2711.01")

## This works: Degrees lat/long
autoplot.OpenStreetMap(rw_map)

## So does this:
plot(rw_tract$geometry)

## These don't:
autoplot.OpenStreetMap(rw_map) +
    geom_sf(rw_tract$geometry)

ggplot(map_data(rw_map), aes(long, lat))


ggplot(aes(x="long", y="lat")) +
    geom_sf(rw_map$geometry)

## Trying with tmap:
library(tidyverse)
library(sf)
library(tmap)
library(terra)
library(spData)
library(spDataLarge)
library(tigris)
options(tigris_use_cache = TRUE)
library(OpenStreetMap)

## All New Zealand examples from https://r.geocompx.org/adv-map.html
nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

tm_shape(nz) +
    tm_fill()

map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)

map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)

map_nz1

nz_water = st_union(nz) |> st_buffer(22200) |> 
  st_cast(to = "LINESTRING")

map_nz2 = map_nz1 +
    tm_shape(nz_water) + tm_lines()

map_nz2

map_nz3 = map_nz2 +
    tm_shape(nz_height) + tm_dots()
map_nz3

tmap_arrange(map_nz1, map_nz2, map_nz3)

ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

plot(st_geometry(nz), col = nz$Land_area)  # works

tm_shape(nz) + tm_fill(col = nz$Land_area) # fails
                                        # Error: Fill argument neither colors nor valid variable name(s)

tm_shape(nz) + tm_fill(col = "Land_area")

nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) |> 
  st_as_sfc()

nz_region

nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
    tm_scale_bar(position = c("left", "bottom"))

nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
    tm_shape(nz_region) + tm_borders(lwd = 3)

library(grid)
nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

## Get an Open Street Map:
rw_map <- openmap(nw, se,
                  type = "osm",
                  mergeTiles = TRUE) %>%
    openproj(projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Get an example census map:
rw_tract <- tracts(state = "MD",
                   county = "Baltimore city",
                   year = "2020") %>%
    filter(NAME == "2711.01")

ttm()

## Quick Tmap; also works:
qtm(rw_tract)

## Also works. _polygons combines _fill and _borders:
tm_shape(rw_tract) +
    tm_polygons()

## Works:
tm_shape(rw_tract) +
    tm_polygons(alpha = 0.2, col = "green") +
    tm_scale_bar() +
    tm_layout(title = "Radnor-Winston Neighborhood") +
    tm_basemap(server = "OpenStreetMap")

## Example from suggestion from Tim Howard,
## https://stat.ethz.ch/pipermail/r-sig-geo/2023-June/029290.html and
## https://stat.ethz.ch/pipermail/r-sig-geo/2023-June/029291.html
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

## Get an example census map:
rw_tract <- tracts(state = "MD",
                   county = "Baltimore city",
                   year = "2020") 
rw_tract <- rw_tract[rw_tract$NAME == "2711.01",]

# for some reason had to re-jigger the box a bit.
# also note your longitudes were backwards. 
lat_max <- 39.36 
long_min <- -76.63
lat_min <- 39.34 
long_max <- -76.60 

bbox <- bb(c(xmin=long_min, ymin=lat_min, xmax=long_max, ymax=lat_max))

basem <- read_osm(bbox)

tmap_mode("plot")

myMap <- tm_shape(basem) +
    tm_rgb() +
    tm_shape(rw_tract) +
    tm_polygons(alpha = 0.2, col = "green") + 
    tm_scale_bar() +
    tm_layout(title = "Radnor-Winston Neighborhood")

# check it out. No borders, scale in km
myMap

                                        # save it. 
tmap_save(myMap, "myMapOut.jpg")

## And, Tim's suggestion for an even simplier version:

basem <- read_osm(bb(rw_tract, ext = 1.5))

myMap <- tm_shape(basem) +
  tm_rgb() +
  tm_shape(rw_tract) +
  tm_polygons(alpha = 0.2, col = "green") + 
  tm_scale_bar() +
  tm_layout(title = "Radnor-Winston Neighborhood")

# check it out, no borders, scale in km
myMap

## Try to add borders:
myMap <- tm_shape(basem) +
    tm_rgb() +
    tm_shape(rw_tract) +
    tm_polygons(alpha = 0.2, col = "green") + 
    tm_scale_bar() +
    tm_grid() + tm_xlab("Long") + tm_ylab("Lat")+
    tm_layout(title = "Radnor-Winston Neighborhood")

# check it out. grid in meters
myMap

                                        # save it. 
tmap_save(myMap, "myMapOut_bbox.jpg")

## Experimenting with census maps in Radnor-Winston, with tmap:
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

## Get the RW census map:
rw_tract <- tracts(state = "MD",
                   county = "Baltimore city",
                   year = "2020") 
rw_tract <- rw_tract[rw_tract$NAME == "2711.01",]

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012") #WARNING:
                                        #Adding
                                        #3012 in
                                        #temporarily

rw_neigh_base <- c(-8528000, 4771000)   # Base offset, to reduce
                                        # typing
rw_neigh_polygon <- matrix(c(
    -150, 425,
    -420, 425,
    -500, 550,
    -500, 650,
    -740, 670,
    -730, 880,
    -710, 880,
    -710, 1050,
    -660, 1060,
    -670, 1150,
    -290, 1190,
    -280, 1100,
    -350, 1100,
    -350, 1020,
    -250, 1020,
    -250, 940,
    -280, 930,
    -280, 890,
    -230, 880,
    -225, 780,
    -160, 780,
    -150, 425
), ncol = 2, byrow = TRUE)

colnames(rw_neigh_polygon) <- c("x", "y")

rw_neigh_polygon <- sweep(rw_neigh_polygon,
                          2,
                          rw_neigh_base,
                          FUN = "+")

## Works, ends up with POINTS, not POLYGON:
rw_neigh_pg <- rw_neigh_polygon %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2),
                 crs = 6487) # NAD83(2011) / Maryland (meters)

## Works better, ends up with POLYGON:
(rw_neigh_pg <- rw_neigh_polygon %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2),
                 crs = 6487) %>% # NAD83(2011) / Maryland (meters)
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
)


rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

basem <- read_osm(bb(rw_tract, ext = 1.3))

tmap_mode("plot")

## Makes map with meters:
(RW_tract_map <- tm_shape(basem) +
     tm_rgb() +
     tm_shape(rw_tract) +
     tm_polygons(alpha = 0.2, col = "green") +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.4, palette = "Accent", n = 10) +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

                                        # save it. 
tmap_save(RW_tract_map, "RW_tract_map.jpg")

## Create a map of just the RW blocks:
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

tmap_mode("plot")

(RW_block_map <- tm_shape(rw_base_blocks) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent", n = 10) +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

                                        # save it. 
tmap_save(RW_block_map, "RW_block_map.jpg")



## Test, to answer Felipe:
bc <- counties(state = "MD") %>%
    filter(NAME == "Baltimore city")

#Or using tigris package counties function for extracting the county
cou <- counties(state = "CA")
cou

sacramento_county <- filter(cou,NAME=="Sacramento")
sacramento_county

basem2 <- read_osm(bb(sacramento_county,ext=3))
basem2                    

myMap2 <- tm_shape(basem2) +
    tm_rgb() +
    tm_shape(sacramento_county) +
    tm_polygons(alpha = 0.4, col = "lightblue") +
    tm_scale_bar() +
    tm_layout(main.title = "Sac county")

 # check it out
myMap2

## Test of adding matrix and vector:
(m <- matrix(c(1,2,3,4,5,6,7,8,9,10), ncol = 2, byrow = TRUE))

(v <- c(1,2))

(m + v)

## Another (better?) way of applying the base offset:
rw_neigh_base <- c(-8528000, 4771000)   # Base offset, to reduce
                                        # typing

rw_neigh_polygon <- matrix(c(
    -150, 475,
    -420, 475,
    -500, 550,
    -500, 650,
    -740, 670,
    -730, 880,
    -710, 880,
    -710, 1050,
    -660, 1060,
    -670, 1150,
    -290, 1190,
    -280, 1100,
    -350, 1100,
    -350, 1020,
    -250, 1020,
    -250, 940,
    -280, 930,
    -280, 890,
    -230, 880,
    -225, 780,
    -160, 780,
    -150, 425
), ncol = 2, byrow = TRUE)
colnames(rw_neigh_polygon) <- c("x", "y")
rw_neigh_polygon[,1] <- rw_neigh_polygon[,1] - 8528000
rw_neigh_polygon[,2] <- rw_neigh_polygon[,2] + 4771000

rw_neigh_polygon

## Another way:
# Base offset, to reduce typing
base_x <- -8528000
base_y <-  4771000

rw_neigh_polygon <- matrix(c(
    -150, 475,
    -420, 475,
    -500, 550,
    -500, 650,
    -740, 670,
    -730, 880,
    -710, 880,
    -710, 1050,
    -660, 1060,
    -670, 1150,
    -290, 1190,
    -280, 1100,
    -350, 1100,
    -350, 1020,
    -250, 1020,
    -250, 940,
    -280, 930,
    -280, 890,
    -230, 880,
    -225, 780,
    -160, 780,
    -150, 425
), ncol = 2, byrow = TRUE) %>%
    + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
             nrow = nrow(.))

## Get base map of RW blocks from OSM:

## WRONG: doesn't retrieve correct map. This uses Maryland metric
## projection. Must use NAD83 decimal degrees.
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

## Converting from Maryland metric projection to decimal degrees:

## Plotting a union of all the RW census blocks:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Works:
plot(st_union(rw_blocks$geometry))

## Self-contained example of plotting maps of RW:
library(tidyverse)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

## List the census blocks that make up RW:
rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012") #WARNING:
                                        #Adding
                                        #3012 in
                                        #temporarily

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Get the RW tract census map:
rw_tract <- tracts(state = "MD",
                   county = "Baltimore city",
                   year = "2020") %>%
    filter(NAME == "2711.01")
rw_tract <- rw_tract[rw_tract$NAME == "2711.01",]

## Get a census map of Baltimore City:
bc <- counties(state = "MD") %>%
    filter(NAME == "Baltimore city")

## Create a basemap of just the RW blocks:
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

## Draw a map of the RW census blocks on the RW basemap:
(RW_block_map <- tm_shape(rw_base_blocks) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 1, palette = "Accent", n = 11) +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

## Trying with minimal number of colors:
(RW_block_map <- tm_shape(rw_base_blocks) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_polygons(col = "MAP_COLORS",
                 minimize = TRUE,
                 alpha = 0.6,
                 palette = "Accent") +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

## Create a polygon for the RW neighborhood boundaries:
base_x <- -8528000
base_y <-  4771000

rw_neigh_polygon <- matrix(c(
    -150, 475,
    -420, 475,
    -500, 550,
    -500, 650,
    -740, 670,
    -730, 880,
    -710, 880,
    -710, 1050,
    -660, 1060,
    -670, 1150,
    -290, 1190,
    -280, 1100,
    -350, 1100,
    -350, 1020,
    -250, 1020,
    -250, 940,
    -280, 930,
    -280, 890,
    -230, 880,
    -225, 780,
    -160, 780,
    -150, 425
), ncol = 2, byrow = TRUE) %>%
    + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
             nrow = nrow(.))

(rw_neigh_pg <- rw_neigh_polygon %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2),
                 crs = 6487) %>% # NAD83(2011) / Maryland (meters)
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
)

## Draw map of just RW neighborhood:
rw_base <- read_osm(bb(rw_neigh_pg, ext = 1.3))

(RW_map <- tm_shape(rw_base) +
     tm_rgb() +
     tm_shape(rw_neigh_pg) +
     tm_fill(col = "green", alpha = 0.6) +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

## Draw a map of the RW census blocks on the RW basemap:
(RW_block_map <- tm_shape(rw_base_blocks) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 1, palette = "Accent", n = 11) +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)


## Draw a map with RW, census tract 2711.01 and Baltimore City:

## Example of converting from projected metric coord to decimal
## degrees, from
## https://stackoverflow.com/questions/70524765/converting-projected-coordinates-into-decimals

coords <- c("Longitude", "Latitude")
rowIDs <- 1:3

library(rgdal)

data_old <- data.frame(
    ID = c(1,1,1),
    Longitude = c(451077.2, 451037.6, 451051.2),
    Latitude = c(7117021, 7116991, 7116956)
)

## Or create data like this:
data <-
  structure(
    list(
      Longitude = c(-93.2552,-93.2558,-93.2555),
      Latitude = c(58.0879, 58.0876, 58.0873)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  )

(data_old <- data)
(data_new <- data_old)

library(rgdal)

data_old <- data
coordinates(data_old) <- c("Longitude", "Latitude")
proj4string(data_old) <- CRS("+init=epsg:4326")
data_old <- spTransform(data_old, CRS("+init=epsg:5321"))
data_old <- as.data.frame(data_old)

data_old

data_new <- data_old
coordinates(data_new) <- c("Longitude", "Latitude")
proj4string(data_new) <- CRS("+init=epsg:5321")
data_new <- spTransform(data_new, CRS("+init=epsg:4326"))
data_new <- as.data.frame(data_new)

library(tidyverse)
library(sf)

data_old %>%
  st_as_sf(coords = c("Longitude", "Latitude"), dim = "XY") %>%
  st_set_crs(5321) %>%
  st_transform(4326) %>%
  as.data.frame() %>%
  extract(geometry,
          c('Longitude', 'Latitude'),
          '\\((.*), (.*)\\)',
          convert = TRUE)

coordinates(data_new) <- c("Longitude", "Latitude")

data_new

proj4string(data_new) <- CRS("+init=epsg:5321")

data_new

(data_new <- spTransform(data_new, CRS("+init=epsg:4326")))

(data_new <- as.data.frame(data_new))

## From same webpage, using tidyverse and sf:
data_old %>% #Convert from metric grid to decimal degrees
  st_as_sf(coords = c("Longitude", "Latitude"), dim = "XY") %>%
  st_set_crs(5321) %>%
  st_transform(4326) %>%
  as.data.frame() %>%
  extract(geometry,
          c('Longitude', 'Latitude'),
          '\\((.*), (.*)\\)',
          convert = TRUE)

data_old %>% #Convert from metric grid to decimal degrees
    st_as_sf(coords = c("Longitude", "Latitude"), dim = "XY") %>%
    st_set_crs(5321) %>%
    st_transform(4326) %>%
    as.data.frame() %>%
    extract(geometry,
            c('Longitude', 'Latitude'),
            '\\((.*), (.*)\\)',
            convert = TRUE)

## Trying now with RW polygon:
base_x <- -8528000
base_y <-  4771000

rw_neigh_polygon <- data.frame(
    matrix(c(
        -150, 475,
        -420, 475,
        -500, 550,
        -500, 650,
        -740, 670,
        -730, 880,
        -710, 880,
        -710, 1050,
        -660, 1060,
        -670, 1150,
        -290, 1190,
        -280, 1100,
        -350, 1100,
        -350, 1020,
        -250, 1020,
        -250, 940,
        -280, 930,
        -280, 890,
        -230, 880,
        -225, 780,
        -160, 780,
        -150, 425
    ), ncol = 2, byrow = TRUE) %>%
    + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
             nrow = nrow(.)),
    col.names = "Longitude", "Latitude")

rw_neigh_polygon <- data.frame(
    matrix(
        c(
            -150, 475,
            -420, 475,
            -500, 550,
            -500, 650,
            -740, 670,
            -730, 880,
            -710, 880,
            -710, 1050,
            -660, 1060,
            -670, 1150,
            -290, 1190,
            -280, 1100,
            -350, 1100,
            -350, 1020,
            -250, 1020,
            -250, 940,
            -280, 930,
            -280, 890,
            -230, 880,
            -225, 780,
            -160, 780,
            -150, 425
        ),
        ncol = 2, byrow = TRUE)
) %>% + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
               nrow = nrow(.))
colnames(rw_neigh_polygon) <- c("Longitude", "Latitude")

## Some tools to use:
guess_crs(rw_neigh_polygon, target_location = c(-76.61246, 39.35010))
suggest_crs()
EPSG <- make_EPSG()
EPSG[grep("4269", EPSG$code),]

rw_neigh_polygon %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), dim = "XY") %>%
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_set_crs(6487)

%>%
    st_transform(4269)
    

sf::st_as_sf(coords = c(1,2),
                 crs = 6487) %>% # NAD83(2011) / Maryland (meters)
    summarize(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
)


## 12 June. Goal: to produce lat/long map with tmap tools, and meter
## map with OSM tools.

## This gives degrees lat long: Changing to meters:
## This is routine #1:
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(OpenStreetMap)

lat_max <- 39.3525
long_max <- -76.617
lat_min <- 39.3455
long_min <- -76.6095
nw <- c(lat_max, long_max)
se <- c(lat_min, long_min)

(rw_map <- openmap(nw, se,
                   type = "osm",
                   mergeTiles = TRUE) %>%
     openproj("+init=epsg:6487") %>%
     OpenStreetMap::autoplot.OpenStreetMap() +
     xlab("long") + ylab("lat")
) #Wish I could add a grid overlay to this map.

png("rw_map.png")
plot(rw_map)
dev.off()

## This gives map in meters: changing to degrees:
## This is routine #2:
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012")

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Create a map of just the RW blocks:
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

tmap_mode("plot")

(RW_block_map <- tm_shape(rw_base_blocks, projection = 4326) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders() +
     tm_scale_bar() +
     tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

tmap_save(RW_block_map, "rw_map_tmap_degrees.png")

## This gives map in meters:
## This is routine #3:
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012")

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Create a map of just the RW blocks:
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

tmap_mode("plot")

## Line below gives map in meters
(RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
## Line below gives map in degrees
## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders() +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_grid() +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

tmap_save(RW_block_map, "rw_map.png")

## Creating a polygon for RW neighborhood, based on CRS 6487 (NAD83
## (2011) / Maryland ) map in meters:
base_x <- 433000
base_y <- 186000
rw_neigh_pg_m <- data.frame(
    matrix(
        c(540, 1140,
          540, 1070,
          480, 1060,
          490, 1000,
          570, 1000,
          570, 940,
          550, 930,
          550, 890,
          580, 890,
          590, 820,
          640, 820,
          650, 590,
          520, 580,
          470, 580,
          350, 660,
          350, 710,
          180, 725,
          190, 900,
          220, 900,
          220, 1030,
          240, 1030,
          240, 1110
        ),
        ncol = 2, byrow = TRUE)
) %>% + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
               nrow = nrow(.)) %>%
sf::st_as_sf(coords = c(1,2), dim = "XY") %>%
summarize(geometry = st_combine(geometry)) %>%
st_cast("POLYGON") %>%
st_set_crs(6487)

## Map it:
rw_base_blocks <- read_osm(bb(rw_neigh_pg_m, ext = 1.3))

## Line below gives map in meters
(RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
## Line below gives map in degrees
## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_neigh_pg_m) +
     tm_fill(col = "green", alpha = 0.2) +
     tm_borders(lwd = 2, alpha = 1) +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_grid() +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

tmap_save(RW_block_map, "rw_map.png")

## Just map census block 3012 on RW OSM:
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
library(tmaptools)

rw_3012 <- c("3012")

## Get the RW blocks from the census:
rw_3012_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) == rw_3012)

## Create a map of just the RW blocks:
rw_base_3012 <- read_osm(bb(rw_3012_blocks, ext = 1.3))

## Line below gives map in meters
(RW_3012_map <- tm_shape(rw_base_3012, projection = 6487) +
## Line below gives map in degrees
## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_3012_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders(lwd = 2) +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     ## tm_grid() +
     tm_layout(title = "US Census block 3012")
)

tmap_save(RW_3012_map, "rw_3012_map.pdf")

## Or, create thw whole RW boundaries and census blocks, and use
## crop_shape to limit it to just 3012:

## Creating a polygon for RW neighborhood, based on CRS 6487 (NAD83
## (2011) / Maryland ) map in meters:
base_x <- 433000
base_y <- 186000
rw_neigh_pg_m <- data.frame(
    matrix(
        c(540, 1140,
          540, 1070,
          480, 1060,
          490, 1000,
          570, 1000,
          570, 940,
          550, 930,
          550, 890,
          580, 890,
          590, 820,
          640, 820,
          650, 590,
          520, 580,
          470, 580,
          350, 660,
          350, 710,
          180, 725,
          190, 900,
          220, 900,
          220, 1030,
          240, 1030,
          240, 1110
        ),
        ncol = 2, byrow = TRUE)
) %>% + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
               nrow = nrow(.)) %>%
sf::st_as_sf(coords = c(1,2), dim = "XY") %>%
summarize(geometry = st_combine(geometry)) %>%
st_cast("POLYGON") %>%
st_set_crs(6487)

## Get the basemap:
rw_base_blocks <- read_osm(bb(rw_neigh_pg_m, ext = 1.3))

## Get the geometry for the US census blocks in RW:
rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012")

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Bounding box for 3012 block, in CRS 6487:
bb_3012 <- bb(matrix(c(433100, 186500, # SW LL corner
                    433700, 186750), ncol = 2)) # NE. UR corner

## Line below gives map in meters
(RW_block_map <- tm_shape(rw_base_blocks, projection = 6487, bbox = bb_3012) +
     ## Line below gives map in degrees
     ## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_neigh_pg_m) +
     ## tm_fill(col = "green", alpha = 0.2) +
     tm_borders(lwd = 4, alpha = 1) +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders(lwd = 2) +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_grid() +
     tm_layout(title = "US Census block 3012 and the Radnor-Winston Neighborhood")
)

tmap_save(RW_block_map, "rw_map.pdf")

## 15 June: Actually starting to work with Census data:
library(tidyverse)
library(tidycensus)

## Some modified exercises from
## https://walker-data.com/census-r/an-introduction-to-tidycensus.html
(total_population_10 <- get_decennial(
     geography = "state",
     variables = "P001001",
     year = 2010,
     ## sumfile = "pl" ## This is the default
 ) %>% filter(NAME=="Maryland")
)

## This works for 2010, but not for 2020:
(bc_pop <- get_decennial(
     geography = "county",
     state = "Maryland",
     county = "Baltimore city",
     variables = "P001001",
     year = 2010,
     )
)

## This works for 2020:
(md__pop <- get_decennial(
     geography = "state",
     state = "Maryland",
     variables = "P1_001N",
     year = 2020,
     )
)

load_variables(year = "2020",
               dataset="pl",
               cache = TRUE) %>%
    print(n=301)

(bc__pop <- get_decennial(
     geography = "county",
     state = "Maryland",
     county = "Baltimore city",
     variables = "P1_001N",
     sumfile = "pl",
     year = 2020,
     )
)

(bc271101__pop <- get_decennial(
     geography = "tract",
     state = "Maryland",
     county = "Baltimore city",
     variables = "P1_001N",
     sumfile = "pl",
     year = 2020,
     ) %>%
     filter(GEOID == "24510271101")
)

get_decennial(
  geography = "block",
  variables = "H1_001N",
  state = "MD",
  county = "Baltimore city",
  year = 2020,
  sumfile = "pl"
)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

## Get the RW blocks from the census:
(rw_blocks_pop <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    variables = c(pop = "P1_001N"),
    year = "2020",
    sumfile = "pl") %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
     arrange(GEOID)
)

## Chap 3:
## Get the RW blocks from the census:
(rw_blocks_hh <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    ## variables = c(pop = "H1_001N"),
    table = "H1",
    year = "2020",
    sumfile = "dhc") %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
     arrange(GEOID)
)

## Above gives different variables than below:
(rw_blocks_hh <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    ## variables = c(pop = "H1_001N"),
    table = "H1",
    year = "2020",
    sumfile = "pl") %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
     arrange(GEOID)
)

## Compute percentages:
occ_vars <- c(
    Occupied = "H1_002N",
    Vacant = "H1_003N"
)

(rw_blocks_hh_pc <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    ## variables = c(pop = "H1_001N"),
    variables = occ_vars,
    summary_var = "H1_001N",
    year = "2020",
    sumfile = "pl") %>%
##     select(!NAME) %>% #Drop the NAME column
     mutate(NAME = NULL) %>%
     filter(substr(GEOID, 6, 11) == "271101" &
            substr(GEOID, 12, 15) %in% rw_block_list) %>%
    mutate(percent = 100 * (value / summary_value)) %>%
    arrange(GEOID) %>%
    select(GEOID, variable, value, percent) 
)

## Compute percentages:
occ_vars <- c(
    Total = "H1_001N",
    Occupied = "H1_002N",
    Vacant = "H1_003N"
)

## Summarize for all of RW:
(rw_blocks_hh_pc <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    ## variables = c(pop = "H1_001N"),
    variables = occ_vars,
    summary_var = "H1_001N",
    year = "2020",
    sumfile = "pl") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
    mutate(percent = 100 * (value / summary_value)) %>%
    arrange(GEOID)
    ## select(GEOID, variable, value, percent) %>%
    ## group_by(variable) %>%
    ## summarize(count = sum(value))
)

load_variables(year = "2020", dataset = "pl", cache = TRUE)

## Try to do all of the above, with a whole table (to cache and reduce
## time)
## This only returns H1_001N variable, in 'pl' dataset
(rw_blocks_hh_pc <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "pl") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
    mutate(percent = 100 * (value / summary_value)) %>%
    arrange(GEOID)
)

## This returns all three H1 variables, in 'sf1' dataset
## Define the get_decennial() call separately, to reduce time:
(bc_H1 <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    summary_var = "H1_001N",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(rw_H1 <- bc_H1 %>%
     mutate(NAME = NULL) %>%
     filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) 
)

print(rw_H1, n=30)

(rw_H1_sum <- rw_H1 %>%
     group_by(variable) %>%
     summarize(count = sum(value))
)

(rw_H1_sum_pc <- rw_H1_sum %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Combine three actions above:
(rw_H1_sum_pc <- bc_H1 %>%
     mutate(NAME = NULL) %>%
     filter(substr(GEOID, 6, 11) == "271101" &
            substr(GEOID, 12, 15) %in% rw_block_list) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for census tract 2711.01:
(t271101_H1 <- get_decennial(
    geography = "tract",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    summary_var = "H1_001N",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1") %>%
     filter(GEOID == "24510271101")
)

(t271101_H1_sum_pc <- t271101_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for Baltimore City:
(bc_H1 <- get_decennial(
    geography = "county",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(bc_H1_sum_pc <- bc_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for Maryland:
(md_H1 <- get_decennial(
    geography = "state",
    state = "MD",
    table = "H1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(md_H1_sum_pc <- md_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc, by = "variable") %>%
     left_join(t271101_H1_sum_pc, by = "variable") %>%
     left_join(rw_H1_sum_pc, by = "variable")
)

H1_col_names <- c(
    md_c = "count.x",
    md_p = "percent.x",
    bc_c = "count.y",
    bc_p = "percent.y",
    t_c  = "count.x.x",
    t_p  = "percent.x.x",
    rw_c = "count.y.y",
    rw_p = "percent.y.y"
)

(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc, by = "variable") %>%
     left_join(t271101_H1_sum_pc, by = "variable") %>%
     left_join(rw_H1_sum_pc, by = "variable") %>%
     rename(all_of(H1_col_names)) %>%
     case_match
)

## Doesn't work correctly, because all columns have the same name
(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc) %>%
     left_join(t271101_H1_sum_pc) %>%
     left_join(rw_H1_sum_pc)
)

## 17 June
library(tidyverse)
library(tidycensus)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

(bc_H1 <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    summary_var = "H1_001N",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(rw_H1_sum_pc <- bc_H1 %>%
     mutate(NAME = NULL) %>%
     filter(substr(GEOID, 6, 11) == "271101" &
            substr(GEOID, 12, 15) %in% rw_block_list) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for census tract 2711.01:
(t271101_H1 <- get_decennial(
    geography = "tract",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    summary_var = "H1_001N",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1") %>%
     filter(GEOID == "24510271101")
)

(t271101_H1_sum_pc <- t271101_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for Baltimore City:
(bc_H1 <- get_decennial(
    geography = "county",
    state = "MD",
    county = "Baltimore city",
    table = "H1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(bc_H1_sum_pc <- bc_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

## Do the same thing for Maryland:
(md_H1 <- get_decennial(
    geography = "state",
    state = "MD",
    table = "H1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1")
)

(md_H1_sum_pc <- md_H1 %>%
     mutate(NAME = NULL) %>%
     group_by(variable) %>%
     summarize(count = sum(value)) %>%
     mutate(percent = 100 * count/count[variable == "H1_001N"])
)

(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc, by = "variable") %>%
     left_join(t271101_H1_sum_pc, by = "variable") %>%
     left_join(rw_H1_sum_pc, by = "variable")
)

H1_col_names <- c(
    md_c = "count.x",
    md_p = "percent.x",
    bc_c = "count.y",
    bc_p = "percent.y",
    t_c  = "count.x.x",
    t_p  = "percent.x.x",
    rw_c = "count.y.y",
    rw_p = "percent.y.y"
)

(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc, by = "variable") %>%
     left_join(t271101_H1_sum_pc, by = "variable") %>%
     left_join(rw_H1_sum_pc, by = "variable") %>%
     rename(all_of(H1_col_names)) %>%
     mutate(variable = as.factor(variable))
)

H1_tab$variable <- factor(H1_tab$variable,
                          labels = c("Total", "Occupied", "Vacant")
                          )

## Didn't work
levels(H1_tab$variable) <- c(
    "H1_001N", "Total",
    "H1-002N", "Occupied",
    "H1_003N", "Vacant")

## Not loaded. Save for if needed
## library(car)

## Not needed:
occ_vars <- c(
    Total = "H1_001N",
    Occupied = "H1_002N",
    Vacant = "H1_003N"
)

## Prepare table for output:
library(knitr)
library(kableExtra)

kable(H1_tab, format = "latex", booktabs = TRUE,
      col.names = c("Status", rep(c("n", "%"), times = 4)),
      caption = "Households by Occupancy Status",
      caption.short = "HH by Occupancy",
      digits = c(0,0,1,0,1,0,1,0,1)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_header_above(c(" " = 1,
                       "Maryland" = 2,
                       "Baltimore City" = 2,
                       "Tract 2711.01" = 2,
                       "Radnor-Winston" = 2))

## 19 June. Make table-fetches into functions to parametize them.
library(tidyverse)
library(tidycensus)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

table <- "H1"

get_rw <- function(table) {
    get_decennial(
        geography = "block",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = "2020",
        sumfile = "sf1") %>%
        mutate(NAME = NULL) %>%
        filter(substr(GEOID, 6, 11) == "271101" &
               substr(GEOID, 12, 15) %in% rw_block_list)
}

rw_H1 <- get_rw("H1")

sum_tab <- function(table) {
    table %>%
        group_by(variable) %>%
        summarize(count = sum(value)) %>%
        mutate(percent = 100 * count/count[variable == "H1_001N"])
}

rw_H1_sum_pc <- sum_tab(rw_H1)

get_ct <- function(table) {
    get_decennial(
        geography = "tract",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = "2020",
        sumfile = "sf1") %>%
        filter(GEOID == "24510271101")
}

t271101_H1 <- get_ct(table)

t271101_H1_sum_pc <- sum_tab(t271101_H1)

get_bc <- function(tabID) {
    get_decennial(
        geography = "county",
        state = "MD",
        county = "Baltimore city",
        table = tabID,
        cache_table = TRUE,
        year = "2020",
        sumfile = "sf1")
}

bc_H1 <- get_bc("H1")

bc_H1_sum_pc <- sum_tab(bc_H1)

get_md <- function(tabID) {
    get_decennial(
        geography = "state",
        state = "MD",
        table = tabID,
        cache_table = TRUE,
        year = "2020",
        sumfile = "sf1")
}

md_H1 <- get_md(table)

md_H1_sum_pc <- sum_tab(md_H1)

H1_col_names <- c(
    md_c = "count.x",
    md_p = "percent.x",
    bc_c = "count.y",
    bc_p = "percent.y",
    t_c  = "count.x.x",
    t_p  = "percent.x.x",
    rw_c = "count.y.y",
    rw_p = "percent.y.y"
)

(H1_tab <- md_H1_sum_pc %>%
     left_join(bc_H1_sum_pc, by = "variable") %>%
     left_join(t271101_H1_sum_pc, by = "variable") %>%
     left_join(rw_H1_sum_pc, by = "variable") %>%
     rename(all_of(H1_col_names)) %>%
     mutate(variable = as.factor(variable))
)

H1_tab$variable <- factor(H1_tab$variable,
                          labels = c("Total", "Occupied", "Vacant")
                          )

library(knitr)
library(kableExtra)

kable(H1_tab, format = "latex", booktabs = TRUE,
      col.names = c("Status", rep(c("n", "%"), times = 4)),
      caption = "Households by Occupancy Status",
      caption.short = "HH by Occupancy",
      digits = c(0,0,1,0,1,0,1,0,1)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_header_above(c(" " = 1,
                       "Maryland" = 2,
                       "Baltimore City" = 2,
                       "Tract 2711.01" = 2,
                       "Radnor-Winston" = 2))

## 20 June. Recreating the blocks map in RW:
rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012")

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Create a map of just the RW blocks:
rw_base_blocks <- read_osm(bb(rw_blocks, ext = 1.3))

tmap_mode("plot")

## Line below gives map in meters
(RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
## Line below gives map in degrees
## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders() +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     tm_grid() +
     tm_layout(title = "Radnor-Winston Neighborhood")
)

## Create the whole RW boundaries and census blocks

## Creating a polygon for RW neighborhood, based on CRS 6487 (NAD83
## (2011) / Maryland ) map in meters:
base_x <- 433000
base_y <- 186000
rw_neigh_pg_m <- data.frame(
    matrix(
        c(540, 1140,
          540, 1070,
          480, 1060,
          490, 1000,
          570, 1000,
          570, 940,
          550, 930,
          550, 890,
          580, 890,
          590, 820,
          640, 820,
          650, 590,
          520, 580,
          470, 580,
          350, 660,
          350, 710,
          180, 725,
          190, 900,
          220, 900,
          220, 1030,
          240, 1030,
          240, 1110
        ),
        ncol = 2, byrow = TRUE)
) %>% + matrix(c(rep(base_x, nrow(.)), rep(base_y, nrow(.))),
               nrow = nrow(.)) %>%
sf::st_as_sf(coords = c(1,2), dim = "XY") %>%
summarize(geometry = st_combine(geometry)) %>%
st_cast("POLYGON") %>%
st_set_crs(6487)

## Get the basemap:
rw_base_blocks <- read_osm(bb(rw_neigh_pg_m, ext = 1.3))

## Get the geometry for the US census blocks in RW:
rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011", "3012")

## Get the RW blocks from the census:
rw_blocks <- blocks(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(substr(GEOID20, 6, 11) == "271101" &
           substr(GEOID20, 12, 15) %in% rw_block_list)

## Line below gives map in meters
(RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     ## Line below gives map in degrees
     ## (RW_block_map <- tm_shape(rw_base_blocks, projection = 6487) +
     tm_rgb() +
     tm_shape(rw_neigh_pg_m) +
     ## tm_fill(col = "green", alpha = 0.2) +
     tm_borders(lwd = 4, alpha = 1) +
     tm_shape(rw_blocks) +
     tm_fill("MAP_COLORS", alpha = 0.2, palette = "Accent") +
     tm_borders(lwd = 2) +
     tm_text("BLOCKCE20", col="black") +
     tm_scale_bar() +
     ## tm_grid() + tm_xlab("Long") + tm_ylab("Lat") +
     ## tm_grid() +
     tm_layout(title = "US Census blocks and the Radnor-Winston Neighborhood")
)


## How to determine hex color values used. From
## https://stackoverflow.com/questions/29175314/display-hex-codes-for-colours-ggplot2-is-using

library(scales)

f <- brewer_pal(type = "Seq", palette="Accent")

f(8)

## [1] "#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17" "#666666"

## 21 Jun. Exploring Race in table P1
## Already loaded all libraries

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

tableID <- "P1"

## Loaded get_rw()
rw_P1 <- get_rw(tableID)

## Going back to retrieving individual variables:

race_vars <- c(
         "TotalPop" = "P1_001N",
         "OneRace" = "P1_002N",
         "TwoOrMoreRace" = "P1_009N",
         "TwoRace" = "P1_010N",
         "ThreeRace" = "P1_026N"
     )

## To get the labels for the rows, add an "A" to the end of the
## variable

rw_race <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    variables = race_vars,
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    mutate(percent = 100 * count/count[variable == "TotalPop"])

    mutate(variable = as.factor(variable))

rw_race$variable <- factor(rw_race$variable,
                           levels = c("TotalPop",
                                      "OneRace",
                                      "TwoOrMoreRace",
                                      "TwoRace",
                                      "ThreeRace")
                           )

arrange(rw_race, variable)

rw_race[order(rw_race$variable)]

## Simplified experiment

t <- tibble(var = c("Five", "Four", "Three", "Two", "One"),
            count = 5:1
            ) %>%
    mutate(var = as.factor(var))

t$var <- factor(t$var, labels = c("One", "Two", "Three", "Four", "Five"))

t
str(t)
dput(t)
levels(t$var)

## This works, but I don't understand why the previous ones didn't
arrange(t, var)

## 27 Jun

## Today's goal: download a whole table, with textual labels, and
## elimminate any rows with zero's for RW blocks.

## sf1-4 doesn't exist for 2020 decennial census:
## > load_variables(year="2020", dataset="sf1")
## Error in get_dataset(dataset, year) : 
##   API endpoint not found. Does this data set exist for the specified year? See https://api.census.gov/data.html for data availability.
## > load_variables(year="2020", dataset="sf2")
## Error in get_dataset(dataset, year) : 
##   API endpoint not found. Does this data set exist for the specified year? See https://api.census.gov/data.html for data availability.
## > load_variables(year="2020", dataset="sf3")
## Error: Summary File 3 was not released in 2010. Use tables from the American Community Survey via get_acs() instead.
## > load_variables(year="2020", dataset="sf4")
## Error in get_dataset(dataset, year) : 
##   API endpoint not found. Does this data set exist for the specified year? See https://api.census.gov/data.html for data availability.
## >

## Demographic profile:
## > load_variables(year="2020", dataset="dp")
## # A tibble: 320 × 3
##    name      label                                                  concept                                         
##    <chr>     <chr>                                                  <chr>                                           
##  1 DP1_0001C Count!!SEX AND AGE!!Total population                   PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  2 DP1_0001P Percent!!SEX AND AGE!!Total population                 PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  3 DP1_0002C Count!!SEX AND AGE!!Total population!!Under 5 years    PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  4 DP1_0002P Percent!!SEX AND AGE!!Total population!!Under 5 years  PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  5 DP1_0003C Count!!SEX AND AGE!!Total population!!5 to 9 years     PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  6 DP1_0003P Percent!!SEX AND AGE!!Total population!!5 to 9 years   PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  7 DP1_0004C Count!!SEX AND AGE!!Total population!!10 to 14 years   PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  8 DP1_0004P Percent!!SEX AND AGE!!Total population!!10 to 14 years PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
##  9 DP1_0005C Count!!SEX AND AGE!!Total population!!15 to 19 years   PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
## 10 DP1_0005P Percent!!SEX AND AGE!!Total population!!15 to 19 years PROFILE OF GENERAL POPULATION AND HOUSING CHARA…
## # ℹ 310 more rows
## # ℹ Use `print(n = ...)` to see more rows
## >

## Demographic and Housing Characteristics:
## > load_variables(year="2020", dataset="dhc")
## # A tibble: 9,067 × 3
##    name     label                                                                                            concept
##    <chr>    <chr>                                                                                            <chr>  
##  1 H10_001N " !!Total:"                                                                                      TENURE…
##  2 H10_002N " !!Total:!!Owner occupied:"                                                                     TENURE…
##  3 H10_003N " !!Total:!!Owner occupied:!!Householder who is White alone"                                     TENURE…
##  4 H10_004N " !!Total:!!Owner occupied:!!Householder who is Black or African American alone"                 TENURE…
##  5 H10_005N " !!Total:!!Owner occupied:!!Householder who is American Indian and Alaska Native alone"         TENURE…
##  6 H10_006N " !!Total:!!Owner occupied:!!Householder who is Asian alone"                                     TENURE…
##  7 H10_007N " !!Total:!!Owner occupied:!!Householder who is Native Hawaiian and Other Pacific Islander alon… TENURE…
##  8 H10_008N " !!Total:!!Owner occupied:!!Householder who is Some Other Race alone"                           TENURE…
##  9 H10_009N " !!Total:!!Owner occupied:!!Householder who is Two or More Races"                               TENURE…
## 10 H10_010N " !!Total:!!Renter occupied:"                                                                    TENURE…
## # ℹ 9,057 more rows
## # ℹ Use `print(n = ...)` to see more rows
## > 

## 28 June
## Download small decennial table and process it

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

rw_race <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "sf1") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    mutate(percent = 100 * count/count[variable == "Total"])

rw_race$variable <- factor(rw_race$variable,
                           levels = names(race_vars))

rw_race <- arrange(rw_race, variable)

pl_vars <- load_variables(year = "2020", dataset = "pl", cache = TRUE)

rw_race <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "pl") %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    left_join(pl_vars, by = c("variable" = "name")) %>%
    filter(count > 0)

str_split(rw_race$label, pattern = "!!")

## Stand alone example:

bg3_race <- get_decennial(
    geography = "block group",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "pl")%>%
    filter(substr(GEOID, 6, 11) == "271101")

pl_vars <- load_variables(year = "2020", dataset = "pl", cache = TRUE)

bg3_race_sum <- bg3_race %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    filter(count > 0) %>%
    .$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", " ") #Replace each !!.* with space

## From Ivan, https://stat.ethz.ch/pipermail/r-help/2023-June/477630.html
bg3_race_sum$label |>
    (\(.) sub('^ !!', '', .))() |>
                            (\(.) gsub('[^!]*!!', ' ', .))()

bg3_race_sum$label %>%
    (\(.) sub('^ !!', '', .))() %>%
                            (\(.) gsub('[^!]*!!', ' ', .))()


## Get a map of all the census blocks in block group 3:
bg3 <- block_groups(state = "MD",
                    county = "Baltimore city",
                    year = "2020") %>%
    filter(TRACTCE == "271101")

bg3_base <- read_osm(bb(bg3, ext=1.3))

(bg3_map <- tm_shape(bg3_base, projection = 6487) +
     tm_rgb() +
     tm_shape(bg3) +
     tm_fill(col = "green", alpha = 0.2)
)


rw_race <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "pl")%>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list)

pl_vars <- load_variables(year = "2020", dataset = "pl", cache = TRUE)

rw_race_sum <- rw_race %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    filter(count > 0)

rw_race_sum$label <- rw_race_sum$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    ## str_replace_all("[^!]*!!", " ") #Replace each !!.* with space
    str_replace_all("[^!]*!!", "\\\\hspace{1em}") #Replace each !!.* with space

rw_race_sum

(kable(rw_race_sum, format = "latex", booktabs = TRUE,
      col.names = c("", "Count", "%-age"),
      caption = "Racial identification of people in RW",
      ## label = "rw_race_2",
      digits = c(0,0,1)
      ) %>%
    row_spec(1, bold = TRUE)
)

rw_race <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    cache_table = TRUE,
    year = "2020",
    sumfile = "pl") %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% rw_block_list)

pl_vars <- load_variables(year = "2020", dataset = "pl", cache = TRUE)

rw_race_sum <- rw_race %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    group_by(variable) %>%
    summarize(count = sum(value)) %>%
    left_join(pl_vars, by=c("variable" = "name")) %>%
    filter(count > 0) %>%
    select(c(label, count)) %>%
    mutate(percent = 100 * count/count[1])

rw_race_sum$label <- rw_race_sum$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "-") #Replace each !!.* with space

(kable(rw_race_sum, format = "latex", booktabs = TRUE,
      col.names = c("", "Count", "%-age"),
      caption = "Racial identification of people in RW",
      ## label = "rw_race_2",
      digits = c(0,0,1)
      ) %>%
    row_spec(1, bold = TRUE)
)

## 27 Jul 2023 TODO: get another table, P16 Household types
library(tidyverse)
library(tidycensus)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

tableID <- "P16"
summary_var <- "P16_001N"

get_rw <- function(table, summary_var) {
    get_decennial(
        geography = "block",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(NAME = NULL) %>%
        filter(substr(GEOID, 6, 11) == "271101" &
               substr(GEOID, 12, 15) %in% rw_block_list) %>%
        group_by(variable) %>%
        summarize(RW_value = sum(value)) %>%
        mutate("RW_pc" = RW_value/first(RW_value) * 100) %>%
        relocate(RW_value, .after = RW_pc)
}

get_bc <- function(table, summary_var) {
    get_decennial(
        geography = "county",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "BC_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

get_md <- function(table, summary_var) {
    get_decennial(
        geography = "state",
        state = "MD",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "MD_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

get_us <- function(table, summary_var) {
    get_decennial(
        geography = "us",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "US_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

labels <- load_variables(2020, "dhc", cache = TRUE)

(rw_P16 <- get_rw(tableID, summary_var))

(bc_P16 <- get_bc(tableID, summary_var))

(md_P16 <- get_md(tableID, summary_var))

(us_P16 <- get_us(tableID, summary_var))


(p16_tab <- us_P16 %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_P16, by = "variable") %>%
     left_join(bc_P16, by = "variable") %>%
     left_join(rw_P16, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

## Stand-alone example
(p16_tab <- us_P16 %>%
     left_join(labels, by = c("variable" = "name")) %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

p16_tab$label <- p16_tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "\\\\hspace{1em}") #Replace each !!.* with space

p16_tab$label <- p16_tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(p16_tab, format = "latex", booktabs = TRUE,
      col.names = c("label", "United States %-age")
      )

kable(p16_tab, format = "latex", booktabs = TRUE,
      col.names = c("label", "United States %-age")
      ) %>%
    add_indent(c(2:9), level_of_indent = c(1,2,2,3,3,1,2,2))

kable(p16_tab, format = "latex", booktabs = TRUE,
      col.names = c("label", "United States %-age")
      ) %>%
    add_indent(c(2,7), level_of_indent = 1) %>%
    add_indent(c(3,4,8,9), level_of_indent = 2) %>%
    add_indent(c(5,6), level_of_indent = 3)


## 31 Jul 2023 Use kable to format table
library(knitr)
library(kableExtra)

p16_tab$label <- p16_tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(p16_tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Status",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Households by Type, (Decennial Census table P16)",
      caption.short = "HH by Type",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2,7), level_of_indent = 1) %>%
    add_indent(c(3,4,8,9), level_of_indent = 2) %>%
    add_indent(c(5,6), level_of_indent = 3)

## 1 Aug: Get table H10:
library(tidyverse)
library(tidycensus)

rw_block_list <- c("3000", "3001", "3002", "3005", "3006", "3007",
                   "3008", "3009", "3010", "3011")

tableID <- "H10"
summary_var <- "H10_001N"

get_rw <- function(table, summary_var) {
    get_decennial(
        geography = "block",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(NAME = NULL) %>%
        filter(substr(GEOID, 6, 11) == "271101" &
               substr(GEOID, 12, 15) %in% rw_block_list) %>%
        group_by(variable) %>%
        summarize(RW_value = sum(value)) %>%
        mutate("RW_pc" = RW_value/first(RW_value) * 100) %>%
        relocate(RW_value, .after = RW_pc)
}

get_bc <- function(table, summary_var) {
    get_decennial(
        geography = "county",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "BC_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

get_md <- function(table, summary_var) {
    get_decennial(
        geography = "state",
        state = "MD",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "MD_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

get_us <- function(table, summary_var) {
    get_decennial(
        geography = "us",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        summary_var = summary_var) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "US_pc" = value / summary_value * 100,
               value = NULL,
               summary_value = NULL)
}

labels <- load_variables(2020, "dhc", cache = TRUE)

(rw_H10 <- get_rw(tableID, summary_var))

(bc_H10 <- get_bc(tableID, summary_var))

(md_H10 <- get_md(tableID, summary_var))

(us_H10 <- get_us(tableID, summary_var))


(h10_tab <- us_H10 %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_H10, by = "variable") %>%
     left_join(bc_H10, by = "variable") %>%
     left_join(rw_H10, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

h10_tab$label <- h10_tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

library(knitr)
library(kableExtra)

kable(h10_tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Tenure by Race",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Tenure by RAce, (Decennial Census table H10)",
      caption.short = "HT by Race",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2,10), level_of_indent = 1) %>%
    add_indent(c(3:9,11:17), level_of_indent = 2)

## TODO: Process more tables in RW_Census.Rnw.
## 2 Aug: Table H13
tableID <- "H13"
summary_var <- "H13_001N"

(rw_H13 <- get_rw(tableID, summary_var))
(bc_H13 <- get_bc(tableID, summary_var))
(md_H13 <- get_md(tableID, summary_var))
(us_H13 <- get_us(tableID, summary_var))

(h13_tab <- us_H13 %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_H13, by = "variable") %>%
     left_join(bc_H13, by = "variable") %>%
     left_join(rw_H13, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

h13_tab$label <- h13_tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(h13_tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Tenure by Race",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Tenure by Age of Householder, (Decennial Census table H13)",
      caption.short = "HT by Age",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2,12), level_of_indent = 1) %>%
    add_indent(c(3:11,13:21), level_of_indent = 2)

## Table H15 HH Tenure by presence of children
tableID <- "H15"
summary_var <- "H15_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Tenure by Children",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Tenure by Presence of People under 18 Years (Decennial Census table H15)",
      caption.short = "HT by Children",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2,5), level_of_indent = 1) %>%
    add_indent(c(3, 4, 6, 7), level_of_indent = 2)

## Table H4 Tenure by ownership
tableID <- "H4"
summary_var <- "H4_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Tenure by Children",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Tenure by Presence of People under 18 Years (Decennial Census table H15)",
      caption.short = "HT by Children",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2:4), level_of_indent = 1)

## Table H6 Race of Householder
tableID <- "H6"
summary_var <- "H6_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Tenure by Children",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Tenure by Presence of People under 18 Years (Decennial Census table H15)",
      caption.short = "HT by Children",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2:8), level_of_indent = 1)

## Table H9 Household size
tableID <- "H9"
summary_var <- "H9_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Sex by Age",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Sex by Age (Decennial Census table P12)",
      caption.short = "Sex by Age",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2:8), level_of_indent = 1) 

## Table P12 Sex by AGe
tableID <- "P12"
summary_var <- "P12_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Size",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Size (Decennial Census table H9)",
      caption.short = "HH Size",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2, 26), level_of_indent = 1) %>%
    add_indent(c(3:25, 27:49), level_of_indent = 2)

## Table P13 Median Age

## NOTE: This type of table doesn't work with standard setup. Have to
## compute median age for all RW blocks.

tableID <- "P13"
summary_var <- "P12_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Size",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Size (Decennial Census table H9)",
      caption.short = "HH Size",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2, 26), level_of_indent = 1) %>%
    add_indent(c(3:25, 27:49), level_of_indent = 2)

## Table P14 Sex by Age under 20
tableID <- "P14"
summary_var <- "P14_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Sex by Age (under 20)",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Sex by Age under 20 (Decennial Census table P14)",
      caption.short = "Sex by Age <20",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2, 23), level_of_indent = 1) %>%
    add_indent(c(3:22, 24:43), level_of_indent = 2)

## Table P16 Household Type
tableID <- "P16"
summary_var <- "P16_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Types",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Types (Decennial Census table P16)",
      caption.short = "HH Types",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2, 7), level_of_indent = 1) %>%
    add_indent(c(3, 4, 8, 9), level_of_indent = 2) %>%
    add_indent(c(5, 6), level_of_indent = 3)

## Table P17 Household Type by Relationship
tableID <- "P17"
summary_var <- "P17_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

kable(tab, format = "latex", booktabs = TRUE,
      col.names = c("Household Type by Relationship",
                    "United States %-age",
                    "Maryland %-age",
                    "Baltimore City %-age",
                    "RW %-age",
                    "RW Count"),
      caption = "Household Type by Relationship (Decennial Census table P17)",
      caption.short = "HH Types",
      digits = c(0,1,1,1,1,0)
      ) %>%
    row_spec(1, bold = TRUE) %>%
    add_indent(c(2, 29), level_of_indent = 1) %>%
    add_indent(c(3, 10:14, 16, 18, 20, 22:28, 30, 31), level_of_indent = 2) %>%
    add_indent(c(4, 7, 15, 17, 19, 21), level_of_indent = 3) %>%
    add_indent(c(5, 6, 8, 9), level_of_indent = 4)
## 7 Aug: last table P17
## Example of calculating median age of population in census blocks.
library(tidyverse)
library(tidycensus)

counts <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P1",
    year = 2020,
    sumfile = "dhc") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% c(3000, 3001, 3002)
           )

labels <- load_variables(2020, "dhc")

tab <- counts %>%
    left_join(labels, by = c("variable" = "name")) %>%
    mutate(variable = NULL, concept = NULL) %>%
    relocate(label)

ages <- get_decennial(
    geography = "block",
    state = "MD",
    county = "Baltimore city",
    table = "P13",
    year = 2020,
    sumfile = "dhc") %>%
    mutate(NAME = NULL) %>%
    filter(substr(GEOID, 6, 11) == "271101" &
           substr(GEOID, 12, 15) %in% c(3000, 3001, 3002)
           )

## 8 Aug Produce a 'population pyramid' for the age groups in table P12
tableID <- "P12"
summary_var <- "P12_001N"

(rw_tab <- get_rw(tableID, summary_var))
(bc_tab <- get_bc(tableID, summary_var))
(md_tab <- get_md(tableID, summary_var))
(us_tab <- get_us(tableID, summary_var))

(tab <- us_tab %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_tab, by = "variable") %>%
     left_join(bc_tab, by = "variable") %>%
     left_join(rw_tab, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

tab$label <- tab$label %>%
    str_replace("^ !!", "") %>% #Drop the leading ' !!'
    str_replace_all("[^!]*!!", "") #Replace each !!.* with nothing

## Can't use the standard get_xx(). Need populations, and regular
## age-group breaks:
get_rw_pop <- function(table) {
    get_decennial(
        geography = "block",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc") %>%
        mutate(NAME = NULL) %>%
        filter(substr(GEOID, 6, 11) == "271101" &
               substr(GEOID, 12, 15) %in% rw_block_list) %>%
        group_by(variable) %>%
        summarize(RW_pop = sum(value))
}

get_bc_pop <- function(table) {
    get_decennial(
        geography = "county",
        state = "MD",
        county = "Baltimore city",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        ) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "BC_pop" = value,
               value = NULL,
               )
}

get_md_pop <- function(table) {
    get_decennial(
        geography = "state",
        state = "MD",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        ) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "MD_pop" = value,
               value = NULL,
               )
}

get_us_pop <- function(table) {
    get_decennial(
        geography = "us",
        table = table,
        cache_table = TRUE,
        year = 2020,
        sumfile = "dhc",
        ) %>%
        mutate(GEOID = NULL,
               NAME = NULL,
               "US_pop" = value,
               value = NULL,
               )
}

table <- "P12"
rw_pop <- get_rw_pop(table)
bc_pop <- get_bc_pop(table)
md_pop <- get_md_pop(table)
us_pop <- get_us_pop(table)

(pop_tab <- us_pop %>%
     left_join(labels, by = c("variable" = "name")) %>%
     left_join(md_pop, by = "variable") %>%
     left_join(bc_pop, by = "variable") %>%
     left_join(rw_pop, by = "variable") %>%
     mutate(variable = NULL, concept = NULL) %>%
     relocate(label)
)

## 11 Aug: Need to consolidate lines in table P12, to give equal,
## 5-year ranges

consolidate <- function(table) {
    newtable <- table[0, ] #Duplicate table with zero rows
    i <- 1 #Index to row number
    while (i <= nrow(table)) {
        if (i==6) {
            tmp <- tibble(
                label = " !!Total:!!Male:!!15 to 19 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==6
        if (i==8) {
            tmp <- tibble(
                label = " !!Total:!!Male:!!20 to 24 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop + table[i+2, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop + table[i+2, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop + table[i+2, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop + table[i+2, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 3
            next
        } #endif i==8
        if (i==18) {
            tmp <- tibble(
                label = " !!Total:!!Male:!!60 to 64 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==18
        if (i==20) {
            tmp <- tibble(
                label = " !!Total:!!Male:!!65 to 69 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==20
        if (i==30) {
            tmp <- tibble(
                label = " !!Total:!!Female:!!15 to 19 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==30
        if (i==32) {
            tmp <- tibble(
                label = " !!Total:!!Female:!!20 to 24 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop + table[i+2, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop + table[i+2, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop + table[i+2, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop + table[i+2, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 3
            next
        } #endif i==30
        if (i==42) {
            tmp <- tibble(
                label = " !!Total:!!Female:!!60 to 64 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==42
        if (i==44) {
            tmp <- tibble(
                label = " !!Total:!!Female:!!65 to 69 years",
                US_pop = table[i, ]$US_pop + table[i+1, ]$US_pop,
                MD_pop = table[i, ]$MD_pop + table[i+1, ]$MD_pop,
                BC_pop = table[i, ]$BC_pop + table[i+1, ]$BC_pop,
                RW_pop = table[i, ]$RW_pop + table[i+1, ]$RW_pop
            )
            ## print(tmp)
            newtable <- rbind(newtable, tmp)
            i <- i + 2
            next
        } #endif i==44
        newtable <- rbind(newtable, table[i, ])
        i <- i+1
    }
    newtable
}

              
            
