## set your working directory for export and import

setwd("/Users/ahourazandiatashbar/Documents/GEOG 171/W9")

## Package setup

pkgs <- c("tidyverse", "tidycensus", "spdep", "mapview", 
          "leafsync", "tmap", "tmaptools", "remotes", "sf", "shiny", "shinyjs")

install.packages(pkgs)
remotes::install_github("walkerke/crsuggest")

## Set your Census API key if need be
library(tidycensus)
options(tigris_use_cache = TRUE)
census_api_key("CENSUS API KEY")
readRenviron("~/.Renviron")

## ----basic-usage---------------------------------------------------------------------------------------
library(tigris)

ca_counties <- counties(state = "ca")

ca_counties


## ----basic-plot----------------------------------------------------------------------------------------
plot(ca_counties$geometry)


## ----Los Angeles-tracts-------------------------------------------------------------------------------------
la_tracts <- tracts(state = "CA", county = "Los Angeles")
la_tracts
plot(la_tracts$geometry)


## ----Los Angeles-roads--------------------------------------------------------------------------------------
la_roads <- roads(state = "CA", county = "Los Angeles")

plot(la_roads$geometry)


## ----washingotn state-tiger------------------------------------------------------------------------------------
wa_counties <- counties("WA")

plot(wa_counties$geometry)


## ----washingotn state-cb---------------------------------------------------------------------------------------
wa_counties_cb <- counties("WA", cb = TRUE)

plot(wa_counties_cb$geometry)


## ----get-yearly-data-----------------------------------------------------------------------------------
alameda90 <- suppressMessages(tracts("CA", "Alameda", cb = TRUE, year = 1990))
alameda00 <- suppressMessages(tracts("CA", "Alameda", cb = TRUE, year = 2000))
alameda10 <- tracts("CA", "Alameda", cb = TRUE, year = 2010)
# Cartographic boundary files not yet released for 2020
alameda20 <- tracts("CA", "Alameda", year = 2020)



## ----plot-yearly-data----------------------------------------------------------------------------------
par(mfrow = c(2, 2))

plot(alameda90$geometry, main = "1990")
plot(alameda00$geometry, main = "2000")
plot(alameda10$geometry, main = "2010")
plot(alameda20$geometry, main = "2020")




## ----mapview, eval = FALSE-----------------------------------------------------------------------------
library(mapview)

mapview(alameda20)


## ----sync, eval = FALSE--------------------------------------------------------------------------------
library(leafsync)

sync(mapview(alameda90), mapview(alameda20))



## ----combine-counties---------------------------------------------------------------------------------------
library(tidyverse)

state_codes <- c(state.abb, "DC")

us_county <- map_df(state_codes, ~counties(state = .x, cb = TRUE))

glimpse(us_county)

plot(us_county$geometry)


## ----tidycensus-load-variable-------------------------------------------------------------------------------

vars <- load_variables(2019, "acs5")
View (vars)

## ----tidycensus-geometry-------------------------------------------------------------------------------
library(tidycensus)
options(tigris_use_cache = TRUE)


ca_income1 <- get_acs(geography = "county", 
                      variables = c(hhincome = "B19013_001"), 
                      state = "CA")

ca_income <- get_acs(geography = "county", 
                     variables = c(hhincome = "B19013_001"), 
                     state = "CA", 
                     geometry = TRUE)

## ----show-geometry-------------------------------------------------------------------------------------
ca_income


## ----plot-geometry-------------------------------------------------------------------------------------
plot(ca_income["estimate"])



## ----geom-sf-------------------------------------------------------------------------------------------
library(tidyverse)

ca_map <- ggplot(ca_income, aes(fill = estimate)) + 
  geom_sf()


## ----plot-geom-sf--------------------------------------------------------------------------------------
ca_map


## ----get-santa-clara-data---------------------------------------------------------------------------------

sc_race <- get_acs(
  geography = "tract",
  state = "CA",
  county = "Santa Clara",
  variables = c(White = "B03002_003",
                Black = "B03002_004",
                Native = "B03002_005",
                Asian = "B03002_006",
                Hispanic = "B03002_012"),
  summary_var = "B03002_001",
  geometry = TRUE
) %>%
  mutate(percent = 100 * (estimate / summary_est))


## ----glimpse-sc-data-----------------------------------------------------------------------------
glimpse(sc_race)


## ----polygons-map, echo = FALSE------------------------------------------------------------------------
library(tmap)

sc_black <- filter(sc_race, 
                         variable == "Black")

tm_shape(sc_black) + 
  tm_polygons() 


## ----choropleth-show, echo = FALSE---------------------------------------------------------------------
tm_shape(sc_black) + 
  tm_polygons(col = "percent")


## ----custom-choropleth-show, echo = FALSE--------------------------------------------------------------
tm_shape(sc_black, 
         projection = sf::st_crs(26915)) + 
  tm_polygons(col = "percent",
          style = "quantile",
          n = 5,
          palette = "Purples",
          title = "ACS estimate") + 
  tm_layout(title = "Percent Black Quant Class\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)


## ----jenks-show, echo = FALSE--------------------------------------------------------------------------
tm_shape(sc_black, 
         projection = sf::st_crs(26915)) + 
  tm_polygons(col = "percent",
          style = "jenks",
          n = 5,
          palette = "viridis",
          title = "ACS estimate",
          legend.hist = TRUE) + 
  tm_layout(title = "Percent Black population Jenks Class\nby Census tract",
            frame = FALSE,
            legend.outside = TRUE)


## ----Exploring modification options--------------------------------------------------------------------------
library (shiny)
library (shinyjs)

tmaptools::palette_explorer()
tm_layout(legend.hist = TRUE)


## ----bubbles-code--------------------------------------------------------------------------------------
library(tmap)

symbol_map <- tm_shape(sc_black) + 
  tm_polygons() + 
  tm_bubbles(size = "percent", alpha = 0.5, 
             col = "navy")


## ----bubbles-map---------------------------------------------------------------------------------------
symbol_map


## ----facet-map-code------------------------------------------------------------------------------------
facet_map <- tm_shape(sc_race,
         projection = sf::st_crs(26915)) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 7,
          palette = "Blues")


## ----facet-map-----------------------------------------------------------------------------------------
facet_map

## ----shift-geo-----------------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)

us_median_age <- get_acs(geography = "state",
                         variables = "B01002_001",
                         year = 2019,
                         survey = "acs1",
                         geometry = TRUE,
                         shift_geo = TRUE)

## ----show-shift-geo, fig.width = 8---------------------------------------------------------------------
tm_shape(us_median_age) + tm_polygons()


## ----style-shift-geo, fig.width = 10-------------------------------------------------------------------
tm_shape(us_median_age) + 
  tm_polygons(col = "estimate", palette = "RdPu", title = "Median age") + 
  tm_layout(legend.outside = TRUE)


## ---- eval = FALSE-------------------------------------------------------------------------------------
library(mapview)

mapview(ca_income, zcol = "estimate")

m1 <- mapview(ca_income, zcol = "estimate")


## ----write-shp, eval = FALSE---------------------------------------------------------------------------
library(sf)

st_write(ca_income, "data/ca_income_1.shp")




