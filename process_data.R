## This script queries American Community Survey data for a user-defined metro area and
## produces a table with pre-selected vulnerability indicators. You can change the metro
## area to your own community to download the data and export it for use in the shiny app.
## This script assumes you will download regional data and also a "core" city for that 
## region. Make sure you adjust the `primary_place` variable if you seek an alternate city
## within a region.

## Source script that loads libraries, functions and data
source("global.R")

routes <- st_read("./data/shapes/Fall21RouteShape.shp") %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84') %>%
  rmapshaper::ms_simplify(.)

mapview(routes)


##### 01. Determine and input target region's FIPS/CBSA codes and places #####

## TODO: Enter a region to look up in the line below to search for the correct CBSA FIPS 
"oakland" -> REGION_OF_INTEREST ; county2msa %>% 
  filter(str_detect(cbsaname15, regex(REGION_OF_INTEREST, ignore_case = TRUE)))

## TODO: Enter the correct CBSA FIPS code found from lookup above to determine the counties that are part of that CBSA
counties2download <- get_counties_from_cbsa(41860)

## Determine states from counties
states2download <- get_states_from_stcnty_fips(counties2download)

## Main places in CBSA
places_in_cbsa <- tigris::places(states2download) %>%
  st_filter(., filter(tigris::counties(state = states2download, cb = T), GEOID %in% counties2download)) %>%
  st_transform(crs = '+proj=longlat +datum=WGS84') %>%
  arrange(desc(ALAND)) 

head(places_in_cbsa, n = 10) 

counties_in_cbsa <- tigris::counties(states2download) %>%
  filter(GEOID %in% counties2download) %>%
  st_transform(crs = '+proj=longlat +datum=WGS84') %>%
  arrange(desc(ALAND)) 

## Grab GEOID/FIPS code of the primary place of interest, which is USUALLY the largest
## by land area. In this case, Oakland is the third largest
primary_place <-  places_in_cbsa[3,]


##### 02. Download data using tidycensus #####
## View potential variables available to use
acs19 <- load_variables(2019, "acs5", cache = TRUE)
s19 <- load_variables(2019, "acs5/subject", cache = TRUE)


tables2grab <- c('B01001', 'B15002', 'B25014', 'C16002', 'B23025', 'B08301', 'B14001', 
                 'B28002', 'B25044', 'B25091', 'B25070', 'B18101')

tables2grab_slim <- c('B03002', 'B01001', 'B18101', 'B23025', 'C17002', 'B08203', 'B15002', 'B08301')

## Race (white, BIPOC) # B03002
## Age (18-34, 35-64, 65+) # B01001
## Disability (disabled, not disabled) # B18101
## Poverty (<1, >1) # C17002
## Vehicles available (0, 1, 2+) # B08203
## Employment status (employed, unemployed) # B23025
## Educational attainment (<HS, HS+) # B15002
### Others to grab:
## Commute mode (transit, bus) # B08301



## Grab relevant tables
query <- purrr::map_df(tables2grab_slim, function(acs_table){
  data <- get_acs(geography = "tract",
                  table = acs_table,
                  state = states2download,
                  survey = "acs5",
                  year = 2019,
                  cache_table = TRUE)
})

tracts.sf <- tigris::tracts(state = states2download, cb = TRUE, year = 2019)


## Recode variables
acs_data <- query %>%
  select(-moe) %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = estimate) %>%
  mutate(tot_pop = B03002_001,
         tot_whitenh = B03002_003,
         tot_bipoc = tot_pop - tot_whitenh,
         tot_youth_15t21 = B01001_003 + B01001_007 + B01001_008 + B01001_009 + B01001_030 +
           B01001_031 + B01001_032 + B01001_033,
         tot_elder_65 = B01001_020 + B01001_021 + B01001_022 + B01001_023 + B01001_024 + B01001_025 + 
           B01001_044 + B01001_045 + B01001_046 + B01001_047 + B01001_048 + B01001_049,
         tot_age_dependent = tot_youth_15t21 + tot_elder_65,
         tot_disability = B18101_004 + B18101_007 + B18101_010 + B18101_013 + B18101_016 + B18101_019 + 
           B18101_023 + B18101_026 + B18101_029 + B18101_032 + B18101_035 + B18101_038,
         tot_poverty = C17002_002 + C17002_003 + C17002_004 + C17002_005 + C17002_006 + C17002_007,
         tot_vehicles_0 = B08203_002,
         tot_vehicles_1 = B08203_003,
         tot_vehicles_2up = B08203_004 + B08203_005 + B08203_006,
         tot_vehicles_shortage = B08203_014 + B08203_020 + B08203_021 + B08203_026 + B08203_027 + B08203_028,
         tot_unemployed = B23025_005,
         tot_less_hs = B15002_004 + B15002_005 + B15002_006 + B15002_007 + B15002_008 + B15002_009 +
           B15002_010 + B15002_011 + B15002_020 + B15002_021 + B15002_022 + B15002_023 + B15002_024 +
           B15002_025 + B15002_026 + B15002_027 + B15002_028,
         tot_more_hs = B15002_001 - tot_less_hs,
         
         pct_bipoc = tot_bipoc / tot_pop,
         pct_youth_15t21 =  tot_youth_15t21 / tot_pop,
         pct_elder_65 = tot_elder_65 / tot_pop,
         pct_age_dependent = tot_age_dependent / tot_pop,
         pct_disability = tot_disability / B18101_001,
         pct_poverty = tot_poverty / C17002_001,
         pct_vehicles_0 = tot_vehicles_0 / B08203_001,
         pct_vehicles_1 = tot_vehicles_1 / B08203_001,
         pct_vehicles_2up = tot_vehicles_2up / B08203_001,
         pct_vehicles_shortage = tot_vehicles_shortage / B08203_001,
         pct_unemployed = tot_unemployed / B23025_001,
         pct_less_hs = tot_less_hs / B15002_001,
         pct_more_hs = tot_more_hs / B15002_001,
         
         state = substr(GEOID, 1, 2)) %>%
  filter(substr(GEOID, 1, 5) %in% counties2download) %>% ## Filter for those tracts in the region
  left_join(., tracts.sf, by = "GEOID") %>% 
  st_as_sf() %>% st_transform(crs = '+proj=longlat +datum=WGS84') %>%
  select(GEOID, NAME = NAME.x, tot_pop:pct_less_hs)
  

## Identify a list of tracts that are within the primary place of interest
## Adjust negative distance buffer to fit desired tolerance
tracts_in_place <- acs_data %>%
  st_filter(primary_place) %>%
  pull(GEOID)

tracts_in_district_counties <- acs_data %>%
  filter(substr(GEOID, 1,5) %in% c("06001", "06013")) %>%
  pull(GEOID)

tracts_in_district <- acs_data %>%
  st_filter(., st_buffer(routes, 0.002)) %>% 
  pull(GEOID)

## Create new variable on whether that tract is in the primary place of interest
acs_data <- acs_data %>%
  mutate(in_primary_place = GEOID %in% tracts_in_place,
         in_transit_district = GEOID %in% tracts_in_district,
         in_east_bay = GEOID %in% tracts_in_district_counties)


##### 03. Export data #####
saveRDS(acs_data, "acs_transit_dependency_weighting_data.rds")

## Optionally view map
# mapview::mapview(acs_data, zcol = "in_primary_place") +
#   mapview::mapview(primary_place)

