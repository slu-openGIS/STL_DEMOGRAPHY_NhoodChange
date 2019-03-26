# Create Neighborhood Change Maps

# dependencies
library(areal)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(tidycensus)
library(prener)

# download 2017 ACS data
get_acs(geography = "tract", year = 2017, variables = "B01003_001", state = 29, county = 510, geometry = TRUE) %>%
  select(GEOID, estimate, moe) %>%
  rename(pop17 = estimate,
         pop17_m = moe)  %>%
  st_transform(crs = 26915) -> stl17

# open 1950 census data
st_read("data/STL_DEMOGRAPHICS_tracts50/STL_DEMOGRAPHICS_tracts50.shp", stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) -> stl50

read_csv("data/STL_DEMOGRAPHICS_pop50/STL_DEMOGRAPHICS_pop50.csv") %>%
  select(Geo_Name, SE_T001_001) %>%
  rename(
    TRACTID = Geo_Name, 
    pop50 = SE_T001_001) %>%
  left_join(stl50, ., by = "TRACTID") -> stl50

# interpolate 1950 data
stl17 %>%
  aw_interpolate(tid = GEOID, source = stl50, sid = TRACTID, 
                 weight = "sum", output = "sf", extensive = "pop50") %>%
  mutate(popChange = ((pop17-pop50)/pop50)*100) %>%
  mutate(breaks = case_when(popChange > 0 ~ "1",
                            popChange >= -25 & popChange <= 0 ~ "2",
                            popChange < -25 & popChange >= -50 ~ "3",
                            popChange < -50 & popChange >= -75 ~ "4",
                            popChange < -75 ~ "5")) -> tractsOverTime

cols <- c("1" = "#92C5DE", "2" = "#FDDBC7", "3" = "#F4A582", "4" = "#D6604D", "5" = "#B2182B")

## load other data for mapping
city <- st_read("data/STL_BOUNDARY_City/STL_BOUNDARY_City.shp", stringsAsFactors = FALSE)
highway <- st_read("data/STL_TRANS_PrimaryRoads/STL_TRANS_PrimaryRoads.shp", stringsAsFactors = FALSE)

# create base map
base <- ggplot() + 
  geom_sf(data = tractsOverTime, mapping = aes(fill = breaks), color = "#000000") + 
  geom_sf(data = highway, mapping = aes(color = "Highways"), size = 1.5, fill = NA) +
  scale_fill_manual(values = cols, name = "Population Change",
                    labels = c("> 0%", "0% to -25%", "-25% to -50%", "-50% to -75%", "< -75%")) +
  scale_colour_manual(name="", values= "black") +
  labs(
    title = "Population Change, 1950-2017",
    subtitle = "2010 Census Tracts"
  ) 

## map 3 - sequoia theme with transparent background
map03 <- base + 
  cp_sequoiaTheme(background = "transparent", base_size = 24, map = TRUE)

cp_plotSave(filename = "results/from1950/2017/tracts-trans.png", plot = map03, preset = "lg", dpi = 500)
