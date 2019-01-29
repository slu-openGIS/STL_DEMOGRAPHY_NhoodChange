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

# open neighborhood data
st_read("data/STL_BOUNDARY_Nhoods/STL_BOUNDARY_Nhoods.shp", stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  filter(NHD_NUM <= 79) %>%
  select(NHD_NUM, NHD_NAME) -> nhoods

# interpolate 1950 data
nhoods %>%
  aw_interpolate(tid = NHD_NUM, source = stl50, sid = TRACTID, 
                 weight = "sum", output = "sf", extensive = "pop50") -> nhoods50

# interpolate 2017 data
nhoods %>%
  aw_interpolate(tid = NHD_NUM, source = stl17, sid = GEOID, 
                 weight = "sum", output = "tibble", extensive = "pop17") %>%
  select(NHD_NUM, pop17) -> nhoods17

# join
left_join(nhoods50, nhoods17, by = "NHD_NUM") %>%
  mutate(popChange = ((pop17-pop50)/pop50)*100) -> nhoodPop

# calculate area
nhoodPop %>%
  mutate(AREA = as.numeric(st_area(geometry))) -> nhoodPop

# export data
st_write(nhoodPop, "data/STL_DEMOGRAPHICS_Nhoods/STL_DEMOGRAPHICS_Nhoods.shp", delete_dsn = TRUE)

# calculate breaks
## calculate breaks manually
nhoodPop %>%
  mutate(breaks = case_when(popChange > 25 ~ "1",
                            popChange > 0 & popChange <= 25 ~ "2",
                            popChange >= -25 & popChange <= 0 ~ "3",
                            popChange < -25 & popChange >= -50 ~ "4",
                            popChange < -50 & popChange >= -75 ~ "5",
                            popChange < -75 ~ "6")) -> nhoodPop

cols <- c("1" = "#92C5DE", "2" = "#D1E5F0", "3" = "#FDDBC7", "4" = "#F4A582", "5" = "#D6604D", "6" = "#B2182B")

## load other data for mapping
city <- st_read("data/STL_BOUNDARY_City/STL_BOUNDARY_City.shp", stringsAsFactors = FALSE)
highway <- st_read("data/STL_TRANS_PrimaryRoads/STL_TRANS_PrimaryRoads.shp", stringsAsFactors = FALSE)

# create base map
base <- ggplot() + 
  geom_sf(data = nhoodPop, mapping = aes(fill = breaks), color = "#000000") + 
  geom_sf(data = highway, mapping = aes(color = "Highways"), size = 1.5, fill = NA) +
  geom_sf(data = city, fill = NA, color = "#000000", size = .25) +
  scale_fill_manual(values = cols, name = "Population Change",
                    labels = c("> 25%", "0% to 25%", "0% to -25%", "-25% to -50%", "-50% to -75%", "< -75%")) +
  scale_colour_manual(name="", values= "black") +
  labs(
    title = "Population Change, 1950-2017",
    subtitle = "St. Louis City Neighborhoods",
    caption = "Data via U.S. Census Bureau / IPUMS / Social Explorer \nMap by Christopher Prener, Ph.D."
  ) 

## map 1 - ggplot2 theme
map01 <- base + 
  theme_gray(base_size = 24) + 
  theme(plot.caption = element_text(hjust = "0"))

cp_plotSave(filename = "results/from1950/2017/nhoodMap-base.png", plot = map01, preset = "lg", dpi = 500)

## map 2 - sequoia theme with white background
map02 <- base + 
  cp_sequoiaTheme(background = "white", base_size = 24, map = TRUE)

cp_plotSave(filename = "results/from1950/2017/nhoodMap-white.png", plot = map02, preset = "lg", dpi = 500)


## map 3 - sequoia theme with transparent background
map03 <- base + 
  cp_sequoiaTheme(background = "transparent", base_size = 24, map = TRUE)

cp_plotSave(filename = "results/from1950/2017/nhoodMap-trans.png", plot = map03, preset = "lg", dpi = 500)
