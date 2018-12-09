# Create Neighborhood Change Maps

# dependencies
library(areal)
library(dplyr)
library(ggplot2)
library(sf)
library(tidycensus)
library(prener)

# download 2010 census data
get_decennial(geography = "tract", variables = "P001001", state = 29, county = 510, geometry = TRUE) %>% 
  select(GEOID, NAME, value) %>%
  rename(pop10 = value) -> stl10

# download 2017 ACS data
get_acs(geography = "tract", year = 2017, variables = "B01003_001", state = 29, county = 510) %>%
  select(GEOID, estimate, moe) %>%
  rename(pop17 = estimate,
         pop17_m = moe) -> stl17

# merge and transform projection
left_join(stl10, stl17, by = "GEOID") %>%
  st_transform(crs = 26915) -> stlPop

# interpolate to neighborhood boundaries and calculate population change
st_read("data/BND_Nhd88_cw.shp", stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  select(NHD_NUM, NHD_NAME) %>%
  aw_interpolate(tid = NHD_NUM, source = stlPop, sid = GEOID, output = "sf", "pop10", "pop17") %>%
  mutate(popChange = ((pop17-pop10)/pop10)*100) -> nhoodPop

## load other data for mapping
city <- st_read("data/STL_BOUNDARY_City.shp", stringsAsFactors = FALSE)
highway <- st_read("data/STL_TRANS_PrimaryRoads.shp", stringsAsFactors = FALSE)

## calculate breaks manually
nhoodPop %>%
  mutate(breaks = case_when(popChange > 10 ~ "1",
    popChange > 3 & popChange <= 10 ~ "2",
    popChange >= -3 & popChange <= 3 ~ "3",
    popChange < -3 & popChange >= -11 ~ "4",
    popChange < -11 & popChange >= -26 ~ "5",
    popChange < -26 ~ "6")) -> nhoodPop

cols <- c("1" = "#67A9CF", "2" = "#D1E5F0", "3" = "#F7F7F7", "4" = "#FDDBC7", "5" = "#EF8A62", "6" = "#B2182B")

# create base map
base <- ggplot() + 
  geom_sf(data = nhoodPop, mapping = aes(fill = breaks), color = NA) + 
  geom_sf(data = highway, mapping = aes(color = "Highways"), size = 1.5, fill = NA) +
  geom_sf(data = city, fill = NA, color = "#000000", size = .25) +
  scale_fill_manual(values = cols, name = "Population Change",
                    labels = c("10% to 15%", "3% to 10%", "+/- 3%", "-3% to -11%", "-11% to -26%", "-26% to -31%")) +
  scale_colour_manual(name="", values= "black") +
  labs(
    title = "Population Change, 2010-2017",
    subtitle = "St. Louis City Neighborhoods",
    caption = "Data via U.S. Census Bureau \nMap by Christopher Prener, Ph.D."
  ) 

## map 1 - ggplot2 theme
map01 <- base + 
  theme_gray(base_size = 24) + 
  theme(plot.caption = element_text(hjust = "0"))

cp_plotSave(filename = "results/2017/nhoodMap-base.png", plot = map01, preset = "lg", dpi = 500)

## map 2 - sequoia theme with white background
map02 <- base + 
  cp_sequoiaTheme(background = "white", base_size = 24, map = TRUE)

cp_plotSave(filename = "results/2017/nhoodMap-white.png", plot = map02, preset = "lg", dpi = 500)


## map 3 - sequoia theme with transparent background
map03 <- base + 
  cp_sequoiaTheme(background = "transparent", base_size = 24, map = TRUE)

cp_plotSave(filename = "results/2017/nhoodMap-trans.png", plot = map03, preset = "lg", dpi = 500)

