
# Load the required packages:
library(fMultivar)
library(RANN)
library(sf)
library(tidyverse)
library(geogrid)
library(viridis)

# Load in spatial data of Great Britain Local Authorities (Super Generalised)
LA.sf <- st_read('~/Local_Authority_Districts_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain.shp')

# Load in Local Authority level EU referendum result data
df <- read_csv('~/EU-referendum-result-data.csv')

# Basic data handling to make the merge (some optional):
LA.sf$lad15cd <- as.character(LA.sf$lad15cd)
df <- df %>% rename(lad15cd = Area_Code)
LA.sf <- left_join(LA.sf, df, by = "lad15cd")
LA.sf <- LA.sf %>% select(-lad15nmw)

# Filter out Scotland and Wales, leaving only England
LAE.sf <- LA.sf  %>%
  filter(Region != "Scotland") %>%
  filter(Region != "Wales")

# Project to British National Grid
LAE.sf <- st_transform(LAE.sf, 27700)

# Create an sp version, for later use
LAE.sp <- as(LAE.sf, 'Spatial')

# Main plot of original boundaries
ggplot(LAE.sf) +
  geom_sf(aes(fill = Pct_Remain))  +
  scale_fill_viridis_c(name = "%") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Creating balanced cartogram and hexogram, adapting from Richard Harris open code
# See: https://rpubs.com/profrichharris/hexograms

# Get the function needed
script <- RCurl::getURL("https://raw.githubusercontent.com/profrichharris/Rhexogram/master/functions.R")
eval(parse(text = script))

# Number of bins guided by the -binN- function for a visual plot.
# 29 is also used by Harris in example.
harris.29  <- hexogram(LAE.sp, 29)

# Extract the cartogram and hexograms
harris.29.carto.sp <- harris.29[[1]] # 1 is carto
harris.29.hex.sp <- harris.29[[2]]   # 2 is hexo

# Convert to sf for plotting
harris.29.carto.sf <- st_as_sf(harris.29.carto.sp)
harris.29.hex.sf   <- st_as_sf(harris.29.hex.sp)

# Create variable for fill
z <-  LAE.sf$Pct_Remain

# Basic plots
ggplot(harris.29.hex.sf) +
  geom_sf(aes(fill = z)) +
  scale_fill_viridis_c()

ggplot(harris.29.carto.sf) +
  geom_sf(aes(fill = z)) +
  scale_fill_viridis_c()

# Next, we try geogrid using the default options and original boundaries.

# Hex grid
LAE.hex <- calculate_grid(shape = LAE.sp, grid_type = "hexagonal", seed = 1)
LAE.hex <- assign_polygons(LAE.sp, LAE.hex)
LAE.hex <- st_as_sf(LAE.hex)

# Reg grid
LAE.reg <- calculate_grid(shape = LAE.sp, grid_type = "regular", seed = 1)
LAE.reg <- assign_polygons(LAE.sp, LAE.reg)
LAE.reg <- st_as_sf(LAE.reg)

# Basic plots
ggplot(LAE.hex) +
  geom_sf(aes(fill = Pct_Remain)) +
  scale_fill_viridis_c()

ggplot(LAE.reg) +
  geom_sf(aes(fill = Pct_Remain)) +
  scale_fill_viridis_c()

# This looked problematic. In an attempt to improve it, whilst still using the original boundaries
# to assign polygons, we created the initial empty grid from the balanced cartogram. This created
# gaps but was considered to be a slight improvement. This was used for the survey.

# Hex grid
LAE.hex.carto     <- calculate_grid(shape = harris.29.carto.sp, grid_type = "hexagonal", seed = 1)
LAE.hex.harris    <- assign_polygons(LAE.sp, LAE.hex.carto)
LAE.hex.harris.sf <- st_as_sf(LAE.hex.harris)

# Regular grid
LAE.reg.carto     <- calculate_grid(shape = harris.29.carto.sp, grid_type = "regular", seed = 432423)
LAE.reg.harris    <- assign_polygons(LAE.sp, LAE.reg.carto)
LAE.reg.harris.sf <- st_as_sf(LAE.reg.harris)

# Basic plots
ggplot(LAE.hex.harris.sf) +
  geom_sf(aes(fill = Pct_Remain)) +
  scale_fill_viridis_c()

ggplot(LAE.reg.harris.sf) +
  geom_sf(aes(fill = Pct_Remain)) +
  scale_fill_viridis_c()

