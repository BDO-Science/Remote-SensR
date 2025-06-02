#install and load rgee package
#install.packages("rgee")
#reticulate::use_condaenv("rgee", required = TRUE)

library(rgee)
library(reticulate) #interfacce between R and python just in case
library(sf)
library(tidyverse)
library(terra)
library(stars)
library(geojsonio)
library(ggspatial)
library(tidyterra)
library(ggpubr)
library(deltamapr)

ee_Authenticate()

#####################
#SETTING UP SHAPEFILES
######################
# Read in the shapefile
# Replace 'path_to_shapefile' with the actual path to your shapefile
shapefile_path <- "california_watershed_shapefile/Data/cdfg_100k_2003_6.shp"
california_watersheds <- st_read(shapefile_path) |>
  st_zm(drop = TRUE, what = "ZM") |>
  unnest(cols = c(NAME))

# Create a vector of creek and river names to filter by
creeks_rivers <- c("Sacramento River", "Feather River", "Yuba River", "Bear River", "American River", "Cosumnes River", "Mokelumne River", "Calaveras River", "Stanislaus River", "Tuolumne River", "Merced River", "San Joaquin River")

# Use str_detect to filter the dataset based on partial string matches
filtered_watersheds <- california_watersheds |>
  filter(str_detect(NAME, paste(creeks_rivers, collapse = "|")))

# Check the structure of the filtered object
str(filtered_watersheds)

# Filter the WW_Watershed dataset to only include the specified creeks and rivers
filtered_watersheds <- california_watersheds |>
  filter(NAME %in% creeks_rivers)

crs_value <- st_crs(WW_Watershed)  # Set CRS to match one of your spatial objects

filtered_watersheds <- st_transform(filtered_watersheds, crs_value)

####################################
#Loading raster image and plotting
####################################

local_file <- "downloaded_image.tif"
raster_data <- rast(local_file)

# Plot with ggplot2 and friends
ggplot() +
  geom_sf(data = WW_Delta, fill = "grey") +
  #geom_sf(data = filtered_watersheds, fill = "grey") +
  #geom_sf(data = WW_DBW, fill = "grey") +
  #geom_sf(data = WW_Watershed, fill = "grey") +
  geom_spatraster(data = chlorophyll, alpha = 0.9) +
  scale_fill_viridis_c() +
  labs(title = "Landsat 9 TOA",
       fill = "Reflectance") +
  theme_pubr() +   labs_pubr(base_size = 14) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  #coord_sf(xlim = c(-123, NULL), expand = FALSE) +  # Set x-axis limit
  annotation_scale(location = "bl", width_hint = 0.5) +  # Optional scale bar
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)  # Optional north
  #facet_wrap(~lyr)





########
#######
#GARAGE
######
######

##############################
#PYTHON AND RGEE
############################
#reticulate::install_miniconda()

#ee_install()

#terminate R and reload rgee package before running this 
#ee_check()

#reticulate::py_install("earthengine-api==0.1.370", envname = "rgee", pip = TRUE)

#in case initialization is acting funky
#rgee::ee_clean_user_credentials(user = 'vaisvila2@gmail.com')


#rgee::ee_Authenticate(user = 'vaisvila2@gmail.com')

# Initialize Earth Engine
rgee::ee_Initialize(user = 'vaisvila2@gmail.com')

ee_get_earthengine_path()

# Define the region of interest and load Landsat 9 collection
delta_roi <- ee$Geometry$Polygon(list(list(
  c(-121.9, 38.0), c(-121.9, 38.3), c(-121.5, 38.3), c(-121.5, 38.0)
)))  # Example polygon for ROI, adjust coordinates as needed

#delta_roi <- ee$Geometry$Polygon(list(list(
#c(-122.5, 37.0), c(-122.5, 39.0), c(-120.5, 39.0), c(-120.5, 37.0), c(-122.5, 37.0)
#)))

landsat_collection <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")
landsat_image <- landsat_collection$
  filterBounds(delta_roi)$
  filterDate('2024-8-27', '2024-9-1')$first()

# Select compatible bands (e.g., B2, B3, B4 for RGB)
landsat_image <- landsat_image$select(c("SR_B2", "SR_B3", "SR_B4", "SR_B5"))  # Replace with the bands you need "SR_B2", "SR_B3", 

# Export to R using Google Drive
landsat_raster <- ee_as_rast(
  image = landsat_image,
  region = delta_roi,
  scale = 10,
  via = "drive"  # Uses Google Drive for temporary storage
)

head(landsat_raster)
str(landsat_raster)

# Assume landsat_raster contains Landsat bands including SR_B3 and SR_B5
# Calculate NDWI
ndwi <- (landsat_raster[[2]] - landsat_raster[[4]]) / (landsat_raster[[2]] + landsat_raster[[4]])

# Calculate a turbidity index as Red/Green ratio
turbidity <- landsat_raster[[3]] / landsat_raster[[2]]

# Calculate Chlorophyll-a index as Blue / Red ratio
chlorophyll <- landsat_raster[[1]] / landsat_raster[[3]]

# Replace the file path with the actual path to your KML file
#channel_kml <- st_read("2012_channel.kml")


# Set up task to export the image to Google Drive
task <- ee_image_to_drive(
  image = img,
  region = delta_roi,
  scale = 30,
  fileFormat = 'GeoTIFF',
  folder = 'GEE_images',
  fileNamePrefix = 'landsat_delta'
)
task$start()

# Load the downloaded GeoTIFF file
landsat_raster <- rast("landsat_delta.tif")
