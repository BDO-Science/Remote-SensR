# Initialize the Earth Engine
ee_Initialize()

# 1. Define the region of interest (ROI)
coordinates <- matrix(c(86.01728415270877, 20.867517904666165,
                        86.01728415270877, 19.877530444762456,
                        87.0733449437244, 19.877530444762456,
                        87.0733449437244, 20.867517904666165), 
                      ncol = 2, byrow = TRUE)

area_of_interest <- ee$Geometry$Polygon(list(coordinates))
Map$centerObject(area_of_interest)

# 2. Select Sentinel-2 images and filter them
sentinel2_collection <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  select(c('B2', 'B3', 'B4', 'B8'))$
  filterDate('2023-01-01', '2023-12-31')$
  filterBounds(area_of_interest)$
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 10))$
  median()$
  multiply(0.0001)  # Scale the reflectance values

Map$addLayer(sentinel2_collection$clip(area_of_interest), 
             list(bands = c('B4', 'B3', 'B2'), min = 0.051, max = 0.322, gamma = 1.1), 
             name = 'True Color Composite', shown = FALSE)

# 3. Add the Sentinel-2 false color composite to the map
Map$addLayer(sentinel2_collection$clip(area_of_interest), 
             list(bands = c('B8', 'B4', 'B3'), min = 0.051, max = 0.322, gamma = 1.1), 
             name = 'False Color Composite', shown = FALSE)

# 4. Calculate the Normalized Difference Water Index (NDWI)
ndwi <- sentinel2_collection$normalizedDifference(c('B3', 'B8'))$rename('NDWI')
Map$addLayer(ndwi$clip(area_of_interest), 
             list(palette = c('green', 'orange', 'red')), 
             name = 'Normalized Difference Water Index', shown = FALSE)

# 5. Print a histogram of NDWI values within the ROI
ndwi_histogram <- ui$Chart$image$histogram(ndwi, area_of_interest, 100)
print(ndwi_histogram)

# 6. Threshold the NDWI to create a water mask
water_mask <- ndwi$gt(0.1)
Map$addLayer(water_mask$clip(area_of_interest), 
             list(palette = c('green', 'blue')), 
             name = 'Water Mask', shown = FALSE)

# 7. Apply the water mask to the Sentinel-2 collection
water_masked_sentinel2 <- sentinel2_collection$updateMask(water_mask)
Map$addLayer(water_masked_sentinel2$clip(area_of_interest), 
             list(), name = 'Masked Water Sentinel-2', shown = FALSE)

# 8. Calculate the Normalized Difference Turbidity Index (NDTI)
ndti <- water_masked_sentinel2$normalizedDifference(c('B4', 'B3'))$rename('NDTI')
Map$addLayer(ndti$clip(area_of_interest), 
             list(palette = c('blue', 'green', 'yellow', 'orange', 'red')), 
             name = 'Normalized Difference Turbidity Index', shown = FALSE)

print(ui$Chart$image$histogram(ndti, area_of_interest, 100))

# 9. Threshold the NDTI to create a turbidity mask
turbidity_mask <- ndti$gt(0.0156)
Map$addLayer(turbidity_mask$clip(area_of_interest), 
             list(palette = c('green', 'red')), 
             name = 'High Turbidity Area', shown = FALSE)

# 10. Export the NDTI image to Google Drive
task <- ee$batch$Export$image$toDrive(
  image = ndti$clip(area_of_interest),
  description = 'Sentinel2_NDTI',
  region = area_of_interest,
  scale = 10,
  crs = ndti$projection()$crs(),
  maxPixels = 1e13,
  folder = 'Water_Turbidity_Mapping'
)

task$start()