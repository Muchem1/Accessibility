library(raster)
library(sf)
library(gdistance)
library(malariaAtlas)

# 1. Read the boundary for the study area.
Tc_Bndry <- st_read("D:/Accessibility_Data/county/L_Bndry.shp")

# 2. Get the friction surface using malariaAtlas.
friction <- malariaAtlas::getRaster(
  surface = "Global friction surface enumerating land-based travel speed with access to motorized transport for a nominal year 2019",
  shp    = Tc_Bndry
)





rasters <- listRaster(printed = FALSE)
# look at titles
fric_idx <- grep("friction surface", rasters$title, ignore.case = TRUE)
rasters[fric_idx, c("dataset_id","title","pub_year")]


friction <- malariaAtlas::getRaster(
  surface    = rasters$dataset_id[fric_idx],
  shp        = Tc_Bndry,
  year       = 2019
)




# Convert the SpatRaster to a RasterLayer.
friction_raster <- raster(friction)

# 3. Load the population raster 
population <- raster("D:/Accessibility_Data/county/L_Pop.tif")

# compute total population of the entire study area 
total_population <- cellStats(population, sum, na.rm = TRUE)
cat("Total population (study area):", total_population, "\n\n")

# 4. Load health facility locations 
health_facilities <- st_read("D:/Accessibility_Data/Laikipia_Analysis/obstetric_gynaecological_YES.shp")

# 5. Create a transition object from the friction surface.
tr <- transition(1 / friction_raster, transitionFunction = mean, directions = 8)
tr <- geoCorrection(tr)

# 6. Convert health facilities from sf to a SpatialPointsDataFrame.
health_facilities_sp <- as(health_facilities, "Spatial")

# 7. Compute the accumulated cost (travel time) raster.
costRaster <- accCost(tr, health_facilities_sp)

# Resample to match the population raster.
costRaster <- resample(costRaster, population, method = "bilinear")

# 8. Define travel time thresholds (in minutes).
mask_0_30    <- costRaster <= 30
mask_31_60   <- costRaster > 30  & costRaster <= 60
mask_61_120  <- costRaster > 60  & costRaster <= 120
mask_121_180 <- costRaster > 120 & costRaster <= 180
mask_180     <- costRaster >= 180

# 9. Sum population in each travel‐time band.
pop_0_30    <- cellStats(mask(population, mask_0_30,    maskvalue = FALSE), sum, na.rm = TRUE)
pop_31_60   <- cellStats(mask(population, mask_31_60,   maskvalue = FALSE), sum, na.rm = TRUE)
pop_61_120  <- cellStats(mask(population, mask_61_120,  maskvalue = FALSE), sum, na.rm = TRUE)
pop_121_180 <- cellStats(mask(population, mask_121_180, maskvalue = FALSE), sum, na.rm = TRUE)
pop_180     <- cellStats(mask(population, mask_180,     maskvalue = FALSE), sum, na.rm = TRUE)

# 10. Output the results.
cat("Population within 0–30 minutes:   ", pop_0_30,    "\n")
cat("Population within 31–60 minutes: ", pop_31_60,   "\n")
cat("Population within 61–120 minutes:", pop_61_120,  "\n")
cat("Population within 121–180 minutes:", pop_121_180, "\n")
cat("Population over 180 minutes:     ", pop_180,     "\n")

# 11. Calculate percentages of each travel time category
total_pop_travel <- pop_0_30 + pop_31_60 + pop_61_120 + pop_121_180 + pop_180
percent_0_30    <- round((pop_0_30    / total_pop_travel) * 100, 1)
percent_31_60   <- round((pop_31_60   / total_pop_travel) * 100, 1)
percent_61_120  <- round((pop_61_120  / total_pop_travel) * 100, 1)
percent_121_180 <- round((pop_121_180 / total_pop_travel) * 100, 1)
percent_180     <- round((pop_180     / total_pop_travel) * 100, 1)

# 12. Print out the percentage results
cat("Percent within 0–30 minutes:    ", percent_0_30,  "%\n")
cat("Percent within 31–60 minutes:   ", percent_31_60, "%\n")
cat("Percent within 61–120 minutes:  ", percent_61_120, "%\n")
cat("Percent within 121–180 minutes: ", percent_121_180, "%\n")
cat("Percent over 180 minutes:       ", percent_180,     "%\n")

# 12. Create a classified raster for visualization
classified_raster <- mask_0_30 * 30 + 
  mask_31_60 * 60 + 
  mask_61_120 * 120 + 
  mask_121_180 * 180 + 
  mask_180 * 181


