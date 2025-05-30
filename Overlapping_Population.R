
library(sf)     
library(terra)  


poly1 <- st_read("D:/Optimizing_Mental_Health_Access/Shapefiles/30mins_CA_combined_pixels/catchment_area_14197_0_30.shp")  
poly2 <- st_read("D:/Optimizing_Mental_Health_Access/Shapefiles/30mins_CA_combined_pixels/catchment_area_15065_0_30.shp")
pop_rast <- rast("D:/Optimizing_Mental_Health_Access/Rasters/L_Pop.tif")

sf::sf_use_s2(FALSE)


intersect_poly <- st_intersection(poly1, poly2)



#Crop & mask the raster to just the intersection polygon
pop_crop <- crop(pop_rast, vect(intersect_poly))
pop_mask <- mask(pop_crop, vect(intersect_poly))

#Sum pop in the masked raster
pop_values <- values(pop_mask, na.rm = TRUE)
pop_sum    <- sum(pop_values, na.rm = TRUE)

total_pop  <- 600855
pct        <- (pop_sum / total_pop) * 100


cat(sprintf(
  "Population in intersection: %.0f\nPercentage of total (600,855): %.2f%%\n",
  pop_sum,
  pct
))
