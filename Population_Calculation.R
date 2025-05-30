library(raster)
pop_raster <- raster("D:/Optimizing_Mental_Health_Access/Rasters/L_Pop.tif")
acc_raster <- raster("D:/Optimizing_Mental_Health_Access/Rasters/classified_obstetric_gynaecological_services_accessibility.tif")


# Ensures pop_raster matches acc_raster in resolution, extent, and CRS
if (!compareRaster(pop_raster, acc_raster, extent=TRUE, rowcol=TRUE, crs=TRUE, stopiffalse=FALSE)) {
  pop_raster <- resample(pop_raster, acc_raster, method = "bilinear")
}

#––– 4. Compute total population
# cellStats sums all cell values in the population raster :contentReference[oaicite:0]{index=0}
total_pop <- cellStats(pop_raster, stat = "sum", na.rm = TRUE)

#––– 5. Compute zonal sums by accessibility class
# zonal() sums pop_raster values for each integer zone in acc_raster :contentReference[oaicite:1]{index=1}
z <- zonal(pop_raster, acc_raster, fun = "sum", na.rm = TRUE)
colnames(z) <- c("Class", "pop_sum")

#––– 6. Calculate percentages
z <- as.data.frame(z)
z$percentage_of_total <- (z$pop_sum / total_pop) * 100

#––– 7. Print results
cat("Total population of study area:", format(total_pop, big.mark=","), "\n\n")
cat("Population by accessibility class:\n")
print(z)
