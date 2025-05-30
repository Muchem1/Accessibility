library(raster)
pop_raster <- raster("D:/Optimizing_Mental_Health_Access/Rasters/L_Pop.tif")
acc_raster <- raster("D:/Optimizing_Mental_Health_Access/Rasters/classified_obstetric_gynaecological_services_accessibility.tif")


if (!compareRaster(pop_raster, acc_raster, extent=TRUE, rowcol=TRUE, crs=TRUE, stopiffalse=FALSE)) {
  pop_raster <- resample(pop_raster, acc_raster, method = "bilinear")
}

# 4. Compute total population
total_pop <- cellStats(pop_raster, stat = "sum", na.rm = TRUE)

# 5. Compute zonal sums by accessibility class
z <- zonal(pop_raster, acc_raster, fun = "sum", na.rm = TRUE)
colnames(z) <- c("Class", "pop_sum")

# 6. Calculate percentages
z <- as.data.frame(z)
z$percentage_of_total <- (z$pop_sum / total_pop) * 100

cat("Total population of study area:", format(total_pop, big.mark=","), "\n\n")
cat("Population by accessibility class:\n")
print(z)
