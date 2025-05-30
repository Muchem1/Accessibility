library(raster)   
library(sp)       
library(dplyr)   
library(readr)  


facilities_file <- "D:/Optimizing_Mental_Health_Access/Services/mental_health_0.csv"   # CSV file with facilities data
raster_file <- "D:/Optimizing_Mental_Health_Access/Rasters/classified_mental_health_services_accessibility.tif"  # Classified raster file


facilities <- read_csv(facilities_file)
print(names(facilities))

missing_coords <- facilities %>% filter(is.na(longitude) | is.na(latitude))
if(nrow(missing_coords) > 0){
  cat("Found", nrow(missing_coords), "rows with missing coordinates. These will be removed.\n")
}

facilities_clean <- facilities %>% filter(!is.na(longitude) & !is.na(latitude))

classified_raster <- raster(raster_file)
facilities_sp <- SpatialPointsDataFrame(
  coords = facilities_clean[, c("longitude", "latitude")],
  data = facilities_clean,
  proj4string = CRS(projection(classified_raster))
)

# Extract raster values at the facilities' locations
facilities_clean$land_cover <- extract(classified_raster, facilities_sp)

# Filter facilities that are outside class "30"
filtered_facilities <- facilities_clean %>% filter(land_cover != 30)

# Rank
ranked_facilities <- filtered_facilities %>% arrange(desc(ppltn_c))

output_file <- "D:/Optimizing_Mental_Health_Access/Services/HF_outside_30mins_access_mental_health.csv"
write_csv(ranked_facilities, output_file)

cat("Filtered and ranked facilities have been written to", output_file, "\n")

