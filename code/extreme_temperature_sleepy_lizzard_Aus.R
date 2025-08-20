# Load required libraries
library(terra)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
require(mapview)

# Load NetCDF file
# nc_file <- "/Users/diegoellis/Downloads/BEST (Berkeley Earth Surface Temperature) 1880-2021_TX90p_ANN_AverageMap_2000-2021_-40.45to-9.94_107.27to170.02.nc"
nc_file <- "indir/BEST (Berkeley Earth Surface Temperature) 1880-2021_TX90p_ANN_AverageMap_2000-2021_-40.45to-9.94_107.27to170.02.nc"
# 
raster_data <- rast(nc_file)  # Load as SpatRaster

# You can select the first layer or a specific variable
r <- raster_data[[1]]

# Load the Sleepy Lizard range shapefile
# Replace "sleepy_lizard.shp" with the actual .shp filename
# sleepy_lizard <- st_read("/Users/diegoellis/Downloads/redlist_species_data_5642b3ec-765c-481a-8710-f70a5bb772ee/data_0.shp")
sleepy_lizard <- st_read("indir/sleepy_lizzard_range/data_0.shp")

# Make sure CRS match
sleepy_lizard <- st_transform(sleepy_lizard, crs(r))

# Convert raster to data frame for ggplot
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "value"  # Rename value column

# Country outline: Australia
australia <- ne_countries(scale = "large", country = "Australia", returnclass = "sf")
australia <- st_transform(australia, crs(r))
australia <- st_transform(australia, crs(sleepy_lizard))

# mapview(r) + mapview(sleepy_lizard)

# Plot with ggplot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  geom_sf(data = sleepy_lizard, fill = NA, color = "black", size = 0.8, lwd = 0.5) +  # <- thicker lines
  coord_sf() +
  theme_minimal() +
  labs(title = "Extreme Weather with Sleepy Lizard Range",
       fill = "Extreme Weather\nValue")

writeRaster(r, filename = "outdir/extreme_weather_tx90p_sleepy_lizzard.tif", overwrite = TRUE)

# Red pal
custom_colors <- c(
  "#f7fcfd",  # lightest
  "#fdd49e",  # pale orange
  "#fdbb84",  # orange
  "#fc8d59",  # reddish-orange
  "#ef6548",  # strong red-orange
  "#d7301f",  # red
  "#b30000",  # deep red
  "#7f0000"   # very dark red/brown
)


ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = custom_colors, na.value = NA,
                       name = "% of days") +
  geom_sf(data = australia, fill = NA, color = "black", size = 0.5, lwd = 0.5) +  # <-- country outline
  geom_sf(data = sleepy_lizard, fill = NA, color = "black", size = 0.9, lwd = 0.5) +
  #coord_sf() +
  theme_minimal() +
  labs(title = "Extreme Weather with Sleepy Lizard Range",
       subtitle = "TX90p Annual Average (2000–2021)",
       fill = "% of days",
       caption = "Data: www.climdex.org | Map: UNSW Sydney")

# Updated plot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = custom_colors, na.value = NA,
                       name = "% of days") +
  geom_sf(data = sleepy_lizard, fill = NA, color = "black", size = 0.8) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Extreme Weather with Sleepy Lizard Range",
    subtitle = "TX90p Annual Average (2000–2021)",
    caption = "Data: www.climdex.org | Map: UNSW Sydney"
  )


# OPTIONAL: Add topographic contours (from a global DEM)
# You can use a cropped version of ETOPO1 or SRTM data
# Here’s a quick option using `elevatr` (requires internet)
# library(elevatr)
# bbox <- st_bbox(australia)
# elev <- get_elev_raster(locations = australia, z = 4, clip = "location")
# elev_terra <- rast(elev)
# contours <- as.contour(elev_terra, levels = seq(0, 2000, by = 500))
# contours_sf <- st_as_sf(contours)

# Raster to df for ggplot
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "value"

# Color palette from image
custom_colors <- c(
  "#f7fcfd", "#fdd49e", "#fdbb84", "#fc8d59",
  "#ef6548", "#d7301f", "#b30000", "#7f0000"
)

# Plot
ggplot() +
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = custom_colors, na.value = NA,
                       name = "% of days", limits = c(10, 19)) +
  geom_sf(data = australia, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = sleepy_lizard, fill = NA, color = "black", size = 0.9) +
  # geom_sf(data = contours_sf, color = "grey50", size = 0.3) +  # Uncomment if adding contours
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Extreme Weather with Sleepy Lizard Range",
    subtitle = "TX90p Annual Average (2000–2021)",
    caption = "Data: Climdex | Map: UNSW Sydney"
  )

library(terra)
library(sf)
library(mapview)
library(elevatr)  # Optional for SRTM

# Load raster
r <- rast("indir/BEST (Berkeley Earth Surface Temperature) 1880-2021_TX90p_ANN_AverageMap_2000-2021_-40.45to-9.94_107.27to170.02.nc")[[1]]

# Load shapefile and reproject
sleepy_lizard <- st_read("indir/sleepy_lizzard_range/data_0.shp")
sleepy_lizard <- st_transform(sleepy_lizard, crs = crs(r))

# Color palette: red to dark red
reds <- colorRampPalette(c("#ffcccc", "#cc0000", "#660000"))

# Mapview raster with custom palette
map_r <- mapview(r,
                 col.regions = reds(10),
                 layer.name = "Extreme Heat",
                 na.color = "transparent",
                 at = seq(min(values(r), na.rm=TRUE), max(values(r), na.rm=TRUE), length.out = 100))

# Mapview shapefile: only black lines, no fill
map_range <- mapview(sleepy_lizard,
                     color = "black",
                     alpha.regions = 0,
                     lwd = 2,
                     layer.name = "Sleepy Lizard Range")

# Optional: Add SRTM contours (low-res example)
# Use elevatr to fetch terrain for same bounding box
bbox <- st_bbox(sleepy_lizard)
elev <- get_elev_raster(locations = sleepy_lizard, z = 6, clip = "bbox")
elev_terra <- rast(elev)
elev_terra <- project(elev_terra, r)  # Match CRS

# Generate contours
contours <- as.contour(elev_terra, levels = seq(0, 2000, by = 250))
contours_sf <- st_as_sf(contours)

map_contours <- mapview(contours_sf,
                        color = "grey10",
                        lwd = 1.5,
                        layer.name = "Topography")

# Combine layers
map_r + map_range + map_contours


require(viridis)
# Generate magma palette
magma_colors <- viridis(100, option = "magma")

# Raster map with magma color scale
map_r <- mapview(r,
                 col.regions = magma_colors,
                 layer.name = "Extreme Heat (magma)",
                 na.color = "transparent",
                 at = seq(min(values(r), na.rm=TRUE), max(values(r), na.rm=TRUE), length.out = 100))

# Optional: SRTM contours
bbox <- st_bbox(sleepy_lizard)
elev <- get_elev_raster(locations = sleepy_lizard, z = 6, clip = "bbox")
elev_terra <- rast(elev)
elev_terra <- project(elev_terra, r)

contours <- as.contour(elev_terra, levels = seq(0, 2000, by = 250))
contours_sf <- st_as_sf(contours)

map_contours <- mapview(contours_sf,
                        color = "grey40",
                        lwd = 0.5,
                        layer.name = "Topography")

# Combine all layers
map_r + map_range + map_contours
