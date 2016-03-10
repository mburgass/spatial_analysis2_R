library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(dplyr)
library(tidyr)
library(stringr)
library(knitr)
library(DT)
dir__spatial   <- 'spatial_data' #rename layer
layer_bc <- 'ohibc_rgn_simple' #rename layer
poly_bc_rgn <- readOGR(dsn = dir_spatial, layer = layer_bc, stringsAsFactors = FALSE) #read in spatial data
poly_bc_rgn@data
poly_bc_rgn@plotOrder
poly_bc_rgn@bbox
poly_bc_rgn@proj4string
summary(poly_bc_rgn)
p4s_bc <- CRS('+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
slotNames(poly_bc_rgn)
head(poly_bc_rgn@data)
summary(poly_bc_rgn@polygons)
poly_bc_rgn@polygons[[1]]@Polygons[[2]]
harv_data <- data.frame(rgn_id   = c(  1,  2,  3,  5,  8), h_tonnes = c(105, 89, 74, 21, 11))
poly_bc_rgn@data <- poly_bc_rgn@data %>% left_join(harv_data, by = 'rgn_id')
poly_bc_rgn@data
poly_bc_rgn@data <- poly_bc_rgn@data %>% mutate(h_kg   = h_tonnes * 1000, h_tot = sum(h_tonnes, na.rm = TRUE), h_prop = round(h_tonnes/h_tot, 3))
## What is the %>% and what does <- mean? mutate?
poly_bc_rgn@data
### Select only regions with non-NA harvest values
poly_bc_harvest <- poly_bc_rgn[!is.na(poly_bc_rgn@data$h_tonnes),] #what does the $ sign mean?
### or use base::subset (but not dplyr::filter)
poly_bc_harvest <- poly_bc_rgn %>% subset(!is.na(h_tonnes))
poly_bc_harvest@data
plot(poly_bc_rgn,     col = rgb(.7, .7, 1),      border = rgb(0, 0, 1))
plot(poly_bc_harvest, col = rgb(.7, .3, .3, .5), border = rgb(1, 0, 0, .5), add = TRUE)
poly_bc_mpa <- readOGR(dsn = dir_spatial, layer = 'mpa_bc_simple')
plot(poly_bc_rgn, col = rgb(.7, .7, 1), border = rgb(0, 0, 1))
plot(poly_bc_mpa, col =rgb(1, .5, .5, .5), border = rgb(1, 0, 0, .5), add = TRUE) ##does not show because of CRS error - regions map different project
poly_bc_mpa <- spTransform(poly_bc_mpa, p4s_bc)
poly_bc_mpa@proj4string
plot(poly_bc_mpa, col =rgb(1, .5, .5, .5), border = rgb(1, 0, 0, .5), add = TRUE) ## now shows because reprojected
poly_bc_3nm <- readShapePoly(fn = file.path(dir_spatial, 'ohibc_offshore_3nm_simple'), proj4string = p4s_bc)
plot(poly_bc_rgn, col = rgb(.7, .7, 1), border = rgb(0, 0, 1))
plot(poly_bc_mpa, col = rgb(1, .5, .5, .5), border = rgb(1, .3, .3, .8), add = TRUE)
plot(poly_bc_3nm, col = rgb(.5, 1, .5, .5), border = rgb(.3, 1, .3, .8), add = TRUE)
### Intersect the MPA layer with the 3 nm regions layer
poly_bc_3nm_mpa <- raster::intersect(poly_bc_mpa, poly_bc_3nm)
plot(poly_bc_3nm_mpa, col = rgb(1, 1, .5, .5), border = rgb(1, 1, .3, .7), add = TRUE)
poly_bc_3nm_mpa@data$area_km2 <- gArea(poly_bc_3nm_mpa, byid = TRUE) / 1e6 ##what does byid=TRUE mean and how do i look this up? What is 1e6?
poly_bc_3nm@data$area_km2 <- gArea(poly_bc_3nm, byid = TRUE) / 1e6
### Summarize the total protected area within each region
prot_area_df <- poly_bc_3nm_mpa@data %>% group_by(rgn_id, rgn_name, rgn_code) %>%summarize(prot_area_km2 = sum(area_km2)) %>%left_join(poly_bc_3nm@data %>% select(rgn_id, tot_area_km2 = area_km2), by = 'rgn_id') %>%mutate(prot_area_pct = round(prot_area_km2 / tot_area_km2, 3) * 100)
##When line is really long like that how do you split it to keep on screen?
knitr::kable(prot_area_df)
DT::datatable(prot_area_df)
ext <- extent(poly_bc_rgn); ext
ext1 <- poly_bc_rgn@bbox; ext1
ext@xmin <- round(ext@xmin - 5000, -4); ext@ymin <- round(ext@ymin - 5000, -4)
### expand the extents out to round 10 km edges
ext@xmax <- round(ext@xmax + 5000, -4); ext@ymax <- round(ext@ymax + 5000, -4)
### if using the @bbox option, use ext1['x', 'min'] and such
reso <- 2500 ### BC Albers uses meters as units, set resolution to a 2.5-km grid
xcol <- (ext@xmax - ext@xmin)/reso ### determine # of columns from extents and resolution
yrow <- (ext@ymax - ext@ymin)/reso ### determine # of rows
rast_base <- raster(ext, yrow, xcol, crs = p4s_bc)
rast_base ### inspect it: resolution and extents are nice and clean
rast_bc_mpa <- rasterize(poly_bc_mpa, rast_base, field = 'STATUS_YR', fun = 'min')
plot(poly_bc_rgn, col = rgb(.7, .7, 1, .5), border = NA)
plot(rast_bc_mpa, add = TRUE)
plot(poly_bc_3nm, col = rgb(1, .5, .5, .5), border = NA, add = TRUE)
### Extract the MPA raster cells by region
mpa_by_rgn <- raster::extract(rast_bc_mpa, poly_bc_3nm, weights = FALSE)
names(mpa_by_rgn) <- poly_bc_3nm@data$rgn_id ##list of extracted vectors
mpa_rgn_df <- data.frame() 
for (rgn_id in names(mpa_by_rgn)) {
  temp_df <- data.frame(rgn_id = as.numeric(rgn_id), year = unlist(mpa_by_rgn[[rgn_id]]))
  mpa_rgn_df <- rbind(mpa_rgn_df, temp_df)
}
prot_area_df2 <- mpa_rgn_df %>%
  group_by(rgn_id) %>%
  summarize(cells_mpa     = sum(!is.na(year)),
            cells_tot     = n(),
            prot_area_pct = round(cells_mpa/cells_tot, 3) * 100) %>%
  left_join(poly_bc_rgn@data %>%
              select(rgn_id, rgn_name),
            by = 'rgn_id')
prot_area_df <- prot_area_df %>%
  rename(prot_pct_vec = prot_area_pct) %>%
  left_join(prot_area_df2 %>%
              select(rgn_id, prot_pct_rast = prot_area_pct))
knitr::kable(prot_area_df)
##So the idea of rasterising was to get finer resolution and clearer results? 