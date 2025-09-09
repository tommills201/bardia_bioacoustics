# Last Updated    : 09/09/25 14:00 
# Author  Name    : Tom Mills
# Affiliation  : UCL
# Title: Plot Bardia map with Recording Sites (based on code provided by Peggy Bevan)

# import packages
library(dplyr)
library(sf)
library(ggplot2)
library(terra)
library(MetBrewer)
library(patchwork) 

setwd("/Users/Mills/Library/CloudStorage/OneDrive-UniversityCollegeLondon/TM_Dissertation_25/Scripts_R")
getwd()

# -----Code ----- #

# - 1. Use sf to to plot data of recording sites - #

# load covariate data
covs = read.csv('siteVariable_149sites_Nepal_Oct2020.csv')
head(covs)
plot(covs$Xcoord, covs$Ycoord)
unique(covs$Datum)

coords = covs[,c(2,4,16,17,18)]

# convert to  sf object by providing the XY columns as coordinates
# initially we create an object in the WGS84 projection with a lat-lon units (CRS = 4326)
locs = sf::st_as_sf(x = coords,
                    coords = c("Xcoord", "Ycoord"),
                    crs = 32644)
plot(locs$geometry)

# reproject to metres for ease using st_transform to a metres projection
locs2 = sf::st_transform(locs, crs = "+proj=utm +zone=44 +north +datum=WGS84 +units=m +no_defs")


# - 2. Plot boundaries  - #

# load Bardia boundaries and overall study area (stored as an ESRI shapefile)

# all boundaries 

boundaries = sf::st_read("Boundaries/WDPA_May2018_NPL-shapefile-polygons.shp") %>%
  sf::st_transform(crs = sf::st_crs(locs))
bardiabound = boundaries[grepl('Bardia', boundaries$NAME),]
plot(bardiabound$geometry)

# NP & BZ shapefiles

NPbound = sf::st_read("Boundaries/BardiaNP.shp") %>%
  sf::st_transform(crs = sf::st_crs(locs))
BZbound = sf::st_read("Boundaries/Bardia_BZ.shp") %>%
  sf::st_transform(crs = sf::st_crs(locs))
OBZbound = sf::st_read("Boundaries/Bardia_vdc.shp") %>%
  sf::st_transform(crs = sf::st_crs(locs))
plot(OBZbound$geometry)
head(NPbound)

# - 3. plot map with site locations - #

# pass ggplot the study area, conservancies and the locations
# ensuring everything has a harmonised CRS means is mapped correctly
ggplot() +
  geom_sf(data=OBZbound, aes(fill = NAME_4)) +
  geom_sf_label(data = OBZbound, aes(label = NAME_4)) +
  geom_sf(data=locs, color="black") +
  theme_classic()

# area to remove
remove = c('Belawa', 'Sorhawa', 'Jamuni', "ManpurMainapokhar",
           'Mahamadpur', 'Taratal', 'Dhodhari',
           "Suryapatawa", "KhairiChandanpur", "Manpurtapara",
           "Bhimapur", "Rajapur", "Badalpur", "Manau",
           "Nayagaun", "Daulatpur", "PasupatiNagar",
           "Patabhar", "Gola")
OBZbound2 = OBZbound[!(OBZbound$NAME_4 %in% remove),]

ggplot() +
  geom_sf(data=OBZbound2, fill = 'grey90') +
  geom_sf(data = BZbound, colour = 'orange') +
  geom_sf(data = NPbound, colour = 'green') +
  geom_sf(data=locs, aes(color=Management)) +
  theme_classic()

# - 4. plot habitat rasters - #

# read in land cover raster (GeoTIFF)
# load in 2019 land cover data
# read in habitat raster (saved as a GeoTiff file)
hab = terra::rast("Landcover_nepal_nlcms_frtc_2019/LandCover_NP_2019.tif")
plot(hab)

hab2 = terra::project(hab, crs(OBZbound2))
plot(hab2)

# crop to bardia field site = in a square
# define the current extent
bardia_extent <- ext(locs)

# calculate the width and height of the current extent
width <- xmax(bardia_extent) - xmin(bardia_extent)
height <- ymax(bardia_extent) - ymin(bardia_extent)

# determine the side length for the square (use the larger of width or height)
side_length <- max(width, height)
# calculate the center of the extent
center_x <- (xmin(bardia_extent) + xmax(bardia_extent)) / 2
center_y <- (ymin(bardia_extent) + ymax(bardia_extent)) / 2
# create a new square extent
square_extent <- ext(
  center_x - side_length / 2, center_x + side_length / 2,
  center_y - side_length / 2, center_y + side_length / 2
)
# expand the square extent by 10 km
square_extent <- square_extent + 10000
# crop the habitat raster to the square extent
bardiahab <- crop(hab2, square_extent)
# plot the result
plot(bardiahab)

writeRaster(bardiahab, "Output/Nepal2020LandCover.tif", overwrite = TRUE)
plot(bardiahab)
plot(locs$geometry)

bardiahab = terra::rast("Output/Nepal2020LandCover.tif")

# head(sp_det)
# head(locs)
# locs  = locs %>%
# left_join(sp_det)
# head(locs)

hab_df = as.data.frame(bardiahab, xy=TRUE)
hab_df$LandCover_NP_2019 = floor(hab_df$LandCover_NP_2019)
hab_df$LandCover_NP_2019 = factor(hab_df$LandCover_NP_2019) 
unique(hab_df$LandCover_NP_2019) 

hab_df = hab_df %>%
  mutate(LC = recode_factor(LandCover_NP_2019,
                            '0' = 'nodata',
                            '1' = 'Rivers',
                            '2' = 'Rivers',
                            '3' = 'Rivers',
                            '4' = 'Forest',
                            '5' = 'Rivers',
                            '6' = 'Built-up Area',
                            '7' = 'Cropland',
                            '8' = 'Other woodland',
                            '9' = 'Other woodland',
                            '10' = 'Grassland',
                            '11' = 'Other woodland'
  )
  )

unique(hab_df$LC)
okabe <- c('grey',"#0072B2","#007e2f","#E69F00","#F0E442","#CC79A7","#009E73")
names(okabe) = levels(hab_df$LC)

# import sample only site labels to highlight
highlight_sites <- read.csv("unique_site_names.csv")
# normalise in case thjere are stray case mismatches
highlight_sites$site <- trimws(highlight_sites$site)
locs$CT_site <- trimws(locs$CT_site)
# filter locs using CT_site vs site
highlight_locs <- dplyr::filter(locs, CT_site %in% highlight_sites$site)


# - 5. plot map with ratsters, sites and legends - #

devtools::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library(ggspatial)
library(scales)
library(ggnewscale)

mymap = ggplot() +
  geom_raster(data = hab_df, aes(x, y, fill=factor(LC))) +
  geom_sf_pattern(data = BZbound, aes(colour = 'Buffer Zone'), pattern = 'stripe', pattern_fill = NA, fill = NA, pattern_density = 1, pattern_spacing = 0.01, show.legend = "polygon", linewidth = 1) +
  geom_sf(data = NPbound, aes(colour = 'National Park'), fill = NA, linewidth = 1, show.legend = 'polygon')+
  geom_sf(data = locs, aes(shape = "All sites"), colour = "black", size = 1.5) +
  geom_sf(data = highlight_locs, aes(shape = "Sample sites"), colour = "blue",
          size = 5, stroke = 1.2) +
  scale_shape_manual(
    values = c("All sites" = 16, "Sample sites" = 1), 
    name = "Sites",
    guide = guide_legend(
      override.aes = list(
        colour = c("black", "blue"),
        fill   = NA, linetype = 0, pattern = "none"))) +
  scale_colour_manual(values = c("National Park" = "grey", "Buffer Zone" = "grey"), limits = c("National Park", "Buffer Zone"), name = 'Boundaries',
                      guide = guide_legend(override.aes = list(linetype = 1, pattern = c('none', 'stripe'), shape = NA))) +
  scale_fill_manual(breaks = names(okabe[-1]), values = okabe[-1], name = 'Land Cover',
                    guide = guide_legend(override.aes = list(linetype = 0, shape = NA, pattern = 'none', ncol = 2))) +
  #labs(fill = 'Land Cover') +
  theme_classic() +
  scale_x_continuous(limits = c(514400,569008), expand = c(0, 0)) +
  scale_y_continuous(limits = c(3118979,3172984), expand = c(0, 0)) +
  annotation_scale() +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_orienteering) +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = scales::alpha("white", 0.8), colour = NA),
        legend.position = "right",
        legend.justification = "top",
        legend.box = "vertical",
        plot.margin = margin(5, 80, 5, 5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  annotate('text', x = 523100, y = 3124040, label = 'India', angle = -50, size = 6)+
  annotate('text', x = 527100, y = 3125040, label = 'Nepal', angle = -50, size = 6)
mymap

world_map = map_data("world")

distinct(world_map, region) %>% 
  ggplot(aes(map_id = region)) +
  geom_map(map = world_map) +
  expand_limits(x = world_map$long, y = world_map$lat)

nepalmap = world_map[world_map$region %in% c('Nepal','India', 'China', 'Bangladesh'),]

dff <- nepalmap %>%
  group_by(region) %>%
  summarise(long = mean(long, na.rm = T), lat = mean(lat, na.rm = T))

dff = dff[-1,]
dff$long = c(86,82,84)
dff$lat = c(29, 27,28.3)

#we want the long/lats to be quite different

inset = distinct(nepalmap, region) %>% 
  ggplot(aes(map_id = region)) +
  geom_map(map = nepalmap, color = 'white', linewidth = 1, fill = 'grey') +
  expand_limits(x = nepalmap$long, y = nepalmap$lat) +
  scale_x_continuous(limits = c(79,88.5),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(25.9,30.5),
                     expand = c(0, 0)) +
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 3),
        panel.background = element_blank(),
        plot.margin=unit(c(0,0,-0.1,-0.1), 'cm'))+
  geom_text(data = dff, 
            aes(long, lat, label = region),
            size = 8, angle = -40) +
  geom_rect(aes(xmin = 81.15, xmax = 81.8,
                ymin = 28.2, ymax = 28.7),
            color = "black", fill = NA)

inset

#library(patchwork)
mymap + inset_element(inset, left = 0.69, bottom = 0.01, right = 0.99, top = 0.28)
ggsave("Output/FieldMap_2.pdf", width = 13, height = 9, dpi = 300)
