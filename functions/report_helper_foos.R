# October 20, 2025
# functions for Water Quality Reports
# moved to here to decrease length of report script

# figure height -----------------------------------------------------------
# calculate the height of figure based on the number of measured variables

calc_fig_height <- function(dat, h1) {

  n_vars <- dat %>%
    ss_pivot_longer() %>%
    distinct(variable) %>%
    nrow()

  if(n_vars == 1) h = h1
  if(n_vars == 2) h = 2 * h1
  if(n_vars == 3) h = 3 * h1
  if(n_vars == 4) h = 4 * h1
  if(n_vars > 4) h = 5 * h1

  h
}


# river map ---------------------------------------------------------------
# generate map of river and associated sampling stations
# must have ggplot2, ggsflabel, and ggspatial loaded 

# ns_shp: shapefile (sf object) of Nova Scotia
# river_sf: sf object with river geometry 
# st_locations_sf: sf object with station geometry
# map_bbox: bbox object providing coordinates for map area
# map_buffer: numeric argument indicating how much buffer to map around the river
## or station coordinates bounding box

# text_size: numeric argument for size of text labels

# waterbody map
generate_river_map <- function(
    ns_shp, river_sf, st_locations_sf, map_bbox, map_buffer, text_size = 4
) {
  
  ggplot() +
    geom_sf(data = ns_shp, linewidth = 0.25) +
    scale_x_continuous(
      limits = c(map_bbox$xmin - map_buffer, map_bbox$xmax + map_buffer)
    ) +
    scale_y_continuous(
      limits = c(map_bbox$ymin - map_buffer, map_bbox$ymax + map_buffer)
    ) +
    #coord_sf(expand = FALSE, lims_method = "geometry_bbox", default_crs = NULL) +
    theme_map() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.title = element_blank(),
      legend.position = "none"
    ) +
    geom_sf(data = river_sf, color = "#00a4e4") +
    geom_sf(data = st_locations_sf, size = 3) +
    annotation_scale(location = "br") +
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      height = unit(1, "cm"),
      width = unit(1, "cm")
    ) +
    ggsflabel::geom_sf_text_repel(
      data = st_locations_sf,
      aes(label = station),
      seed = 12, size = text_size, color = "black", bg.color = "white"
    )
}
