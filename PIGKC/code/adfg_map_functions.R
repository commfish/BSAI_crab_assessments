# notes ----
## miscellaneous custom functions for map making 
## Tyler Jackson
## tyler.jackson@alaska.gov
## sources: 
### https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles
### https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
## last updated 2020/2/19

## source functions within a separate script
## see annotation below for arguments

# load ----
library(sp)
library(RANN)
library(rgdal) # requires sp, will use proj.4 if installed
library(maptools)
library(tidyverse)
library(gpclib); gpclibPermit()

# functions ----

#path <- "./data/maps/statewide_scallop_survey_grid_2019"
#layer <- "scalGrid2019_all_albers"

## read shapefile into R to draw a polygon in ggplot2
### arugment path is the file path to the directory housing the shapefiles
### arugment layer is the name of shapfiles, minus the extension
### output: List. First element is a dat frame to draw a polygon, second element is the projection string
f_shp_prep <- function(path, layer){

shp <- readOGR(dsn = path, layer = layer)
proj_4_string <- proj4string(shp)
shp@data$id <- rownames(shp@data)
shp.points <- fortify(shp, region = "id")
list(plyr::join(shp.points, shp@data, by = "id"),
     proj_4_string)
}

## transfom AK polygon data projection
### arugment x is output of f_shp_prep (both list objects, data and projection string)
### arguments lat and long are names of lat and long fields
### arugment to is the projection string to transform data to
f_transform_crs <- function(x, longitude = "long", latitude = "lat", to){
  # isolate lat and lon
  x[[1]] %>%
    dplyr::select(longitude, latitude) -> xy
  # set up CRS string for transformation
  crs <- CRS(x[[2]])
  # complete the transformation
  g <- spTransform(SpatialPoints(xy, proj4string = crs), 
                   CRS(to))
  # replace coords
  x[[1]][, c(longitude, latitude)] <- coordinates(g)
  # replace projection string
  x[[2]] <- to
  # print!
  x
}

## create a fillable grid based on lat and long
## fill data is joined with grid via nearest neighbor algorithm and summed within grid cell
## args:
### lat - list of min and max latitude of entire polygon
### lon - list of min and max longitude of entire polygon
### by - list of increment (in degrees) for grid height and width (long, lat)
### join - optional. The data frame to join to grid data containing values to fill grid. Must 
###        contain columns 'lat' and 'long'.
### values - names of column in 'join' data frame containing values to fill grid
f_make_grid <- function(lat, long, by, join, values){
  # make the empty grid
  expand_grid(long = seq(long[1], long[2], by[1]),
              lat = seq(lat[1], lat[2], by[2])) %>%
    SpatialPointsDataFrame(coords = ., data =.) -> tmp
  gridded(tmp) <- T
  tmp %>%
    as(., "SpatialPolygons") %>%
    SpatialPolygonsDataFrame(., data = data.frame(id = row.names(.),
                                                  row.names = row.names(.))) %>%
    fortify(., region = "id") -> x
  x %>%
    group_by(id) %>%
    select(-order) %>%
    distinct() %>%
    summarise(cent_long = mean(long),
              cent_lat = mean(lat)) -> x_tmp
  x_tmp %>%
    right_join(x, by = "id") %>%
    select(long, lat, order, hole, piece, group, id, cent_long, cent_lat) -> x
  # join with fill data
  if(!missing(join)){
    join %>%
      select(long, lat) %>%
      as.matrix(.) %>%
      spDists(., y = as.matrix(select(x_tmp, 2, 3))) %>%
      as_tibble() -> tmp
    names(tmp) <- x_tmp$id
    nn <- apply(tmp, 1, function(x){names(which.min(x))})
    join %>%
      bind_cols(id = nn) %>%
      select(id, values) %>%
      group_by(id) %>%
      mutate_at(2, sum) %>%
      distinct %>%
      right_join(x, by = "id") %>%
      select(long, lat, order, hole, piece, group, id, cent_long, cent_lat, values)
  } else {x}
}


