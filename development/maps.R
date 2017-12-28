library("rgdal")
library("rgeos")
library("maps")
shp <- readOGR("D:/ZSmith/Projects/Chessie_BIBI/bibi_baseline/bibi_baseline/data/gis/bibi_shapefile/ICPRB_Bioregions.shp")
proj4string(shp)

### define SpatialGrid object
bb <- bbox(shp)
cs <- c(3.28084, 3.28084)*1000  # cell size 6km x 6km (for illustration)
# 1 ft = 3.28084 m
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
grd
# cellcentre.offset 923018 129964
# cellsize           19685  19685
# cells.dim              8      8

sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(shp)))

bound <- SpatialPolygons(shp@polygons, proj4string=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
clip_grid = sp_grd[!is.na(over(sp_grd, bound)),]
plot(clip_grid)

test <- bibi.sub
coordinates(test) <- ~ longitude + latitude
proj4string(test) <- CRS("+init=epsg:4269")
CRS.new <- CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
test <- spTransform(test, CRS.new)






test$polygon_id = over(test, clip_grid)$id


test2 <- test@data %>% 
  group_by(spatial, subspatial, polygon_id, huc_12) %>% 
  summarize(count = n())

library("lattice")
spplot(sp_grd, "id", colorkey=FALSE,
       panel = function(...) {
         panel.gridplot(..., border="black")
         sp.polygons(shp)
         sp.points(blah, cex=1.5)
         #panel.text(..., col="red")
       })
test5 <- bibi.sub %>% 
  group_by(station_id) %>% 
  summarize(count = n())

blah <- subset(test, polygon_id == 19891)
plot(blah)
test3 <- test@data %>% 
  filter(polygon_id == 19891)
coordinates(test3) <- ~ longitude + latitude
proj4string(test3) <-CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
