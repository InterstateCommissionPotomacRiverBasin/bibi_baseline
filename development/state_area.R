library(rgdal)
library(rgeos)
library(maps)
#------------------------------------------------------------------------------
shp <- readOGR("D:/ZSmith/Projects/Chessie_BIBI/bibi_baseline/bibi_baseline/data/gis/bibi_shapefile/ICPRB_Bioregions.shp")
proj4string(shp)
#test <- subset(shp, shp$Basin != "Bay")
shp2 <- gUnaryUnion(shp, id = shp@data$Basin)

#------------------------------------------------------------------------------
counties.df <- map_data("county", c("Virginia", "Maryland"))



states.vec <- c('Maryland', "Virginia", "West Virginia",
                "Delaware", "Pennsylvania", "New York",
                "District of Columbia")
states.df <- map_data('state', region = states.vec) %>% 
  unite(gregion, region, subregion, remove = FALSE)
library(sp)


sp <- lapply(unique(states.df$gregion), function(region.i) {
  list(Polygons(list(Polygon(states.df[states.df$gregion %in% region.i, 1:2])), ID = region.i))
}) %>% unlist() %>% SpatialPolygons()
class(sp)
proj4string(sp) <- CRS("+init=epsg:4269")
CRS.new <- CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
sp <- spTransform(sp, CRS.new)

sp2 <- SpatialPolygonsDataFrame(sp,
                                data.frame(id = unique(states.df$gregion), 
                                           row.names = unique(states.df$gregion)))
#------------------------------------------------------------------------------
ggplot() +
  geom_polygon(data = shp, aes(long, lat, group = group)) +
  geom_polygon(data = sp2,
               aes(x = long, y = lat, group = group), alpha = 1, fill = "red") +
  coord_equal() 
#------------------------------------------------------------------------------
shp2 <- gBuffer(shp2, width = 0, byid = TRUE) 
sp2 <- gBuffer(sp2, width = 0, byid = TRUE) 
clip <- gIntersection(sp2, shp2, byid = TRUE) 
clip <- SpatialPolygonsDataFrame(clip,
                                data.frame(id = names(clip), 
                                           row.names = names(clip)))
ggplot() +
  geom_polygon(data = shp2, aes(long, lat, group = group), fill = "blue", color = "black") +
  geom_polygon(data = clip,
               aes(x = long, y = lat, group = group), alpha = 1, fill = "red", color = "black") +
  coord_equal() 
#------------------------------------------------------------------------------

test <- lapply(unique(clip$id), function(i) {
  final.df <- data.frame(id = i)
  final.df$area <- gArea(clip[clip$id == i, ])
  final.df
}) %>% 
  bind_rows()

test2 <- test %>% 
  mutate(state = case_when(
    str_detect(id, "district") ~ "district of columbia",
    str_detect(id, "delaware") ~ "delaware",
    str_detect(id, "maryland") ~ "maryland",
    str_detect(id, "virginia") ~ "virginia",
    str_detect(id, "west virginia") ~ "west virginia",
    str_detect(id, "pennsylvania") ~ "pennsylvania",
    str_detect(id, "new york") ~ "new york",
    TRUE ~ "ERROR"
  )) %>% 
  group_by(state) %>% 
  summarize(area = sum(area))
