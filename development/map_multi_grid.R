rating.df <- bibi.sub %>% 
  filter(spatial == "region",
         period == "2000_2008")
huc12.prep <- prep_poly(huc12.poly, "huc12") %>% 
  prep_grid(rating.df, period, huc12)

huc8.prep <- prep_poly(huc8.poly, "huc8") %>% 
  prep_grid(rating.df, period, huc8)

huc6.prep <- prep_poly(huc6.poly, "huc6") %>% 
  prep_grid(rating.df, period, huc6)

catchment.prep <- prep_poly(catchment.poly, "catchment") %>% 
  prep_grid(rating.df, period, catchment)

huc8.prep$color <- ifelse(huc8.prep$id %in% c("02080108", "02080208", "02050303"), "selected", "unselected")

ggplot() +
  geom_polygon(data = huc12.prep, aes(long, lat, group = group, fill = Samples), color = "black") +
  scale_fill_manual(values = c("Absent" = "#999999", "Present" = "#0072B2")) +
  #geom_polygon(data = huc6.prep, aes(long, lat, group = group, fill = NA), color = "orange") +
  geom_polygon(data = huc8.prep, aes(long, lat, group = group, fill = NA, color = color)) +
  geom_text(data = huc8.prep, aes(long, lat, label = id)) +
  #scale_color_manual(values = c("selected" = "red", "unselected" = "black")) +
  coord_equal() +
  theme_bw() +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
