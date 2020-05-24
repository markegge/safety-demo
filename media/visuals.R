library(leaflet) # easy mapping in R
# Subset to Pittsburgh region
# Reproject from StatePlane to Web Mercator for Leaflet


# leaflet(leaflet_segments) %>%
#   addProviderTiles(providers$CartoDB.DarkMatter) %>% 
#   addPolylines(color = ~pal(lighting), group = "Lighting") %>%
#   addLegend(group = "Lighting", pal = pal, values = ~lighting, 
#             title = "Roadway Lighting", labels = c("Illuminated", "Unilluminated"))



leaflet_segments <- st_transform(segments[segments$DISTRICT_N %in% c("11", ], 4326)
lb_segments <- st_transform(segment_buffer[segments$DISTRICT_N %in% c("11"), ], 4326)
crash <- st_transform(crashes[which(crashes$DISTRICT %in% c("11") & crashes$CRASH_YEAR >= 2016), ], 4326)

leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(data = lb_segments, color = "orange", group = "Buffered Segments") %>%
  addPolylines(data = leaflet_segments, color = "brown", opacity = 0.8,
               weight = 1, group = "Road Segments") %>%
  addCircles(data = crash, weight = 2, radius = 2, 
             opacity = 0.9, color = "yellow", group = "2016 Crahes") %>%
  addLegend(colors = c("orange", "brown", "yellow"), labels = c("Buffered Segments", "Road Segments", "2016 Crashes"))



ss <- segments
ss$ST_RT_NO <- as.numeric(ss$ST_RT_NO)
ss$CTY_CODE <- as.numeric(ss$CTY_CODE)
rpart.plot(rpart(lighting ~ ST_RT_NO + CTY_CODE + DISTRICT_N + SEG_LNGTH_ + CUR_AADT,  
             data = ss))

