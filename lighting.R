# How does Roadway Lighting Condition Affect Safety?
# 2 May 2020
# Author: Mark Egge (mark@eateggs.com)

# options(scipen = 99) # disable scientific notation

# DATA TRANSFORMATION -------------------------------------------------------------------

# Safety analysis typically uses five years' worth of historic crash data
# See doc/PA_Crashes_Data_Dictionary.pdf for details

# Read in PA Crash CSVs for 2012 – 2016
# Downloaded from https://crashinfo.penndot.gov/PCIT/welcome.html
library(data.table) # fast data manipulation
crashes <- list()
for(year in as.character(2012:2016)) {
  file_name <- paste0("data/Statewide_", year, "/CRASH_", year, "_Statewide.csv")
  crashes[[year]] <- data.table::fread(file_name)
}
crashes <- rbindlist(crashes) # join the five files into one

# Filter to only crashes with "2 - Dark - No Street Lights" or 
#                             "3 - Dark - Street Lights"
crashes <- crashes[ILLUMINATION %in% c(2, 3)]
crashes[ILLUMINATION == 2, crash_lighting := "unlit"]
crashes[ILLUMINATION == 3, crash_lighting := "lit"]


# GIS --------------------------------------------------------------------------
# read in shapefile of Pennsylvania freeway road segments based on HPMS 2018
# Source: https://geo.dot.gov/server/rest/services/Hosted/Pennsylvania_2018_PR/FeatureServer/0
library(sf) # spatial data manipulation
segments <- sf::st_read("shp/traffic/traffic.shp")
segments$id <- 1:nrow(segments) # assign a unique identifier for each segment

#' SPATIAL JOIN TO CALCULATE CRASH COUNTS AND RATES ----------------------------
#' Spatially join crashes to road segments, count the crashes, and join back

# Create a sf spatial object from crashes using the the crash data lat/lon fields.
crashes <- st_as_sf(crashes, coords = c("DEC_LONG", "DEC_LAT"), crs = 4326, na.fail = FALSE)

# Reproject from Web Mercator lat/lon coordinates to 
# PA State Plane EPSG:2272 NAD83 / Pennsylvania South (ft)
crashes <- st_transform(crashes, 2272) # reproject data to EPSG:2272

# Buffer the road segments by 50 feet
segment_buffer <- st_buffer(segments, dist = 50, endCapStyle = "FLAT")

# Spatially join the crashes to the buffered segments
joined <- st_join(segment_buffer, crashes, left = TRUE)


# TABULATE CRASHES AND IMPUTE LIGHTING
#' Our roadway segments dataset does not include information about overhead lighting.
#' We can use the data in the crash dataset (which describes the lighting condition)
#' to infer if a roadway has overhead illumination.
#' 
#' The section below uses crash data to impute a "lighting" attribute for our
#' road segments.
#' 
#' Where a roadway has multiple crashes, the lighting attribute is assigned
#' based on predominant value reported for associated crashes.

# Convert sf spatial object to a data.table for aggregation
segment_crashes <- as.data.table(st_drop_geometry(joined))

# count total crashes by segment id and illumination condition
counts <- segment_crashes[, .(crashes = .N), by = .(id, crash_lighting)]

# Reshape long to wide, filling missing values with zero
lighting_counts <- dcast(counts, id ~ crash_lighting, value.var = "crashes", fill = 0)  

# Count annual crashes by segment
lighting_counts[, total_crashes := lit + unlit]

# Annual Crahes = total crashes divided by five years
lighting_counts[, crashes := total_crashes / 5]

# Assign lighting based on majority of crashes (lit or unlit)
lighting_counts[total_crashes > 0,
                lighting := as.factor(ifelse(lit >= unlit, "lit", "unlit"))] 

# Join imputed lighting conditions back to segments sf
segments <- merge(segments, lighting_counts, by = "id")

# Calculate crash rates - crashes per million VMT
segments$mvmt <- segments$DLY_VMT * 365 / 1000000 # million annual vehicle miles travelled
segments$rate <- segments$crashes / segments$mvmt # annual crashes per 1m annual vmt

# DECISION TREES --------------------------------------------------------------
#' FILL MISSING SEGMENT ILLUMINATION VALUES WITH A DECISION TREE ---------------
# 
# Train a decision tree which assigns lighting to
# segments without a crash history based on attributes like length and volume

# K-Nearest Neighbors would also provide a great way to fill missing lighting values

# Table of "lit", "unlit", and "unknown" lighting_conditon attributes
cat("Before:"); table(segments$lighting, useNA = "ifany")

library(rpart); library(rpart.plot) # decision tree models
# For segments without an imputed lighting_condition, predict
# which road segments are illuminated using an rpart decision tree
fit <- rpart(lighting ~ ST_RT_NO + CTY_CODE + DISTRICT_N + SEG_LNGTH_ + CUR_AADT,  
             data = segments)
# Classifier performance evaluation omitted for the sake of brevity; moderate fit

# use decision tree to fill in missing lighting values
predictions <- predict(fit, type = "class", newdata = segments[is.na(segments$lighting), ])
segments[is.na(segments$lighting), ]$lighting <- predictions

# Table of "lit", "unlit", and "unknown" lighting_conditon attributes
cat("After"); table(segments$lighting, useNA = "ifany")

# Decision Trees are a great tool for revealing structural relationships in data

# What explains / predicts crash counts? A decision tree:
fit <- rpart(crashes ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, data = segments)
rpart.plot(fit, digits = -1)

# What explains / predicts crash rates?
fit <- rpart(rate ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, data = segments)
rpart.plot(fit, digits = -1)



# PLOTTING ---------------------------------------------------
#' In this section we map, plot, and summarize our analysis dataset to better
#' understand what relationships may existing within the data. The findings from
#' this section inform the subsequent modeling section.

# MAP LIGHTING TYPES
# limit to Pittsburgh region and reproject to web mercator for leaflet
library(leaflet) # easy mapping in R
# Subset to Pittsburgh region
leaflet_segments <- segments # segments[segments$DISTRICT_N %in% c("11", "12"), ]
# Reproject from StatePlane to Web Mercator for Leaflet
leaflet_segments <- st_transform(leaflet_segments, 4326)

leaflet(leaflet_segments) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(lighting), group = "Lighting") %>%
  addLegend(group = "Lighting", pal = pal, values = ~lighting, 
            title = "Roadway Lighting", labels = c("Illuminated", "Unilluminated"))


# Map lighting conditions and crash rates
pal <- colorFactor(c("yellow", "brown", "orange"), leaflet_segments$lighting)
qpal <- colorQuantile(palette = "YlOrRd", domain = leaflet_segments$rate, n = 7)
leaflet(leaflet_segments) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(lighting), group = "Lighting") %>%
  addPolylines(color = ~qpal(rate), group = "Crash Rates") %>%
  addLegend(group = "Lighting", pal = pal, values = ~lighting, position = "bottomright") %>%
  addLegend(pal = qpal, values = ~rate, group = "Crash Rates",
            labFormat = function(type, cuts, p) {
              n = length(cuts); p = paste0(round(p * 100), '%')
              cuts = paste0(round(cuts[-n], 2), " – ", round(cuts[-1], 2))
            }, 
            position = "bottomright", title = "Crash Rate") %>%
  addLayersControl(
    overlayGroups = c("Lighting", "Crash Rates"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Crash Rates")

# convert spatial object to data.table for easy data manipulation
DT <- as.data.table(st_drop_geometry(segments))

# plot crash counts vs. vmt
# relationship between crashes and vmt seems fairly linear
library(ggplot2)
ggplot(segments, aes(x = mvmt, y = crashes, color = lighting)) +
  geom_point(alpha = 1/5, stroke = 0, size = 1.5) + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Crash Count vs. VMT")

# box plot of crash rates (limiting to rate < 5 for visualization)
ggplot(segments, aes(x = lighting, y = rate)) + 
  geom_boxplot() + ylab("Crash Rate") + 
  ggtitle("Crashes per Million VMT by Lighting Condition") + 
  coord_cartesian(ylim = c(0, 3)) # zoom in

ggplot(segments) + 
  geom_density(aes(x = mvmt), color = "blue", show.legend = TRUE) + 
  geom_density(aes(x = crashes), color = "purple", show.legend = TRUE) + 
  coord_cartesian(xlim = c(0, 30))


# MODELING      ---------------------------------------------------------------
library(pscl) # Poisson and zero-inflated regression

# fit a zero inflated negative binomial model
fit <- pscl::zeroinfl(total_crashes ~ lighting + mvmt | mvmt, data = segments)
summary(fit)

# what does this look like?
synth <- data.table(lighting = c(rep("lit", 100), rep("unlit", 100)),
                    mvmt = seq(0.1, 70, length.out = 100))
fit_aadt <- lm(CUR_AADT ~ poly(mvmt, 3), data = segments)
synth$CUR_AADT <- predict(fit_aadt, newdata = synth)
synth$total_crashes <- predict(fit, newdata = synth)
ggplot() + 
  geom_point(data = segments, aes(x = mvmt, y = crashes, color = lighting),
             alpha = 1/5, stroke = 0, size = 1.5) + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  geom_line(data = synth, aes(x = mvmt, y = total_crashes, color = lighting)) +
  ggtitle("Estimated Crashes and VMT", "By Lighting Condition")


# FINDINGS --------------------------------------------------------------------
#' 
#' The zero-inflated negative binomial model outputs show that unlit road segments 
#' have 0.39 fewer crashes on average than lit segments, controlling for VMT.
#' 
#' This is an important reminder that correlation does not imply causation. This
#' result does not mean that roadway lighting causes crashes. The most plausible
#' explanation is that roadway lighting is installed in dangerous road sections
#' and our regression equation does not fully control for the other exogenous
#' factors such as intersections or other roadway conditions associated with greater
#' conflicts and crashes.
#' 
#' This finding is rather counter-intuitive from a "lighting improves safety" 
#' perspective, but makes sense if lighting is installed in locations with higher
#' crash risk (e.g. intersections). 
#' 

