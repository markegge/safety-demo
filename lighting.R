# How does Roadway Lighting Condition Affect Safety?
# 2 May 2020
# Author: Mark Egge (mark@eateggs.com)

# options(scipen = 99) # disable scientific notation

# DATA TRANSFORMATION -------------------------------------------------------------------

# Safety analysis typically uses five years' worth of historic crash data
# Load in the road segment shapefile and the PA Crash CSVs
# Downloaded five years of data from https://crashinfo.penndot.gov/PCIT/welcome.html
# Read in crash data files for 2012 – 2016
library(data.table) # fast data manipulation
crashes <- list()
for(year in as.character(2012:2016)) {
  crashes[[year]] <- fread(paste0("data/Statewide_", year, "/CRASH_", year, "_Statewide.csv"))
}
crashes <- rbindlist(crashes) # join the five files into one

# Filter to only crashes with "2 - Dark - No Street Lights" or "3 - Dark - Street Lights"
# See doc/PA_Crashes_Data_Dictionary.pdf for details
crashes <- crashes[ILLUMINATION %in% c(2, 3)]
crashes[ILLUMINATION == 2, crash_lighting := "unlit"]
crashes[ILLUMINATION == 3, crash_lighting := "lit"]


# GIS --------------------------------------------------------------------------
# read in shapefile of Pennsylvania freeway road segments based on HPMS 2018
# Source: https://geo.dot.gov/server/rest/services/Hosted/Pennsylvania_2018_PR/FeatureServer/0
library(sf) # spatial data manipulation
segments <- st_read("shp/traffic/traffic.shp") # all st_* functions are from the sf package
segments$id <- 1:nrow(segments) # assign a unique identifier for each segment

#' SPATIAL JOIN TO CALCULATE CRASH COUNTS AND RATES ----------------------------
#' Spatially join crashes to road segments, count the crashes, and join back

# Create a sf spatial object from crashes using the the crash data longitude and latitude fields.
crashes <- st_as_sf(crashes, coords = c("DEC_LONG", "DEC_LAT"), crs = 4326, na.fail = FALSE)

# Reproject from Web Mercator lat/long coordinates to PA State Plane EPSG:2272 NAD83 / Pennsylvania South (ft)
crashes <- st_transform(crashes, 2272) # reproject data to EPSG:2272

# Buffer the road segments by 50 feet
segment_buffer <- st_buffer(segments, dist = 50, endCapStyle = "FLAT")

# Spatially join the crashes to the buffered segments
joined <- st_join(segment_buffer, crashes, left = FALSE) # inner join

# Convert sf spatial object to a data.table for aggregation
segment_crashes <- as.data.table(st_drop_geometry(joined))

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

# count total crashes by segment id and illumination condition
counts <- segment_crashes[, .(crashes = .N), by = .(id, crash_lighting)]

# Reshape long to wide, filling missing values with zero
lighting_counts <- dcast(counts, id ~ crash_lighting, value.var = "crashes", fill = 0)  

# assign lighting based on majority of crashes (lit or unlit)
lighting_counts[, lighting := as.factor(ifelse(lit >= unlit, "lit", "unlit"))] 

# Count annual crashes by segment
lighting_counts[, total_crashes := lit + unlit]
# Annual Crahes = total crashes divided by five years
lighting_counts[, crashes := total_crashes / 5]

# Join imputed lighting conditions back to segments sf
segments <- merge(segments, lighting_counts, by = "id", all.x = TRUE)

# Calculate crash rates - crashes per million VMT
segments[is.na(segments$crashes), ]$crashes <- 0 # segments without any crashes have 0 crashes
segments$mvmt <- with(segments, (DLY_VMT * 365) / 1000000) # million annual vehicle miles travelled
segments$rate <- with(segments, crashes / mvmt) # annual crashes per 1m annual vmt

# DECISION TREES --------------------------------------------------------------
#' FILL MISSING SEGMENT ILLUMINATION VALUES WITH A DECISION TREE ---------------
# 
# Train a decision tree which assigns lighting to
# segments without a crash history based on attributes like length and volume

# K-Nearest Neighbors would also provide a great way to fill missing lighting values

# Table of "lit", "unlit", and "unknown" lighting_conditon attributes
cat("Before:"); table(segments$lighting, useNA = "ifany")

library(rpart) # decision tree models
# For segments without an imputed lighting_condition, predict
# which road segments are illuminated using an rpart decision tree
fit <- rpart(lighting ~ ST_RT_NO + CTY_CODE + DISTRICT_N + SEG_LNGTH_ + CUR_AADT,  data = segments)
# Classifier performance evaluation omitted for the sake of brevity; moderate fit

# use decision tree to fill in missing lighting values
predictions <- predict(fit, type = "class", newdata = segments[is.na(segments$lighting), ])
segments[is.na(segments$lighting), ]$lighting <- predictions

# Table of "lit", "unlit", and "unknown" lighting_conditon attributes
cat("After"); table(segments$lighting, useNA = "ifany")


# Decision Trees are a great tool for revealing structural relationships in data

# What explains / predicts crash counts? A decision tree:
fit <- rpart(crashes ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, data = DT)
rpart.plot(fit, digits = -1)

# What explains / predicts crash rates?
fit <- rpart(rate ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, data = DT)
rpart.plot(fit, digits = -1)



# PLOTTING ---------------------------------------------------
#' In this section we map, plot, and summarize our analysis dataset to better
#' understand what relationships may existing within the data. The findings from
#' this section inform the subsequent modeling section.

# MAP LIGHTING TYPES
# limit to Pittsburgh region and reproject to web mercator for leaflet
library(leaflet) # easy mapping in R
leaflet_segments <- st_transform(segments[segments$DISTRICT_N %in% c("11", "12"), ], 4326)

pal <- colorFactor(c("yellow", "brown", "orange"), leaflet_segments$lighting)
leaflet(data = leaflet_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(lighting)) %>%
  addLegend(pal = pal, values = ~lighting)

# map crash rates
qpal <- colorQuantile(palette = "YlOrRd", domain = leaflet_segments$rate, n = 6)
leaflet(data = leaflet_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~qpal(rate)) %>%
  addLegend(pal = qpal, values = ~rate, opacity = 1)

# convert spatial object to data.table for easy data manipulation
DT <- as.data.table(st_drop_geometry(segments))

# plot crash counts vs. vmt
# relationship between crashes and vmt seems fairly linear
library(ggplot2)
ggplot(DT, aes(x = mvmt, y = crashes, color = lighting)) +
  geom_point(alpha = 1/5, stroke = 0, size = 1.5) + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Crash Count vs. VMT")

# box plot of crash rates (limiting to rate < 5 for visualization)
ggplot(DT[rate < 5], aes(x = lighting, y = rate)) + 
  geom_boxplot()

# are lighting and AADT correlated?
cor(DT$DLY_VMT, DT$lighting == "lit") # Yes - weak negative correlation

hist(DT$mvmt) # VMT has an exponential distribution
hist(DT$crashes) # annual crashes has similar distribution
hist(log(DT$mvmt))
ggplot(DT) + 
  geom_density(aes(x = mvmt), color = "blue", show.legend = TRUE) + 
  geom_density(aes(x = crashes), color = "purple", show.legend = TRUE) + 
  coord_cartesian(xlim = c(0, 30))


# MODELLING      ---------------------------------------------------------------
DT[, total_crashes := as.integer(crashes * 5)] # total 5 year crashes for count model

library(pscl) # Poisson and zero-inflated regression

# fit a zero inflated negative binomial model
fit <- pscl::zeroinfl(total_crashes ~ lighting + mvmt | mvmt, data = DT)
summary(fit)

# what does this look like?
synth <- data.table(lighting = c(rep("lit", 100), rep("unlit", 100)),
                    mvmt = seq(0.1, 70, length.out = 100))
fit_aadt <- lm(CUR_AADT ~ poly(mvmt, 3), data = DT)
synth$CUR_AADT <- predict(fit_aadt, newdata = synth)
synth$total_crashes <- predict(fit, newdata = synth)
ggplot(synth, aes(x = mvmt, y = total_crashes, color = lighting)) + 
  geom_line() +
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

