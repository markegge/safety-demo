#' How does Roadway Lighting Condition Affect Safety?
#' 2 May 2020
#' Author: Mark Egge (mark@eateggs.com)

library(sf) # spatial data manipulation
library(data.table) # fast data manipulation
library(leaflet) # easy mapping in R
library(ggplot2) # plotting
library(rpart); library(rpart.plot) # decision tree models
library(MASS); library(pscl) # Poisson and zero-inflated regression

source("R/summary_plots.R") # function for creating summary plots

options(scipen = 99) # disable scientific notation

#' LOAD DATA -------------------------------------------------------------------
#' Load in the road segment shapefile and the PA Crash CSVs

# read in shapefile of Pennsylvania freeway road segments based on HPMS 2018
# Source: https://geo.dot.gov/server/rest/services/Hosted/Pennsylvania_2018_PR/FeatureServer/0
segments <- st_read("shp/traffic/traffic.shp") # all st_* functions are from the sf package
segments$id <- 1:nrow(segments) # assign a unique identifier for each segment

# Safety analysis typically uses five years' worth of historic crash data
# Read in crash data files for 2012 – 2016
crashes <- list()
for(year in as.character(2012:2016)) {
  crashes[[year]] <- fread(paste0("data/Statewide_", year, "/CRASH_", year, "_Statewide.csv"))
}
crashes <- rbindlist(crashes) # join the five files into one

suppressWarnings(summary_plots(crashes, "out/plots.pdf")) # create summary plots

# Filter to only crashes with "2 - Dark - No Street Lights" or "3 - Dark - Street Lights"
# See doc/PA_Crashes_Data_Dictionary.pdf for details
crashes <- crashes[ILLUMINATION %in% c(2, 3)]
crashes[ILLUMINATION == 2, crash_lighting := "unlit"]
crashes[ILLUMINATION == 3, crash_lighting := "lit"]


#' SPATIALLY JOIN CRASHES TO SEGMENTS -------------------------------------------
#' Create a sf spatial object using the the crash data longitude and latitude fields.
#' Buffer the road segments by 50', then spatially join the crashes to the segments.
#' Spatial Projection: EPSG:2272 NAD83 / Pennsylvania South (ft)

# Create spatial object from crash data using sf
crashes <- crashes[!(is.na(DEC_LONG) | is.na(DEC_LAT))] # keep only records with x, y values
crashes <- st_as_sf(crashes, coords = c("DEC_LONG", "DEC_LAT"), crs = 4326) # create sf spatial object
crashes <- st_transform(crashes, 2272) # reproject data to EPSG:2272

# buffer road segments by 50 feet
segment_buffer <- st_buffer(segments, dist = 50, endCapStyle = "FLAT")

# spatially join crashes to road segment data to crashes
joined <- st_join(segment_buffer, crashes, left = FALSE) # inner join

segment_crashes <- as.data.table(st_drop_geometry(joined)) # convert sf to data.table

# count annual crashes by segment
crash_counts <- segment_crashes[, .(annual_crashes = .N / 5), by = id]  

# merge crash counts back to segments
segments <- merge(segments, crash_counts, by = "id", all.x = TRUE)

# calculate crash rates - crashes per million VMT
segments[is.na(segments$annual_crashes), ]$annual_crashes <- 0
segments$mvmt <- with(segments, (DLY_VMT * 365) / 1000000) # million annual vehicle miles travelled
segments$rate <- with(segments, annual_crashes / mvmt) # annual crashes per 1m annual vmt


#' SEGMENT ILLUMINATION IMPUTATION --------------------------------------------
#' Our roadway dataset does not include information about overhead lighting.
#' The section below uses crash data to impute a "lighting" attribute for our
#' road segments.
#' 
#' Where a roadway has multiple crashes, the lighting attribute is assigned
#' based on predominant value reported for associated crashes.
#' 
#' Then, this data is used to train a decision tree, which assigns lighting to
#' segments without a crash history based on attributes like length and volume

# IMPUTE which road segments are illuminated based on crash records 

# count total crashes by segment id and illumination condition
counts <- segment_crashes[, .(crash_count = .N), by = .(id, crash_lighting)]

segment_counts <- dcast(counts, id ~ crash_lighting,          # long to wide transform
                        value.var = "crash_count", fill = 0)  # fill missing values with zero

# assign segment_lighting based on majority of crashes (lit or unlit)
segment_counts[, segment_lighting := ifelse(lit >= unlit, "lit", "unlit")] 

# join the imputed roadway lighting condition to all road segments
light <- merge(as.data.table(st_drop_geometry(segments)), # create data.table from segments attributes
               segment_counts[, .(id, segment_lighting)], # join result, keeping only segment_lighting field
               by = "id", all.x = TRUE) # join by id, keeping all segments

# Table of "lit", "unlit", and "unknown" lighting_conditon attributes
table(light$segment_lighting, useNA = "ifany")

# For segments with unknwon lighting_condition, predict
# which road segments are illuminated using an rpart decision tree
# light[light$urban_code < 99998, ]$urban_code <- 1
fit <- rpart(segment_lighting ~ ST_RT_NO + CTY_CODE + DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, 
             data = light)
# Evaluate our goodness of fit
# rpart.plot(fit)
# summary(fit)
# create a confusion matrix to evaluate classifier performance
actual <- light[!is.na(segment_lighting), segment_lighting]
predicted <- predict(fit, type = "class")
(cm <- as.matrix(table(Actual = actual, Predicted = predicted))) # create the confusion matrix
(accuracy <- sum(diag(cm)) / sum(cm) ) # number of correctly classified instances per class  / number of instances
p <- rowSums(cm) / sum(cm); q <- colSums(cm) / sum(cm)
(kappa <- (accuracy - sum(p * q)) / (1 - sum(p * q))) # kappa score indicates moderate agreement

# use decision tree to fill in missing lighting values
predictions <- predict(fit, type = "class", newdata = light[is.na(segment_lighting)])
light[is.na(segment_lighting)]$segment_lighting <- predictions


# join lighting attribute back to road segments
segments <- merge(segments, light[, .(id, lighting = segment_lighting)], by = "id", all.x = TRUE)

# map lighting types
# limit to Pittsburgh region and reproject to web mercator for leaflet
leaflet_segments <- st_transform(segments[segments$DISTRICT_N %in% c("11", "12"), ], 4326)

pal <- colorFactor(c("yellow", "brown", "orange"), leaflet_segments$lighting)
leaflet(data = leaflet_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(lighting)) %>%
  addLegend(pal = pal, values = ~lighting)

#' EXPLORATORY DATA ANALYSIS ---------------------------------------------------
#' In this section we map, plot, and summarize our analysis dataset to better
#' understand what relationships may existing within the data. The findings from
#' this section inform the subsequent modeling section.

# convert spatial object to data.table for easy data manipulation
DT <- as.data.table(st_drop_geometry(segments))
# DT <- DT[!is.na(rate)] # one weird segment has no geom or rate

# plot crash counts vs. vmt
# relationship between crashes and vmt seems fairly linear
ggplot(DT, aes(x = mvmt, y = annual_crashes)) +
  geom_point() + ggtitle("Crash Count vs. VMT")

# map crash rates
qpal <- colorQuantile(palette = "YlOrRd", domain = leaflet_segments$rate, n = 6)
leaflet(data = leaflet_segments) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~qpal(rate)) %>%
  addLegend(pal = qpal, values = ~rate, opacity = 1)


# some descriptive plots
hist(DT$rate)
hist(log(DT$rate))
hist(DT[rate < 50]$rate, breaks = 50)


# What explains / predicts crash counts? A decision tree:
fit <- rpart(annual_crashes ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT,
             data = DT)
# summary(fit)
rpart.plot(fit)

# What explains / predicts crash rates?
fit <- rpart(rate ~ DISTRICT_N + SEG_LNGTH_ + CUR_AADT + DLY_VMT, data = DT)
# summary(fit)
rpart.plot(fit)

# box plot of crash rates
boxplot(rate ~ lighting, data = DT[rate < 2])

# are lighting and AADT related?
cor(DT$DLY_VMT, DT$lighting == "lit") # Yes - weak negative correlation

hist(DT$mvmt) # VMT has an exponential distribution
hist(DT$annual_crashes) # annual crashes has similar distribution
hist(log(DT$mvmt))

# MODELLING      ---------------------------------------------------------------
DT[, total_crashes := as.integer(annual_crashes * 5)] # total 5 year crashes for count model (whole numbers)

# fit a Poisson regression model
fit <- glm(total_crashes ~ lighting + mvmt, data = DT, family = poisson)
summary(fit)
# goodness of fit measure:
1 - pchisq(summary(fit)$deviance, summary(fit)$df.residual) # model does not fit the data (p < 0.05)

# fit a negative binomial regression model
fit <- MASS::glm.nb(total_crashes ~ lighting + mvmt, data = DT)
summary(fit)
confint(fit)
# goodness of fit measure:
1 - pchisq(summary(fit)$deviance, summary(fit)$df.residual) # model does not fit the data

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
       


#' FINDINGS --------------------------------------------------------------------
#' 
#' The zero-inflated negative binomial model outputs show that unlit road segments 
#' have 0.39 fewer crashes on average than lit segments, controlling for VMT.
#' 
#' This finding is rather counter-intuitive from a "lighting improves safety" 
#' perspective, but makes sense if lighting is installed in locations with higher
#' crash risk (e.g. intersections). 
#' 

