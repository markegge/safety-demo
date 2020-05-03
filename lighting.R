#' How does Roadway Lighting Condition Affect Safety?
#' 2 May 2020
#' Author: Mark Egge (mark@eateggs.com)

library(data.table) # fast data manipulation
library(sf) # spatial data manipulation
library(leaflet) # easy mapping in R
library(ggplot2) # plotting

source("R/summary_plots.R") # function for creating summary plots

options(scipen = 99) # disable scientific notation

# LOAD DATA --------------------------------------------------------------------

# read in road segments sf object for Pennsylvania
# road segment attributes based on HPMS 2018
# Source: https://geo.dot.gov/server/rest/services/Hosted/Pennsylvania_2018_PR/FeatureServer/0
segments <- st_read("shp/roads/pa_2018.shp")
segments$id <- 1:nrow(segments)

# read in each year's crash file
# data source: https://pennshare.maps.arcgis.com/apps/webappviewer/index.html?id=8fdbf046e36e41649bbfd9d7dd7c7e7e
crashes <- list()
for(year in as.character(2012:2016)) {
  crashes[[year]] <- fread(paste0("data/Statewide_", year, "/CRASH_", year, "_Statewide.csv"))
}

crashes <- rbindlist(crashes) # join the five files into one

summary_plots(crashes, "out/plots.pdf") # create summary plots

# Filter to only crashes with:
#  * 2 - Dark - No Street Lights or 3 - Dark - Street Lights
# See doc/PA_Crashes_Data_Dictionary.pdf for details
crashes <- crashes[ILLUMINATION %in% c(2, 3)]
crashes[ILLUMINATION == 2, crash_lighting := "unlit"]
crashes[ILLUMINATION == 3, crash_lighting := "lit"]


# SPATIALLY JOIN CRASHES TO SEGMENTS -------------------------------------------

# Create spatial object from crash data using sf
crashes <- crashes[!(is.na(DEC_LONG) | is.na(DEC_LAT))] # keep only records with x, y values
crashes <- st_as_sf(as.data.table(crashes), coords = c("DEC_LONG", "DEC_LAT"), crs = 4326)
crashes <- st_transform(crashes, 2272) # reproject data to EPSG:2272 NAD83 / Pennsylvania South (ftUS)

# buffer road segments by 50 feet
segment_buffer <- st_buffer(segments, dist = 50, endCapStyle = "FLAT")

# spatially join crashes to road segment data to crashes
j <- st_join(segment_buffer, crashes) # inner join

j <- as.data.table(st_drop_geometry(j)) # convert sf to data.table

crash_counts <- j[, .(crash_count = .N / 5), by = id]  # count annual crashes by segment

# merge crash counts back to segments
segments <- merge(segments, crash_counts, by = "id", all.x = TRUE)

# calculate rates
segments[is.na(segments$crash_count)]$crash_count <- 0
segments$mvmt <- with(segments, (aadt * 365 * miles) / 1000000) # million annual vehicle miles travelled
segments$rate <- with(segments, crash_count / mvmt) # annual crashes per 1m annual vmt


# IMPUTE which road segments are lit based on crash records --------------------
crash_counts_lighting <- j[, .(crash_count = .N),       # count total crashes
                           by = .(id, crash_lighting)]  # by segment id and illumination condition

light_counts <- dcast(crash_counts_lighting, id ~ crash_lighting,  # long to wide transform
                      value.var = "crash_count", fill = 0)         # filling missing values with zero
light_counts[`NA` == 0, segment_lighting := ifelse(lit >= unlit, "lit", "unlit")] # assign based on majority of crashes

# join the imputed roadway lighting condition back to all roads
light <- merge(segments, light_counts[, .(id, segment_lighting)], by = "id", all.x = TRUE) # tabular join, keep all segments
setDT(light)
table(light$segment_lighting, useNA = "ifany")

# Predict which road segments are illuminated using a decision tree
library(rpart); library(rpart.plot)
light[light$urban_code < 99998, ]$urban_code <- 1
fit <- rpart(segment_lighting ~ aadt + aadt_combi + aadt_singl + access_con + f_system + 
               facility_t + surface_ty + through_la + truck + urban_code + miles, 
             data = light)
# Evaluate our goodness of fit
rpart.plot(fit)
summary(fit)
# create a confusion matrix
actual <- light[!is.na(segment_lighting), segment_lighting]
predicted <- predict(fit, type = "class")
(cm <- as.matrix(table(Actual = actual, Predicted = predicted))) # create the confusion matrix
(accuracy <- sum(diag(cm)) / sum(cm) ) # number of correctly classified instances per class  / number of instances
p <- rowSums(cm) / sum(cm); q <- colSums(cm) / sum(cm)
(kappa <- (accuracy - sum(p * q)) / (1 - sum(p * q))) # moderate agreement

# use decision tree to fill in missing lighting values
predictions <- predict(fit, type = "class", newdata = light[is.na(segment_lighting)])
light[is.na(segment_lighting)]$segment_lighting <- predictions


# join lighting attribute back to road segments
segments <- merge(segments, light[, .(id, lighting = segment_lighting)], by = "id")


# EXPLORATORY DATA ANALYSIS ----------------------------------------------------

# map lighting types
pal <- colorFactor(c("yellow", "brown", "orange"), segments$lighting)
leaflet(data = st_transform(st_as_sf(segments), 4326)) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(lighting)) %>%
  addLegend(pal = pal, values = ~lighting)

# map crash rates
pal <- colorBin(palette = "YlOrRd", domain = segments$rate, 
                bins = c(0, 5, 10, 25, 75, 200, 600, 10000))
leaflet(data = st_transform(segments, 4326)) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(color = ~pal(rate)) %>%
  addLegend(pal = pal, values = ~rate)


DT <- as.data.table(segments)
DT <- DT[!is.na(rate)] # one weird segment has no geom or rate

# some descriptive plots
hist(DT$rate)
hist(log(DT$rate))
hist(DT[rate < 50]$rate, breaks = 50)

# plot crash counts vs. vmt
# relationship between crashes and vmt seems fairly linear
ggplot(DT, aes(x = mvmt, y = crash_count)) +
  geom_point()

# What explains / predicts crash counts? A decision tree:
fit <- rpart(crash_count ~ aadt + aadt_combi + aadt_singl + access_con + f_system + 
               facility_t + surface_ty + through_la + truck + urban_code + miles +  mvmt,
             data = DT)
summary(fit)
rpart.plot(fit)

# What explains / predicts crash rates?
fit <- rpart(rate ~ access_con + f_system + facility_t + surface_ty + urban_code, data = DT)
summary(fit)
rpart.plot(fit)

# box plot of crash rates
boxplot(rate ~ lighting, data = DT[rate < 2])

# are lighting and AADT related?
cor(DT$aadt, DT$lighting == "lit")
with(DT[f_system <= 2 & facility_t %in% c(1, 2)], cor(lighting == "lit", aadt)) # freeways only
boxplot(rate ~ lighting, data = (DT[f_system <= 2 & facility_t %in% c(1, 2) & rate < 5]))


hist(DT$mvmt) # VMT has an exponential distribution
hist(log(DT$mvmt))
hist(DT$crash_count)

# MODELLING      ---------------------------------------------------------------
library(MASS)
DT[, total_crashes := as.integer(crash_count * 5)] # total 5 year crashes for count model (whole numbers)

# fit a Poisson regression model
fit <- glm(total_crashes ~ lighting + mvmt, data = DT, family = poisson)
summary(fit)
# goodness of fit measure:
1 - pchisq(summary(fit)$deviance, summary(fit)$df.residual) # model does not fit the data

# fit a negative binomial regression model
fit <- MASS::glm.nb(total_crashes ~ lighting + mvmt, data = DT)
summary(fit)
confint(fit)
# goodness of fit measure:
1 - pchisq(summary(fit)$deviance, summary(fit)$df.residual) # model fits the data


#' FINDINGS --------------------------------------------------------------------
#' 
#' The negative binomial model outputs show that unlit road segments 
#' have 0.44 fewer crashes on average than lit segments, controlling for VMT.
#' 
#' This finding is rather counter-intuitive from a "lighting improves safety" 
#' perspective, but makes sense if lighting is installed in locations with higher
#' crash risk (e.g. intersections).
#' 
