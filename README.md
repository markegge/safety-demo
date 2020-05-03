# R for Transportation Data Analytics

## Pennsylvania Road Lighting and Safety

This repository demonstrates an end-to-end analysis of the question:

_How does roadway lighting affect roadway safety?_

This project demonstrates the use of the R statistical programming languaeg and several packages to conduct a "data science" analysis of the relationship between roadway lighting (i.e. streetlights) and crashes on major roads in Pennsylvania.

**View the R notebook here: [https://markegge.github.io/safety-demo/]**

## Concepts Demonstrated

This package illustrates several common transportation data analysis tasks, including:

* Managing data using the fast `data.table` package
* Using the `sf` package to perform a spatial join between point data (crashes) and line data (road segments)
* Using the `rpart` package to predict lighting conditions for road segments with unknown lighting conditions based on road segments with known lighting conditions
* Exploratory data analysis using the `leaflet` mapping package and `ggplot2` plotting package
* Fitting a zero-inflated Poisson regression model to predict crash counts controlling for traffic volume



# Walkthrough

The `lighting.R` source file walks through the steps to attempt to elucidate the relationship between roadway lighting conditions (is there a streetlight present?) and crash history.

**Problem Statement:** Does roadway lighting reduce crashes?

**Hypothesis:** When controlling for exposure (traffic volumes), roadway lighting is associated with in fewer crashes.

## Dataset

This project combines crash data with road inventory and traffic data:

* Road segment and traffic count data from [PennDOT's RMS Traffic Open Data](https://data-pennshare.opendata.arcgis.com/datasets/rmstraffic-traffic-volumes)
* PA Crash Data from [PennDOT's Public Crash Databases](https://crashinfo.penndot.gov/PCIT/welcome.html)


# Attribution

This project is created as a demonstration for CCAC ATE-252 and may be freely reused and distribuetd.

Project author: Mark Egge (mark@eateggs.com)