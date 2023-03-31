# POPULATION GRID Fiji - FJI - 2023 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution population grids
## Join work between FBOS and SDD ##
## We are reviewing the process including official data provided by FBOS ##

# Libraries
library(sf)
library(sp)
library(maptools)
library(raster)
library(spData)
library(rgeos)
library(dplyr)
library(rgdal)
library(magrittr)
library(terra) # using this library for spatial operations, libraries raster and rgdal will be deprecated in 2023

# SET PARAMETERS
## working directory
wd <- "C:/git/spc/popgrid_2022"
setwd(wd)

## Country code to name output data and other files
country <- 'FJI'
## Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2022/",country)

## Define Population Growth Rates parameters
census_year <- 2017
current_year <- 2022
pop_2017 <- 884887  # calculated from latest census data
prjpop_2022 <-  901603   # From SDD .STAT population projections - We need to update projections according to last work made by FBOS/SDD

## calculate PGR
popGR <- (prjpop_2022 - pop_2017)/((current_year - census_year)*pop_2017)
popGR


# LOADING INPUT LAYERS
## hh locations from census
hhloc <- vect(paste0(dd,"/Population Grid Data/Fiji_HH_2017.shp"))
crs(hhloc)

## We are making all the geospatial analysis using Fiji Map Grid projection EPSG 3460, grabbing it from the first layer loaded
fji_crs <- crs(hhloc)

## EA boundaries layer including census population and hh counts
ea <- vect(paste0(dd,"/layers/FJI_EA2017_PPGwork.gpkg"))

## OSM building footprints
bf <- vect(paste0(dd,"/layers/OSM/gis_osm_buildings_a_free_1.shp"))
bf <- project(bf,fji_crs) #reproject to Fiji CRS
bf_points <- centroids(bf,inside=T) # converting the layer into centroids
