# POPULATION GRID AMERICAN SAMOA - ASM - 2022 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution population grids
## Luis de la Rua - luisr@spc.int - October 2022

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
library(terra)

# SET PARAMETERS
## working directory
wd <- "C:/git/spc/popgrid_2022"
setwd(wd)


## Country code to name output data
country <- 'ASM'
# Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2022/",country)

# Define Population Growth Rates parameters
census_year <- 2010
current_year <- 2020
pop_2010 <- 55519
prjpop_2020 <- 56818

popGR <- (prjpop_2020 - pop_2010)/((current_year - census_year)*pop_2010)
popGR

# POULATION SPATIAL DISTRIBUTION INPUT
# FBook popgrid as a replacement of the GPS

#bring points based on FB dataset
pts <- st_read("layers/FB_points_ahs.shp")
class(pts)

#EA boundary with 2011 population figures and Average hh size calculated
EA <- st_read(paste0(dd,"/layers/AS_counties_4326.shp"))

#replace NA values
pts$ahs[is.na(pts$ahs2010)]<- 0

# zone layer to avoid extent crop and setting CRS
zone<- st_read("layers/zone_4326.shp")
extent(zone)

# generate blank raster 100m
rast100m <- raster()
extent(rast100m) <- extent(zone)
crs(rast100m) <- crs(zone)
res(rast100m) <- 0.001 #ATTENTION REMEMBER WE ARE USING 4326 CRS -> resolution in degrees!!!
rast100m


#total population calculated from point layer
totpop_pts <- sum(pts$ahs)
totpop_pts
#what is the value of the 2020 projected population?
prjpop2020 <- (totpop_pts+(totpop_pts * popGR * (as.numeric(current_year)- as.numeric(census_year))))
