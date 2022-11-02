# POPULATION GRID KIRIBATI - KIR - 2022 UPDATE

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

## Country code to name output data and other files
country <- 'KIR'
# Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2022/",country)

# Define Population Growth Rates parameters
census_year <- 2020
current_year <- 2022
pop_2010 <- 55519
prjpop_2020 <- 56818

# check if there is pgr defined in census report
popGR <- (prjpop_2020 - pop_2010)/((current_year - census_year)*pop_2010)
popGR

# POULATION SPATIAL DISTRIBUTION INPUT
# HH locations and population from 2020 PHC
pts <- st_read(paste0(dd,"/layers/hhloc_2020PHC_3832.gpkg"))

# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"/layers/zone.gpkg"))

# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 100
rast100m

# set 3832 projection
crs(rast100m) <- CRS("+init=epsg:3832")
rast100m

##RASTERIZE Check that pop field is integer
rastpop2018_100m <- rasterize(pts,rast100m,'hhsize',fun=sum)
rastpop2018_100m
totpop2018_count <- cellStats(rastpop2018_100m, 'sum')
totpop2018_count

# optional draw histogram ignoring NA values
# hist(na.omit(getValues(rastpop2018_100m)))

writeRaster(rastpop2018_100m ,'raster/KIR_pop2018_100.tif', overwrite=TRUE)

#projecting population data up to current year
pop_dif <- totpop2018_count*popGR * (as.numeric(current_year)- as.numeric(census_year))
pop_dif

#project population on GPS location dataset
pts2020 <- pts
pts2020

##ROUND PRESERVE SUM projecting population over the point layer as source
#define formula for the round preserving sum
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

#create field with total population projected
pts2020$totpop2020 <- (pts2020$HHsize + (pts2020$HHsize * popGR * (as.numeric(current_year)- as.numeric(census_year))))
pts2020

pts_totpop2020 <- sum (pts2020$totpop2020)
pts_totpop2018 <- sum(pts2020$HHsize)

pop_evolution <- pts_totpop2020 - pts_totpop2018
pop_evolution

#round and check to see if it works
round_preserve_sum(pts2020$totpop2020)

pts_totpop2020_rps<- sum(round_preserve_sum(pts2020$totpop2020))
pts_totpop2020_rps
dif <- pts_totpop2020_rps - pts_totpop2020
dif

#create the rounded field and add it into the att table
pts2020$totpop2020rps <- round_preserve_sum(pts2020$totpop2020)
pts2020

sum(pts2020$totpop2020rps)
st_write(pts2020,"layers/pts2020.shp",delete_layer=TRUE)

rastpop2020rps_100m<- rasterize(pts2020,rast100m,'totpop2020rps',fun=sum)
rastpop2020rps_100m
cellStats(rastpop2020rps_100m, 'sum')

writeRaster(rastpop2020rps_100m ,'raster/rastpop2020rps_100m.tif', overwrite=TRUE)
