# POPULATION GRID Palau- PLW - 2022 UPDATE

# Use of census data and country projections to generate 100m resolution population grids
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
country <- 'PLW'
# Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2022/",country)

# Define Population Growth Rates parameters
census_year <- 2015
current_year <- 2022
pop_2010 <- 0 # calculated from latest census data
prjpop_2022 <- 17976  # From SDD .STAT population projections

# POULATION SPATIAL DISTRIBUTION INPUT
# HH locations and population from 2010 PHC HH listing (2019 PHC has not been released yet)
pts <- st_read(paste0(dd,"/layers/GPSmodif_rmahs.shp"))
pts$pop <- pts$rmahs

pts$pop[is.na(pts$pop)]<- 0 # remove NAs to allow computing the sum
pop_2010 <- sum(pts$pop)

# check if there is pgr defined in census report
popGR <- (prjpop_2022 - pop_2010)/((current_year - census_year)*pop_2010)
popGR

# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"/layers/zone_4326.shp"))

# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 0.001
rast100m

# set projection
crs(rast100m) <- CRS("+init=epsg:4326")
rast100m

# RASTERIZE Check that pop field is integer
rastpop2010_100m <- rasterize(pts,rast100m,'pop',fun=sum)
rastpop2010_100m
totpop2010_count <- cellStats(rastpop2010_100m, 'sum')
totpop2010_count

# optional draw histogram ignoring NA values
# hist(na.omit(getValues(rastpop2020_100m)))

# writeRaster(rastpop2020_100m ,'raster/KIR_pop202.tif', overwrite=TRUE)

# projecting population data up to current year
pop_dif <- totpop2010_count*popGR * (as.numeric(current_year)- as.numeric(census_year))
pop_dif

# project population on GPS location dataset
pts2010 <- pts
pts2010

## ROUND PRESERVE SUM projecting population over the point layer as source
# define formula for the round preserving sum
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# create field with total population projected
pts2010$totpop2022 <- (pts2010$pop + (pts2010$pop * popGR * (as.numeric(current_year)- as.numeric(census_year))))
pts2010

pts_totpop2022 <- sum(pts2010$totpop2022,na.rm = T)
pts_totpop2010 <- sum(pts2010$pop,na.rm = T)

pop_evolution <- pts_totpop2022 - pts_totpop2010
pop_evolution

# round and check to see if it works
round_preserve_sum(pts2010$totpop2022)

pts_totpop2022_rps<- sum(round_preserve_sum(pts2010$totpop2022))
pts_totpop2022_rps
dif <- pts_totpop2022_rps - pts_totpop2022
dif

# create the rounded field and add it into the att table
pts2010$totpop2022rps <- round_preserve_sum(pts2010$totpop2022)
pts2010

sum(pts2010$totpop2022rps)
# st_write(pts2020,"layers/pts2020.shp",delete_layer=TRUE)

rastpop2022rps_100m<- rasterize(pts2010,rast100m,'totpop2022rps',fun=sum)
rastpop2022rps_100m
cellStats(rastpop2022rps_100m, 'sum')

writeRaster(rastpop2022rps_100m ,paste0(dd,"/raster/",country,"_pop2022.tif"), overwrite=TRUE)
