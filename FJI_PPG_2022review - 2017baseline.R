# POPULATION GRID Fiji - FJI - 2022 UPDATE

# Use of census data and country projections to generate  100m resolution population grids
## Join work between FBOS and SDD ##
## We are reviewing the process including official data provided by FBOS ##

# Libraries
library(sf)
library(sp)
library(raster)
library(spData)
library(dplyr)
library(magrittr)
library(terra) # using this library for spatial operations, libraries raster and rgdal will be deprecated in 2023
library(tidyterra)
library(tmap)
library(leaflet)
library(exactextractr) # summary population by Province or Subdivision
library(tictoc)

# 1. SET PARAMETERS -----
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
# https://pacificdata.org/data/dataset/population-projections-df-pop-proj

## calculate PGR
popGR <- (prjpop_2022 - pop_2017)/((current_year - census_year)*pop_2017)
popGR

## Define ROUND PRESERVE SUM function
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# LOADING INPUT LAYERS ----
## hh locations from census
hhloc <- vect(paste0(dd,"/Population Grid Data/Fiji_HH_2017.shp"))
crs(hhloc)

## We are making all the spatial analysis using Fiji Map Grid projection EPSG 3460, grabbing it from the first layer loaded
fji_crs <- crs(hhloc)

## EA boundaries layer including census population and hh counts
ea <- vect(paste0(dd,"/layers/FJI_EA2017_PPGwork.gpkg"))
plot(ea)

## OSM building footprints
bf <- vect(paste0(dd,"/layers/OSM/gis_osm_buildings_a_free_1.shp"))
bf <- project(bf,fji_crs) #reproject to Fiji CRS
bf_points <- centroids(bf,inside=T) # converting the layer into centroids


## IDENTIFY EAs WITH HH LOCATIONS GAPS ----
## Extract the EAs with huge difference between census hh counts and number of HH locations to know where the data gaps can be found
eagap <- subset(ea,ea$diff > 0.5 | ea$diff < -0.5 )

# Visualize where are the EAs with data gaps
plot(eagap, border='red')
plot(ea)
plot(eagap, border='red', add=T)

## take building footprints that within the EAs with gaps
tic()
bf_ingaps <- intersect(bf_points, eagap)
toc()
nrow(bf_points)
nrow(bf_ingaps)

## FILL THE GAPS WITH OSM BUILDING FOOTPRINTS AND CALCULATE AVERAGE HH SIZE PER EA ----

## Merge hh locations from census with the points from the building footprints that are going to give use info on where the settlements are
hhloc_merged <- terra::union(hhloc,bf_ingaps) 
nrow(hhloc_merged)
hhloc_merged$val <- 1 # this is useful later to be able to count points in polygon
hhloc_merged <- hhloc_merged[,"val"] # simplify the layer
head(hhloc_merged)

## Count number of hhlocations within each EA 
ea_simpl <- ea[,"ea2017"] # before we simplify dataset

i <- intersect (ea_simpl,hhloc_merged) # run intersection between EAs and hhloc so we have EA code in each hh point

## Tabulate to calculate the hhcounts by EA (collapse number of hh/EA)
hhcount <- as.data.frame(i) %>% 
  group_by(i$ea2017) %>% 
  count()

## Connect hhcount table with ea layer
ea <- merge(ea,hhcount,all.x=TRUE, by.x='ea2017', by.y='i$ea2017')
head(ea)

## Calculate Average HH size per EA (AHS) (and iterate over the different age groups? next iteration)
ea$avhhsize <- ea$Total_Popu/ea$n
head(ea)
ea_ahs <-ea[,c("ea2017","avhhsize")] #Simplify layer
head(ea_ahs)
head(i)

## WORK AT HH LEVEL, ALLOCATE AV HH SIZE AND PROJECT POPULATION ----

## Retrieve AHS from EA and include it into hhlocations merged as an attribute 
hhloc_ahs <- intersect(i,ea_ahs)
head(hhloc_ahs)
hhloc_ahs <- hhloc_ahs[,c("ea2017","avhhsize")] # Clean unused fields from dataset
sum(hhloc_ahs$avhhsize) # check everything is adding ok

## Project the population for each of the hh locations
hhloc_ahs$pop2022 <- (hhloc_ahs$avhhsize + (hhloc_ahs$avhhsize * popGR * (as.numeric(current_year)- as.numeric(census_year))))
totpop2022 <- sum(hhloc_ahs$pop2022)

## Round to get integers
hhloc_ahs$pop2022rps <- round_preserve_sum(hhloc_ahs$pop2022)
head(hhloc_ahs)
totpop2022rps <- sum(hhloc_ahs$pop2022rps)
totpop2022rps 

## GENERATE THE POPULATION GRID RASTER ----

## Convert the Point layer into a 100x100m raster.
## Create a blank raster first
ras <- rast()
crs(ras) <- fji_crs # CRS
ext(ras) <- ext(ea)  # set extent the same as the EA fwork
res(ras) <- 100 # Resolution

## Rasterize the hh locations dataset using the projected population
rastpop2022 <- terra::rasterize(hhloc_ahs,ras,'pop2022rps',fun=sum)

totpop2022rps - (global(rastpop2022, fun='sum',na.rm=T)) # checking that raster includes same population as the original, if it 0 we are good and means that the
# Result 0 then rounding worked well 

## Export Raster into tif format
writeRaster(rastpop2022,paste0(dd,"/raster/",country,"_rastpop2022_01.tif"), overwrite=T)
writeRaster(rastpop2022,"C:/gis/data/output/rastpop_2022.tif", overwrite=T)

## Summarize data

# Print the result

hist_tab <- print(table(values(rastpop2022)))
write.csv(hist_tab,paste0(dd,"/raster/",country,"_rastpop2017_01.csv"))
hist(rastpop2022)

# Summary data
prov <- terra::aggregate(ea,by="Province", dissolve=T)
prov <- as(prov,"Spatial")
crs(prov)<-crs(rastpop2022)
pop_prov <- exactextractr::exact_extract(rastpop2022,
                                       prov,
                                       'sum',
                                       full_colnames=T)
pop_prov <- cbind(prov, pop_prov)

pop_prov <- pop_prov %>% vect(pop_prov) %>% 
  rename(popgrid = c.252035.140625..15712.7568359375..50902.28515625..10539.1630859375..) %>% 
  select(c("Province","popgrid"))

# The same at subdivision level.
