####################################################################################################
####################################################################################################
## Intégrer les pertes 2015 2016 dans le produit CNIAF UMD 2000-2014, pertes filtrées à 5 pixels 
## remi.dannunzio@fao.org
## 2017/10/20
####################################################################################################
####################################################################################################
options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(rgeos)

######################### Repertoire de travail
setwd("/home/dannunzio/aa_input/")

ras_name <- "zonal_test_map.tif"
zone_nme <- "zones_test.shp"

zdir <- paste0("zonal_",substr(basename(ras_name),1,nchar(basename(ras_name))-4),"_",substr(basename(zone_nme),1,nchar(basename(zone_nme))-4),"/")
zdir <- gsub("è","e",zdir)
zdir <- gsub("é","e",zdir)
zdir <- gsub("à","a",zdir)

dir.create(zdir)

zone <- readOGR(zone_nme)
head(zone)

attr_nme <- "zone"
legend   <- levels(as.factor(zone@data[,attr_nme]))
legend   <- data.frame(cbind(legend,1:length(legend)))
names(legend) <- c(attr_nme,"zonal_code")
zone@data$sort_id <- row(zone@data)[,1]
zone@data <- arrange(merge(zone@data,legend,all.x=T),sort_id)[,c(attr_nme,"zonal_code")]

ras_proj <- proj4string(raster(ras_name))
shp_proj <- proj4string(zone)

######################### Project if necessary
if(ras_proj != shp_proj){
  zone <- spTransform(zone,ras_proj)
  shp_proj <- proj4string(zone)
}

writeOGR(zone,paste0(zdir,"zonal.shp"),"zonal","ESRI Shapefile",overwrite_layer = T)

######################### Compute Zonal stats
system(sprintf("oft-zonal_large_list.py -i %s -um %s -o %s -a %s",
               ras_name,
               paste0(zdir,"zonal.shp"),
               paste0(zdir,"stats.txt"),
               "zonal_code"
               ))

######################### Read Zonal stats
df <- read.table(paste0(zdir,"stats.txt"))
names(df) <- c("zonal_code","total","no_data",paste0("map_",1:(ncol(df)-3)))

df1 <- merge(df,legend)
df2 <- df1[,c(ncol(df1),2:ncol(df))]
df3 <- df2
pix  <- res(raster(ras_name))[1]
unit <- 10000
df3[,2:ncol(df)] <- df2[,2:ncol(df)]*pix*pix/unit
df3
write.csv(df3,paste0(zdir,"zonal_stats.csv"),row.names = F)
