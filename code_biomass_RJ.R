################
#######
## Autor: Pedro Rajão
## Data Início: 22/05/2024
#######
################
# 
library(dplyr)
library(readr)
library(gridExtra)
library(dplyr)
library(readr)
library(gridExtra)
library(ggplot2)
library(sf)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(rgdal)
library(cowplot)
library(lattice)
library(tidyr)
library(sp)
library(stars)
library(leaps)
library(lme4)
library(ggplot2)
library(cluster)
library(vegan)
#

df_if_rj <- read.csv(file.choose(), sep=',', dec='.')

df_if_rj <- df_if_rj %>%
  mutate(mun_sub_sub = paste(mun, Subunidade, sep = "_"))

df_lat_lon <- df_if_rj %>%
  select(lat_pc, lon_pc)

write.csv(df_lat_lon, "lat_lon_data.csv", row.names = FALSE)

results <- df_if_rj %>%
  group_by(mun_sub_sub) %>%
  summarise(
    biomass_proxy = sum(DAP),
    lat = mean(lat_pc, na.rm = TRUE),
    long = mean(lon_pc, na.rm = TRUE)
    )

shapefile_pontos <- st_as_sf(results, coords = c("long", "lat"), crs = 4326)
str(shapefile_pontos)

import_rasters <- function(directory) {
  files <- list.files(directory, full.names = TRUE)
  raster_list <- list()
  for (file in files) {
    raster <- rast(file)
    raster_name <- tools::file_path_sans_ext(basename(file))
    raster_list[[raster_name]] <- raster
  }
  
  return(raster_list)
}

directory <- "C:/Users/pedro.rajao/Desktop/meus artigos/Biomass RJ/IFF_biomass"
rasters <- import_rasters(directory)

valores <- list()
for (nome_raster in names(rasters)) {
  raster <- rasters[[nome_raster]]
  valores[[nome_raster]] <- terra::extract(raster, shapefile_pontos)
}


# Combine os valores extraídos com os atributos do shapefile
resultado2 <- cbind(st_drop_geometry(shapefile_pontos), valores)


