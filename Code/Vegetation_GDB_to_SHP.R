library(raster)
library(sf)
library(sp)
library(stars)
library(plotly)
library(terra)
#===========================================================================================
# Converts cursed format
#===========================================================================================
Veg_GDB_to_SHP <- function(fact = 4)
{ 

  #-------------------------------------------------------------------------------
  # POINT THIS TO A SHAPEFILE OF ALL THREE SISKIYOU WATERSHEDS
  Siskiyou_Watersheds <<- st_read(file.path('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/Upland-Management-Siskiyou/Shapefiles',
                                            'Upland_project_identification_QGIS','shapefiles_used'), layer = 'siskiyou_watersheds', quiet = TRUE)
  
  #-------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------
  # recommended defaults
  # fact <- 4
  #-------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  # POINT THESE TO THE FVEG GEO DATABASE
  cat('Loading Raster For Value Translation \n\n')
  raster_for_levels <- raster('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/fveg22_1.gdb')
  raster_for_levels <- crop(raster_for_levels, extent(st_transform(Siskiyou_Watersheds,3310)))
  
  
  cat('Loading Raster For Values \n\n')
  raster_for_values <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/fveg22_1.gdb')
  raster_for_values <- crop(raster_for_values, extent(st_transform(Siskiyou_Watersheds,3310)))
  
  cat('Aggregating Raster For Values \n\n')
  raster_for_aggregating <- aggregate(raster_for_values, fact = fact)
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  translate_df <- data.frame(shortcode = levels(raster_for_levels)[[1]]$WHRTYPE,
                             longcode = levels(raster_for_levels)[[1]]$WHRALL,
                             translation = levels(raster_for_levels)[[1]]$WHRNAME,
                             whr10 = levels(raster_for_levels)[[1]]$WHR10NAME)
  unique_short_codes <- unique(translate_df$shortcode)
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  cat('Processing Blank Raster \n\n')
  blank_ <- raster_for_values
  blank_ <- st_as_sf(as.polygons(blank_))
  blank_$WHRALL <- rep(-9999, nrow(blank_))
  blank_$WHR10 <- rep(-9999, nrow(blank_))
  blank_ <- blank_[1, ]
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  for(i in 1:length(unique_short_codes))
  {
    
    cat('Processing Actual Raster WHR Codes ',
        i/length(unique_short_codes) * 100, ' % Done \n\n')
    tmp <- raster_for_values
    values_for_string_code <- levels(raster_for_values)[[1]]$Value[grepl(unique_short_codes[i], levels(raster_for_values)[[1]]$WHRALL) == TRUE]
    translation_for_string_code <- translate_df$translation[translate_df$shortcode == unique_short_codes[i]][1]
    whr10_for_string_code <- translate_df$whr10[translate_df$shortcode == unique_short_codes[i]][1]
    values(tmp)[!(values(tmp) %in% values_for_string_code)] <- NA
    
    
    tmp <- st_as_sf(as.polygons(tmp))
    tmp$WHRALL <- rep(translation_for_string_code, nrow(tmp))
    tmp$WHR10 <- rep(whr10_for_string_code, nrow(tmp))
    blank_ <- rbind(blank_, tmp)

    
  }
  #-------------------------------------------------------------------------------------------
  

  #-------------------------------------------------------------------------------------------
  # diagnostics
  cat('Plotting diagnostics \n\n')
  blank_ <- blank_[-c(1), ]
  final_rasterized <- rasterize(blank_, raster_for_aggregating, c('WHRALL'))
  final_rasterized2 <- rasterize(blank_, raster_for_aggregating, c('WHR10'))
  final_polygons <- st_as_sf(as.polygons(final_rasterized))
  final_polygons$WHR10 <- rep(NA,(nrow(final_polygons)))
  for(i in 1:nrow(final_polygons))
  {
    
    final_polygons$WHR10[i] <- translate_df$whr10[final_polygons$WHRALL[i] == translate_df$translation][1]
    
  }
  
  
  
  plot(final_rasterized, main = 'Raster')
  plot(st_transform(Siskiyou_Watersheds, 3310), add = T, col = NA, border = 'red', lwd = 2)
  
  
  plot(st_geometry(st_transform(Siskiyou_Watersheds, 3310)), main = 'Juniper', col = NA, border = 'blue', lwd = 2)
  plot(st_geometry(final_polygons[final_polygons$WHRALL == 'Juniper', ]), border = NA, col = 'red', add = T)
  plot(st_geometry(st_transform(Siskiyou_Watersheds, 3310)), add = T, col = NA, border = 'blue', lwd = 2)
  #-------------------------------------------------------------------------------------------
  

  #-------------------------------------------------------------------------------------------
  # Writeout 
  # POINT DIRECTORIES TO WRITEOUT LOCATIONS
  cat('Writeout files \n\n')
  st_write(final_polygons,
           'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Scott_WHRALL.shp',
           append = FALSE)
  writeRaster(final_rasterized,
              'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Scott_WHRALL.tif', 
              overwrite = TRUE)
  writeRaster(final_rasterized2,
              'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Scott_WHR10.tif', 
              overwrite = TRUE)
  #-------------------------------------------------------------------------------------------

  
  cat('IMPORTANT:: TIF REQUIRES AUX FILE FOR LEVELS \n\n')

}
#-------------------------------------------------------------------------------------------



png(filename=paste("C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/Upland-Management-Siskiyou/",
                   'Output/Maps/', 'WHR10.png', sep = ''),
    width=12, height=12, units="in", res=300)

read <<- st_read('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Output/Scott_WHRALL.shp')
Palette <- sequential_hcl(n = length(unique(read$WHR10)),
                          palette = 'Reds2')
Palette <- c('#ffe771','#ffdebf','#224539','#236f30','#b2d85f','#798a73','#020608','#2382b2','#00565c')


Palette[as.factor(read$WHR10)]
plot(st_geometry(read), col = Palette[as.factor(read$WHR10)], border = NA)


rgb(as.vector(col2rgb(Palette[9])/255)[1],
    as.vector(col2rgb(Palette[9])/255)[1],
    as.vector(col2rgb(Palette[9])/255)[1],
    0.6)
Palette2 <- c()
for(i in 1:length(Palette))
{
  
  Palette2 <- append(Palette2, rgb(as.vector(col2rgb(Palette[i])/255)[1],
                                   as.vector(col2rgb(Palette[i])/255)[2],
                                   as.vector(col2rgb(Palette[i])/255)[3],
                                   0.4))
  
}

plot(merged_siskiyou_DEM_hillshade, col = sequential_hcl(n = 100, palette = 'grays'))
plot(st_geometry(st_transform(read,4326)),
     col = Palette2[as.factor(read$WHR10)],
     border = NA, add = T)

plot(st_geometry(read), col = Palette2[as.factor(read$WHR10)], border = NA)

# read$color <- rep(NA,nrow(read))
# read$color[read$WHR10 == unique(read$WHR10)[1]] <- Palette[1]
# read$color[read$WHR10 == unique(read$WHR10)[2]] <- Palette[2]
# read$color[read$WHR10 == unique(read$WHR10)[3]] <- Palette[3]
# read$color[read$WHR10 == unique(read$WHR10)[4]] <- Palette[4]
# read$color[read$WHR10 == unique(read$WHR10)[5]] <- Palette[5]
# read$color[read$WHR10 == unique(read$WHR10)[6]] <- Palette[6]
# read$color[read$WHR10 == unique(read$WHR10)[7]] <- Palette[7]
# read$color[read$WHR10 == unique(read$WHR10)[8]] <- Palette[8]
# read$color[read$WHR10 == unique(read$WHR10)[9]] <- Palette[9]
# read <- projectRaster(read, crs = crs(Siskiyou_HUC12))
# nf <- plotly::layout(matrix(c(1,2), ncol = 2, byrow = T),
#                      widths = c(3,1))
plot(st_transform(st_geometry(Siskiyou_Watersheds[Siskiyou_Watersheds$Name == 'Scott' | Siskiyou_Watersheds$Name == 'Shasta', ]),3310),
     border = 'black',
     lwd = 2, axes = F)
plot(read, col = Palette, border = NA, add = T, legend = F)

plot(st_transform(st_geometry(st_intersection(Shasta_Wildfires[Shasta_Wildfires$YEAR_ == 2021, ],
                 Siskiyou_Watersheds[Siskiyou_Watersheds$Name == 'Shasta', ])),3310), add = T, col = NA, border = 'red', lwd = 5)

plot(st_transform(st_geometry(st_intersection(Scott_Wildfires[Scott_Wildfires$YEAR_ == 2021, ],
                 Siskiyou_Watersheds[Siskiyou_Watersheds$Name == 'Scott', ])),3310), add = T, col = NA, border = 'red', lwd = 5)

plot(st_transform(st_geometry(Siskiyou_Watersheds[Siskiyou_Watersheds$Name == 'Scott' | Siskiyou_Watersheds$Name == 'Shasta', ]),3310),
     border = 'black',
     lwd = 6, add = T)
dev.off()


png(filename=paste("C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/Upland-Management-Siskiyou/",
                   'Output/Maps/', 'WHR10_legend.png', sep = ''),
    width=12, height=12, units="in", res=300)
plot(c(1:25),c(1:25), col = 'white', xaxt= 'n', yaxt = 'n', ylab = '', xlab = '', bty = 'n')
legend(x = 'center',
       legend = levels(rast(read))[[1]]$WHR10,
       col = Palette,
       pch = 15,
       cex = 2)
dev.off()




