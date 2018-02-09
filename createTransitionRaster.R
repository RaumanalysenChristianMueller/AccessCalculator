# Creates a cost Raster for travel cost analyses
# Raumanalysen - Christian Müller
# raumanalysen@mailbox.org

createTransitionRaster <- function(networkLines, costRasCellSize, studyArea){
  
  # load packages
  cat("Loading R-Packages...")
  for (i in 1:2){
    if (!require("gdistance")) install.packages("gdistance", dependencies = T)
    if (!require("rgdal")) install.packages("rgdal", dependencies = T)
    if (!require("rgeos")) install.packages("rgeos", dependencies = T)
    if (!require("raster")) install.packages("raster", dependencies = T)
    if (!require("plotKML")) install.packages("plotKML", dependencies = T)
    if (!require("RSAGA")) install.packages("RSAGA", dependencies = T)
  }
  
  # get extent from study area
  ext <- extent(studyArea)
  
  # get projection from network lines
  proj <- CRS(proj4string(networkLines))
  
  # create raster
  ras <- raster(resolution = rep(costRasCellSize, 2), ext = ext, crs = proj)
  values(ras) <- -1
  
  # rasterize network lines
  tras <- rasterize(networkLines, ras, fun = "first")  # revision until here
  
  # # We'll pre-check to make sure there is a valid GDAL install
  # # and that raster and rgdal are also installed.
  # # Note this isn't strictly neccessary, as executing the function will
  # # force a search for a valid GDAL install.
  # gdal_setInstallation()
  # valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
  # if(require(raster) && require(rgdal) && valid_install)
  # {
  #   # Example from the original gdal_rasterize documentation:
  #   # gdal_rasterize -b 1 -b 2 -b 3 -burn 255 -burn 0
  #   # 	-burn 0 -l tahoe_highrez_training tahoe_highrez_training.shp tempfile.tif
  #   dst_filename_original  <- system.file("external/tahoe_highrez.tif", package="gdalUtils")
  #   # Back up the file, since we are going to burn stuff into it.
  #   dst_filename <- paste(tempfile(),".tif",sep="")
  #   file.copy(dst_filename_original,dst_filename,overwrite=TRUE)
  #   #Before plot:
  #   plotRGB(brick(dst_filename))
  #   src_dataset <- system.file("external/tahoe_highrez_training.shp", package="gdalUtils")
  #   tahoe_burned <- gdal_rasterize(src_dataset,dst_filename,
  #                                  b=c(1,2,3),burn=c(0,255,0),l="tahoe_highrez_training",verbose=TRUE,output_Raster=TRUE)
  #   #After plot:
  #   plotRGB(brick(dst_filename))
  # }
  
  # build transition raster, fill with values and conduct geographic correction 
  values(tras)[which(is.na(values(tras)) == F)] <- costRasCellSize
  values(tras)[which(is.na(values(tras)))] <- 0
  trans <- transition(tras, transitionFunction = max, 8)
  trans <- geoCorrection(trans)
  
  # return transition raster
  return(trans)
  
}