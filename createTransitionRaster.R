# Creates a cost Raster for travel cost analyses
# Raumanalysen - Christian Müller
# raumanalysen@mailbox.org

createTransitionRaster <- function(networkLines, transRasCellSize, studyArea){
  
  # load packages
  cat("Loading R-Packages...")
  for (i in 1:2){
    if (!require("gdistance")) install.packages("gdistance", dependencies = T)
    if (!require("rgdal")) install.packages("rgdal", dependencies = T)
    if (!require("rgeos")) install.packages("rgeos", dependencies = T)
    if (!require("raster")) install.packages("raster", dependencies = T)
    if (!require("plotKML")) install.packages("plotKML", dependencies = T)
    if (!require("RSAGA")) install.packages("RSAGA", dependencies = T)
    if (!require("gdalUtils")) install.packages("gdalUtils", dependencies = T)
    if (!require("RQGIS")){
      if (Sys.info()["sysname"] == "Linux") system("gksudo apt-get install libudunits2-dev")
      install.packages("RQGIS", dependencies = T)
    }
  }
  
  # get extent from study area
  ext <- extent(studyArea)
  
  # get projection from network lines
  proj <- CRS(proj4string(networkLines))
  
  # create raster
  ras <- raster(resolution = rep(transRasCellSize, 2), ext = ext, crs = proj)
  values(ras) <- 0
  
  
  # # write temp files for gdal operation
  # tempRas_path <- paste0(tempfile(), ".tif")
  # writeRaster(ras, file = tempRas_path, format = "ascii", prj = T, overwrite = T)
  # tempShp_path <- paste0(tempfile(), ".shp")
  # writeOGR(networkLines, dirname(tempShp_path), strsplit(basename(tempShp_path), ".", fixed = T)[[1]][1],
  #          driver = "ESRI Shapefile", overwrite_layer = T)
  
  # rasterize network lines
  tras <- rasterize(networkLines, ras, fun = "first")  # revision until here
  
  # alternative 1:
  # os.system("ogr2ogr -t_srs EPSG:102001 %s %s" % (tempShp_path, tempShp_path)) # output then input
  # 
  # gdal_setInstallation()
  # tras <- gdal_rasterize(tempShp_path, tempRas_path, b = 1, burn = transRasCellSize,
  #                        l = strsplit(basename(tempShp_path), ".", fixed = T)[[1]][1],
  #                        verbose = T, output_Raster = T)
  
  # more alternatives:
  # v.to.rast.attribute()
  # v.to.rast.value()
  # gdalogr:rasterize()
  # gdalogr:rasterize_over()
  
  
  
  # build transition raster, fill with values and conduct geographic correction 
  values(tras)[which(is.na(values(tras)) == F)] <- costRasCellSize
  values(tras)[which(is.na(values(tras)))] <- 0
  trans <- transition(tras, transitionFunction = max, 8)
  trans <- geoCorrection(trans)
  
  # return transition raster
  return(trans)
  
}