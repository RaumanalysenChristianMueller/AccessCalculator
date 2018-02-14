# Stand alone function for creating transition rasters
# Raumanalysen - Christian Müller
# raumanalysen@mailbox.org

createTransitionRaster <- function(networkLines, transRasCellSize, studyArea){
  
  # load packages
  cat("Loading R-Packages...")
  for (i in 1:2){
    if (!require("raster")) install.packages("raster", dependencies = T)
    if (!require("sp")) install.packages("sp", dependencies = T)
    if (!require("rgdal")) install.packages("rgdal", dependencies = T)
    if (!require("RQGIS")){
      if (Sys.info()["sysname"] == "Linux") system("gksudo apt-get install libudunits2-dev")
      install.packages("RQGIS", dependencies = T)
    }
    if (!require("gdistance")) install.packages("gdistance", dependencies = T)
  }
  
  
  # get extent from study area
  ext <- extent(studyArea)
  
  # get projection from network lines
  proj <- CRS(proj4string(networkLines))
  
  # create raster
  ras <- raster(resolution = rep(transRasCellSize, 2), ext = ext, crs = proj)
  values(ras) <- NA
  
  # prepare dummy field in vector data
  if (!("AccessDum" %in% colnames(networkLines@data))){
    networkLines@data <- cbind(networkLines@data, AccessDum = 1)
  }
  
  # define algrorithm for rasterization of network lines
  # find_algorithms("rasterize")
  alg <- "gdalogr:rasterize_over"
  
  # get algorithm parameters
  # get_usage(alg)
  args <- pass_args(alg, INPUT = networkLines, FIELD = "AccessDum", INPUT_RASTER = ras)
  
  # execute algorithm
  run_qgis(alg = alg, params = args, load_output = T)
  
  # load and reproject rasterized network
  tras <- raster(args$INPUT_RASTER)
  tras <- projectRaster(tras, crs = proj)
  
  
  # build transition raster
  trans <- transition(tras, transitionFunction = function(x){1}, directions = 16)
  
  # conduct geographic correction
  trans <- geoCorrection(trans)
  
  # return transition raster
  return(trans)
  
}