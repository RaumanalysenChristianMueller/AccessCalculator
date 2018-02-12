# Calculates accessibility of points for whole study areas using a travel cost analysis based on a street or path network.
# It is written in R and optimized for integration in QGIS
# Raumanalysen - Christian Müller
# raumanalysen@mailbox.org

# data input from QGIS
##Raumanalysen_ChristianMueller = group
##Ausgangspunkte = vector
##Zu_erreichende_Punkte = vector
##Wegenetz = vector
##Untersuchungsgebiet = vector
##Gitterzellengroesse_fuer_Wegenetz = number
##Ausgabe_Shapefile = output vector
##Ausgabe_Raster = output raster

# test parameters for windows
# Ausgangspunkte = readOGR("C:/HochschuleBochum/Daten/Bochum/Stadtgruen", "Gruenflaechen")
# Zu_erreichende_Punkte = readOGR("C:/HochschuleBochum/Daten/Bochum/Hiwis_Datenerhebung_Bochum/einzelPOIs", "OwnSurvey_rathaus")
# Wegenetz = readOGR("C:/HochschuleBochum/Daten/Bochum/Netzwerke", "FussWanderwege")
# Untersuchungsgebiet = readOGR("C:/HochschuleBochum/Daten/Bochum", "StudyArea")
# Gitterzellengroesse_fuer_Wegenetz = 50

# test parameters for linux
# Ausgangspunkte = readOGR("/media/sf_HochschuleBochum/Daten/Bochum/Stadtgruen", "Gruenflaechen")
# Zu_erreichende_Punkte = readOGR("/media/sf_HochschuleBochum/Daten/Bochum/Hiwis_Datenerhebung_Bochum/einzelPOIs", "OwnSurvey_rathaus")
# Wegenetz = readOGR("/media/sf_HochschuleBochum/Daten/Bochum/Netzwerke", "FussWanderwege")
# Untersuchungsgebiet = readOGR("/media/sf_HochschuleBochum/Daten/Bochum", "StudyArea")
# Gitterzellengroesse_fuer_Wegenetz = 50


# rewrite variable names (as GUI is in German)
toPoints <- Zu_erreichende_Punkte
fromPoints <- Ausgangspunkte
networkLines <- Wegenetz
studyArea <- Untersuchungsgebiet
transRasCellSize <- Gitterzellengroesse_fuer_Wegenetz


# defince function
calculateAccessibility <- function(toPoints, fromPoints, networkLines, studyArea, gridCellSize){
  
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
  values(ras) <- 0
  
  # prepare dummy field in vector data
  if (!("AccessDum" %in% colnames(networkLines@data))){
    networkLines@data <- cbind(networkLines@data, AccessDum = transRasCellSize)
  }
  
  # define algrorithm for rasterization of network lines
  # find_algorithms("rasterize")
  alg <- "gdalogr:rasterize_over"
  
  # get algorithm parameters
  # get_usage(alg)
  args <- pass_args(alg, INPUT = networkLines, FIELD = "AccessDum", INPUT_RASTER = ras)
  
  # execute algorithm
  run_qgis(alg = alg, params = args, load_output = T)
  
  # load rasterized network
  tras <- raster(args$INPUT_RASTER)
  
  
  # build transition raster
  trans <- transition(tras, transitionFunction = max, 8)
  
  # conduct geographic correction
  trans <- geoCorrection(trans)
  
  
  # create output object
  out <- fromPoints
  frColN <- colnames(out@data)[1]
  out@data <- as.data.frame(out@data[,1])
  colnames(out@data) <- frColN
  
  
  # calculate travel cost
  costs <- accCost(trans, toPoints)
  
  # reproject to starting points
  costs <- projectRaster(costs, crs = proj)
  
  # adjust values for cell size and replace infinite values
  values(costs) <- values(costs) * transRasCellSize
  values(costs)[which(values(costs) == Inf)] <- NA
  
  
  # get values for each starting location (shape)
  out <- extract(x = costs, y = out, fun = mean, na.rm = T, sp = T)
  colnames(out@data)[ncol(out@data)] <- "Distance"
      
  return(list(out_poly = out, out_ras = costs))
  
}

# execute function
res <- calculateAccessibility(toPoints, fromPoints, networkLines, studyArea, transRasCellSize)
  

# write to file
Ausgabe_Shapefile = res$out_poly
Ausgabe_Raster = res$out_ras


