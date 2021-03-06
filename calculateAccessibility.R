# Calculates accessibility of points for whole study areas using a travel cost analysis based on a street or path network.
# It is written in R and optimized for integration in QGIS
# Raumanalysen - Christian M�ller
# raumanalysen@mailbox.org

# data input from QGIS
##Raumanalysen_ChristianMueller = group
##Ausgangspunkte = vector
##Zu_erreichende_Punkte = vector
##Wegenetz = vector
##Untersuchungsgebiet = vector
##Ausgabe_Shapefile = output vector
##Ausgabe_Raster = output raster

# test parameters for windows
# Ausgangspunkte = readOGR("C:/HochschuleBochum/Daten/Bochum/Stadtgruen", "Gruenflaechen")
# Zu_erreichende_Punkte = readOGR("C:/HochschuleBochum/Daten/Bochum/Hiwis_Datenerhebung_Bochum/einzelPOIs", "OwnSurvey_rathaus")
# Wegenetz = readOGR("C:/HochschuleBochum/Daten/Bochum/Netzwerke", "FussWanderwege")
# Untersuchungsgebiet = readOGR("C:/HochschuleBochum/Daten/Bochum", "StudyArea")

# test parameters for linux
# Ausgangspunkte = readOGR("/media/sf_Raumanalysen_ChristianMueller/Schulungen/QGIS/Fortgeschrittene/FSA_Mar_2018/Daten/vorbereiteteDaten_Backup", "Wohngebaeude_test")
# Zu_erreichende_Punkte = readOGR("/media/sf_Raumanalysen_ChristianMueller/Schulungen/QGIS/Fortgeschrittene/FSA_Mar_2018/Daten/vorbereiteteDaten_Backup", "Kindergaerten_test")
# Wegenetz = readOGR("/media/sf_Raumanalysen_ChristianMueller/Schulungen/QGIS/Fortgeschrittene/FSA_Mar_2018/Daten/vorbereiteteDaten_Backup", "Wegenetz_test")
# Untersuchungsgebiet = readOGR("/media/sf_Raumanalysen_ChristianMueller/Schulungen/QGIS/Fortgeschrittene/FSA_Mar_2018/Daten/vorbereiteteDaten_Backup", "Bergheim_test")


# rewrite variable names (as GUI is in German)
toPoints <- Zu_erreichende_Punkte
fromPoints <- Ausgangspunkte
networkLines <- Wegenetz
studyArea <- Untersuchungsgebiet
transRasCellSize <- 10
bufSize <- 50


# define function
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
    if (!require("rgeos")) install.packages("rgeos", dependencies = T)
    if (!require("tcltk")) install.packages("tcltk", dependencies = T)
    if (!require("tcltk2")) install.packages("tcltk2", dependencies = T)
  }                      
  
  # create progress bar
  try(tk <- tktoplevel(), silent = T)
  try(tk2ico.setFromFile(win = tk, iconfile =  paste0(getwd(), "/Logo.ico")), silent = T)
  try(font_text <- tkfont.create(family = "Ebrima", size = 12, weight = "bold"), silent = T)
  try(tktitle(tk) <- "Raumanalysen - Christian Mueller - AccessibilityCalculator", silent = T)
  try(tk_lab <- tk2label(tk), silent = T)
  try(tk_lab <- tk2label(tk, font = font_text), silent = T)
  try(tk_pb <- tk2progress(tk, length = 700), silent = T)
  try(tkgrid(tk_lab, row = 0), silent = T)
  try(tkgrid(tk_pb, row = 1), silent = T)
  try(tkraise(tk), silent = T)
  try(tk_center(tk), silent = T)
  
  
  # report status
  try(tkconfigure(tk_lab, text = "Analyse-Raster wird vorbereitet..."), silent = T)
  try(tkconfigure(tk_pb, value = 5, maximum = 100), silent = T)
  
  
  # get extent from study area
  ext <- extent(studyArea)
  
  # get projection from network lines
  proj <- CRS(proj4string(networkLines))
  
  # create raster
  ras <- raster(resolution = rep(transRasCellSize, 2), ext = ext, crs = proj)
  raster::values(ras) <- NA
  
  # prepare dummy field in vector data
  if (!("AccessDum" %in% colnames(networkLines@data))){
    networkLines@data <- cbind(networkLines@data, AccessDum = 1)
  }
  
  
  # report status
  try(tkconfigure(tk_lab, text = "Konvertiere Wegenetz zu Raster..."), silent = T)
  try(tkconfigure(tk_pb, value = 10, maximum = 100), silent = T)
  
  
  
  # define algrorithm for rasterization of network lines
  # find_algorithms("rasterize")
  alg <- "gdalogr:rasterize_over"
  
  # get algorithm parameters
  # get_usage(alg)
  args <- pass_args(alg, INPUT = networkLines, FIELD = "AccessDum", INPUT_RASTER = ras)
  
  # execute algorithm
  run_qgis(alg = alg, params = args, load_output = T)
  
  # report status
  try(tkconfigure(tk_lab, text = "Raster wird geladen..."), silent = T)
  try(tkconfigure(tk_pb, value = 50, maximum = 100), silent = T)
  
  
  # load and reproject rasterized network
  tras <- raster(args$INPUT_RASTER)
  tras <- projectRaster(tras, crs = proj)
  
  
  # report status
  try(tkconfigure(tk_lab, text = "Erstelle Transition-Raster..."), silent = T)
  try(tkconfigure(tk_pb, value = 60, maximum = 100), silent = T)
  
  
  # build transition raster
  trans <- transition(tras, transitionFunction = function(x){1}, directions = 16)
  
  # conduct geographic correction
  trans <- geoCorrection(trans)
  
  
  # create output object
  out <- fromPoints
  frColN <- colnames(out@data)[1]
  out@data <- as.data.frame(out@data[,1])
  colnames(out@data) <- frColN
  
  
  # report status
  try(tkconfigure(tk_lab, text = "Berechne Reisekosten..."), silent = T)
  try(tkconfigure(tk_pb, value = 80, maximum = 100), silent = T)
  
  # buffer aournd targets to ensure network connection
  toPoints_buf <- gBuffer(toPoints, byid = T, width = bufSize)
  
  # get target buffer coordinates
  coords <- c()
  polys <- toPoints_buf@polygons
  for (p in 1:length(polys)){
    thisPolys <- polys[[p]]@Polygons
    for (ps in 1:length(thisPolys)){
      coords <- rbind(coords, thisPolys[[ps]]@coords)
    }
  }
  
  # calculate travel cost
  costs <- accCost(trans, coords)
  
  
  # adjust values for cell size and replace infinite values
  raster::values(costs)[which(raster::values(costs) == Inf)] <- NA
  

  # report status
  try(tkconfigure(tk_lab, text = "Extrahiere Distanzen f�r Start-Features..."), silent = T)
  try(tkconfigure(tk_pb, value = 90, maximum = 100), silent = T)
  
  # buffer around starting to ensure network connection
  buf <- gBuffer(out, byid = T, width = bufSize)
  
  
  # get values for each starting location (shape)
  buf <- raster::extract(x = costs, y = buf, fun = mean, na.rm = T, sp = T)
  colnames(buf@data)[ncol(buf@data)] <- "Distanz"
  out@data <- buf@data
  
  
  # report status
  try(tkconfigure(tk_lab, text = "Korrigiere Start-Features, die sich mit Ziel-Features �berschneiden..."), silent = T)
  try(tkconfigure(tk_pb, value = 95, maximum = 100), silent = T)
  
  
  # correct for starting features which intersect target features
  inters <- raster::intersect(out, toPoints)
  if (nrow(inters) > 0){
    inters_pos <- which(is.na(over(out, inters)[,1]) == F)
    if (length(inters_pos) > 0) out@data[inters_pos, "Distanz"] <- 0
  }
  
  # close progress bar
  try(tkdestroy(tk),  silent = T)
      
  return(list(out_poly = out, out_ras = costs))
  
}

# execute function
res <- calculateAccessibility(toPoints, fromPoints, networkLines, studyArea, transRasCellSize)
  

# write to file
Ausgabe_Shapefile = res$out_poly
Ausgabe_Raster = res$out_ras
