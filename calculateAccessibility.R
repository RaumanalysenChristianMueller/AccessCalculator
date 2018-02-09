# Calculates accessibility of points for whole study areas using a travel cost analysis based on a street or path network.
# It is written in R and optimized for integration in QGIS
# Raumanalysen - Christian Müller
# raumanalysen@mailbox.org

# data input from QGIS
##Raumanalysen_ChristianMueller = group
##Zu_erreichende_Punkte = vector
##Wegenetz = vector
##Untersuchungsgebiet = vector
##Gitterzellengroesse_fuer_Wegenetz = number
##Gitterzellengroesse_der_Ausgabe = number
##Ausgabe_Shapefile_Flaechen = output vector
##Ausgabe_Shapefile_Punkte = output vector

# test parameters for windows
# Zu_erreichende_Punkte = readOGR("C:/HochschuleBochum/Daten/Bochum/Hiwis_Datenerhebung_Bochum/einzelPOIs", "OwnSurvey_rathaus")
# Wegenetz = readOGR("C:/HochschuleBochum/Daten/Bochum/Netzwerke", "FussWanderwege")
# Untersuchungsgebiet = readOGR("C:/HochschuleBochum/Daten/Bochum", "StudyArea")
# Gitterzellengroesse_fuer_Wegenetz = 50
# Gitterzellengroesse_der_Ausgabe = 250

# test parameters for linux
# Zu_erreichende_Punkte = readOGR("/media/sf_HochschuleBochum/Daten/Bochum/Hiwis_Datenerhebung_Bochum/einzelPOIs", "OwnSurvey_rathaus")
# Wegenetz = readOGR("/media/sf_HochschuleBochum/Daten/Bochum/Netzwerke", "FussWanderwege")
# Untersuchungsgebiet = readOGR("/media/sf_HochschuleBochum/Daten/Bochum", "StudyArea")
# Gitterzellengroesse_fuer_Wegenetz = 50
# Gitterzellengroesse_der_Ausgabe = 250


# rewrite variable names
fromPoints <- Zu_erreichende_Punkte
networkLines <- Wegenetz
studyArea <- Untersuchungsgebiet
costRasCellSize <- Gitterzellengroesse_fuer_Wegenetz
gridCellSize <- Gitterzellengroesse_der_Ausgabe

shortestDistance <- function(fromPoints, networkLines, studyArea,
                             costRasCellSize, gridCellSize){
  
  # load packages
  cat("Loading R-Packages...")
  for (i in 1:2){
    if (!require("gdistance")) install.packages("gdistance", dependencies = T)
    if (!require("rgdal")) install.packages("rgdal", dependencies = T)
    if (!require("rgeos")) install.packages("rgeos", dependencies = T)
    if (!require("SpatialTools")) install.packages("SpatialTools", dependencies = T)
    if (!require("maptools")) install.packages("maptools", dependencies = T)
  }
  
  # create cost raster
  pr_bar <- winProgressBar(title = "AccessibilityCalculator - Raumanalysen_Christian_Müller",
                           label = "Transition raster wird erzeugt. Dieser Schritt dauer am längsten und erhöht sich entscheidend durch kleine Gitterzellengrößen für das Wegenetz und große Untersuchungsgebiete. Der Fortschritt während dieser Berechnung wird nicht angezeigt. Es geht gleich weiter...",
                           min = 1, max = 100, width = 500, initial = 60)
  source("createTransitionRaster.r")
  costTrans <- createTransitionRaster(networkLines, costRasCellSize, studyArea)  # revision until here (see subscript)
  
  
  # create output object
  out <- fromPoints
  frColN <- colnames(out@data)[1]
  out@data <- as.data.frame(out@data[,1])
  colnames(out@data) <- frColN
  
  # convert input polygons or lines to centroids
  # if (class(fromPoints)[1] != "SpatialPointsDataFrame"){
  #   fromPoints <- gCentroid(fromPoints, byid = T)
  #   fromPoints <- SpatialPointsDataFrame(coords = coordinates(fromPoints),
  #                                        proj4string = CRS(proj4string(fromPoints)),
  #                                        data = out@data)
  # }
  
  # snap starting points to lines
  # networkLines <- readOGR(dirname(networkLinesPath), strsplit(basename(networkLinesPath), ".", fixed = T)[[1]][1])
  # fromPoints <- snapPointsToLines(fromPoints, networkLines, maxDist = gridCellSize/2)
  
  # iterate over each starting point
  for (j in 1:length(toPointsPath)){
    
    thisFile <- toPointsPath[j]
    
    # load destination points (only if there is at least one feature)
    if (ogrInfo(dirname(thisFile), strsplit(basename(thisFile), ".", fixed = T)[[1]][1])$nrows > 0){
      
      toPoints <- readOGR(dirname(thisFile), strsplit(basename(thisFile), ".", fixed = T)[[1]][1])
      
      # convert vertices to points
      # if (class(toPoints)[1] != "SpatialPointsDataFrame"){
      #   if (class(toPoints)[1] == "SpatialLinesDataFrame"){
      #     toPoints <- as(toPoints, "SpatialPointsDataFrame")
      #   } else if (class(toPoints)[1] == "SpatialPolygonsDataFrame"){
      #     useCoords <- toPoints@polygons[[1]]@Polygons[[1]]@coords
      #     if (length(toPoints@polygons) < 1){
      #       for (p in 2:length(toPoints@polygons)){
      #         useCoords <- rbind(useCoords, toPoints@polygons[[p]]@Polygons[[1]]@coords)
      #       }
      #     }
      #     toPoints <- SpatialPointsDataFrame(useCoords, as.data.frame(rep(0, nrow(useCoords))))
      #   }
      # }
      getVertexCoordinates <- function(so){
        if (class(so) == "SpatialPointsDataFrame"){
          coords <- coordinates(so)
        } else if (class(so) == "SpatialLinesDataFrame"){
          coords <- c()
          lines <- so@lines
          for (l in 1:length(lines)){
            thisLines <- lines[[l]]@Lines
            for (ls in 1:length(thisLines)){
              coords <- rbind(coords, thisLines[[ls]]@coords)
            }
          }
        } else if (class(so) == "SpatialPolygonsDataFrame"){
          coords <- c()
          polys <- so@polygons
          for (p in 1:length(polys)){
            thisPolys <- polys[[p]]@Polygons
            for (ps in 1:length(thisPolys)){
              coords <- rbind(coords, thisPolys[[ps]]@coords)
            }
          }
        }
        return(coords)
      }
      useCoords <- getVertexCoordinates(toPoints)
      toPoints <- SpatialPointsDataFrame(useCoords, as.data.frame(rep(0, nrow(useCoords))))
      
      # add attribute field
      thissplit <- strsplit(basename(thisFile), "_", fixed = T)[[1]]
      thisName <- paste(outFieldPrefix, paste(thissplit[1:(length(thissplit)-1)], collapse = "_"), sep = "")
      thisName <- gsub(x = thisName, pattern = ".shp", replacement = "")
      out@data <- cbind(out@data, numeric(nrow(out@data)))
      colnames(out@data)[ncol(out@data)] <- thisName
      
      # calculate travel cost
      # s <- Sys.time()
      costs <- accCost(costTrans, toPoints)
      
      # resample to starting points
      ext <- extent(out)
      proj <- CRS(proj4string(out))
      ras <- raster(resolution = c(gridCellSize, gridCellSize), ext = ext, crs = proj)
      values(ras) <- NA
      pos <- which(values(costs) == Inf)
      if (length(pos) > 0) values(costs)[pos] <- NA
      outRas <- resample(costs, ras)
      
      # estimate travel distance from traveled number of cells as mean of minimum path length (horizontally or vertically straight) and maximum path length (first horizontally than vertically straight) through a cell
      # values(outRas) <- values(outRas) * ((costRasCellSize^2 + costRasCellSize)/2)
      
      # # estimate travel distance from traveled number of cells
      # out@data[,ncol(out@data)] <- values(outRas) * (costRasCellSize * 2)
      
      # estimate travel distance
      out@data[,ncol(out@data)] <- values(outRas)
      
      # correct for grid cells which intersect target features
      inters <- raster::intersect(out, toPoints)
      inters_pos <- which(is.na(over(out, inters)[,1]) == F)
      if (length(inters_pos) > 0) out@data[inters_pos, ncol(out@data)] <- 0
      
      
      # s <- Sys.time()
      # temp <- costDistance(costTrans, coordinates(fromPoints)[,1:2], coordinates(toPoints)[,1:2])
      # temp[which(temp == Inf)] <- NA
      # for(r in 1:nrow(temp)){
      #   out@data[r,ncol(out@data)] <- min(temp[r,], na.rm = T)
      # }
      # out@data[which(out@data[,ncol(out@data)] == Inf),ncol(out@data)] <- NA
      # out@data[,ncol(out@data)] <- costDistance(costTrans, coordinates(fromPoints)[,1:2], coordinates(toPoints)[,1:2])
      # spplot(out, "F1100_010")
      # e <- Sys.time()
      # e-s
      
      
      
      # for (i in 1:nrow(fromPoints@data)){
      #   
      #   # select three target destinations which are closest (euclidean) to start point
      #   frC <- cbind(coordinates(fromPoints[i,])[,1], coordinates(fromPoints[i,])[,2])
      #   toC <- coordinates(toPoints)[,1:2]
      #   edis <- dist2(frC, toC)
      #   eToPoints <- toPoints[order(edis)[1:3],]
      #   
      #   
      #   # calculate shortest paths
      #   Sys.time()
      #   thisShort <- shortestPath(costTrans, fromPoints[i,], eToPoints, "SpatialLines")
      #   Sys.time()
      #   thisShort2 <- costDistance(costTrans, fromPoints[i,], eToPoints)
      #   Sys.time()
      #   
      #   # get the very shortest path
      #   thisDis <- min(SpatialLinesLengths(thisShort), na.rm = T)
      #   
      #   # save to attribute table
      #   out@data[i,ncol(out@data)] <- thisDis
      #   
      # }
    }
  }
  
  
  # write to file
  writeOGR(out, dsn = dirname(outFilePath), layer = strsplit(basename(outFilePath), ".", fixed = T)[[1]][1],
           driver = 'ESRI Shapefile', overwrite_layer = T)
  
  # copy projection file
  file.copy(from = paste(strsplit(fromPointsPath, ".shp", fixed = T)[[1]][1], ".prj", sep = ""),
            to = paste(strsplit(outFilePath, ".shp", fixed = T)[[1]][1], ".prj", sep = ""), overwrite = T)
  
}

# write to file
Ausgabe_Shapefile_Flaechen = out_poly
Ausgabe_Shapefile_Punkte = out_points


