#datos <- set5; mun <- paises; layer.field <- "NAME_1"; data.field <- "departamento"
corroboracion <- function(datos, mun, layer.field, data.field){
  coordinates(datos)=~longitud+latitud
  ovm <- over(datos, mun)
  assign("cntrm", eval(parse(text = paste0("ovm$",layer.field))))
  assign("l", eval(parse(text = paste0("length(which(!is.na(datos@data$",data.field,")))"))))
  if(l == 1){
    assign("tmp", eval(parse(text = paste0("agrep(ovm$",layer.field,", datos$",data.field,", max = 1, value=F, ignore.case=T)"))))
    mmx <- c(1, 1, 1)[tmp]
    nmx <- NA
  } else if(l > 1){
    assign("jmx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) == gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con igual entidad
    assign("imx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) != gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con diferente entidad
    na.mx <- which(is.na(cntrm))
    id.exa <- datos@data$ID[jmx] # ID's de las filas exactas 
    
    CompareMun <- cbind(imx, cntrm[imx], datos@data$municipio[imx], datos@data$ID[imx])
    assign("CompareMun", eval(parse(text = paste0("cbind(imx, cntrm[imx], datos@data$", data.field,"[imx], datos@data$ID[imx])")))) 
    
    uniqueMun <- (sort(unique(CompareMun[, 3]))) # Saco valores unicos por municipio reportados en tabla
    (uniqueMun <- uniqueMun[which(!is.na(uniqueMun) & uniqueMun != "")]) # Eliminos NA's de los municipios
    mmx <- c(0, 0)
    nmx <- c(0, 0)
    
    if (length(uniqueMun) > 0){
      ma1x <- NULL
      for (i in 1:length(uniqueMun)){
        (uniqueMun[i])
        (pos.mun <- which(CompareMun[, 3] == uniqueMun[i])) # Selecciono posiciones que cotienen al municipio i del over
        (mun.i <- CompareMun[pos.mun, 2]) # selecciono municipios de tabla para el municipio de tabla i
        (tmp <- agrep(gsub(" ", "", uniqueMun[i]), gsub(" ", "",mun.i), max = 2, value=F, ignore.case=T)) #Comparo similitud entre municipios reportados en tabla y extraidos con coordenada
        (max <- cbind(as.integer(CompareMun[pos.mun, 4]), 0)) # Genero tabla con resultados
        (max[tmp, 2] <- 1) #Asigno 1 para los que esten bien
        (ma1x <- rbind(ma1x,max))
      }
      lmx <- ma1x[which(ma1x[, 2] == 1), 1] # municipio igual. Extraigo posiciones de tabla original con municipios reconocidos validos 
      mmx <- sort(as.integer(c(id.exa, lmx))) # Filas de la tabla con datos validados positivamente
      
      kmx <- ma1x[which(ma1x[, 2] == 0), 1] # municipio diferente. 
      nmx <- sort(c(imx, kmx, na.mx))
    } 
    if (length(id.exa) > 0 | length(uniqueMun) <= 0){
      mmx <- sort(as.integer(c(id.exa))) 
      nmx <- sort(c(na.mx))
    }
  } else {
    mmx <- rep(0, nrow(datos))
    nmx <- rep(NA, nrow(datos))
  }
  Xx <- list()
  Xx[[1]] <- mmx
  Xx[[2]] <- nmx
  Xx[[3]] <- cntrm
  return(Xx)
}

VERIFICACION_PAISES <- function(set3, routineType, evalElevation = FALSE){
  cat("\n\n\n\n  0. Loading information", "\n")
  
  set3$latitud <- as.numeric(set3$latitud)
  set3$longitud <- as.numeric(set3$longitud)
  
  cat("  1. Evaluating records coordinates (1 - 12)", "\n")
  
  # Latitud
  row.with.lat <- which(!is.na(set3$latitud)) # filas con latitud #
  set3$conlat <- NA
  set3$conlat[row.with.lat] <- 1 #Vector resultados #
  rm(row.with.lat)
  
  # Longitud
  row.with.lon <- which(!is.na(set3$longitud)) ## filas sin longitud #
  set3$conlon <- NA
  set3$conlon[row.with.lon] <- 1 #Vector resultados #
  
  set5 <- subset(set3, !is.na(set3$conlon) & !is.na(set3$conlat))

  rm(set3, row.with.lon)
  
  # ### 6. CONCORDANCIA PAISES -------------------------------------------  
  cat("  2. Evaluating country match (2 - 12)", "\n")
  
  ubicacion <- set5 
  coordinates(ubicacion) =~ longitud + latitud
  
  ##evaluar otros paises
  bien_pais <- over(ubicacion, paises)
  sugerencia_paises <- na.omit(data.frame("NOMB" = unique(bien_pais$NAME_0), "ISO2" = unique(bien_pais$ISO2), "ISO3" = unique(bien_pais$ISO3)))
  
  set5$bienPais <- 0 
  for (p in 1:nrow(sugerencia_paises)){
    set5$bienPais[grep(tolower(sugerencia_paises$NOMB[p]), tolower(set5$pais))] <- 1    
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO2[p]))] <- 1
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises$ISO3[p]))] <- 1
  }
  
  set5$bienPais[which(gsub(" ", "", set5$pais) == gsub(" ", "", bien_pais$NAME_0))] <- 1
  set5$sugerencia_pais <- bien_pais$NAME_0
  
  rm(bien_pais, ubicacion)
  
  # ##### 7. CONSISTENCIA GEOGRAFICA DEPARTAMENTOS MUNICIPIOS
  
  #  DEPARTAMENTOS
  cat("  3. Evaluating state/Province match (3 - 12)", "\n")
  DEP <- corroboracion(set5, paises, "NAME_1", "departamento") 
  bien_depto <- rep(NA, nrow(set5))
  bien_depto[set5[, 1] %in% DEP[[1]]] <- 1 
  sugerencia_depto <- DEP[[3]] #
  set5$bien_depto <- bien_depto
  set5$sugerencia_depto <- sugerencia_depto
  
  ## ----MUNICIPIOS------
  
  ### Revisar "over" de los registros con el "Shape" de municipio
  cat("  4. Evaluating municipality match (4 - 12)", "\n")
  
  #Corroboracion municipios todos los paises
  A <- corroboracion(set5, paises, "NAME_2", "municipio")   
  bien_mun <- cbind(set5$ID, rep(NA, nrow(set5)), "Mapa" = rep(NA, nrow(set5)))  
  bien_mun[bien_mun[, 1] %in% A[[1]], 2] <- 1 
  bien_mun[bien_mun[, 1] %in% A[[1]], 3] <- "Suramerica" 
  
  sugerencia_mun <- A[[3]] # 
  
  rm(DEP, bien_depto, sugerencia_depto)
  
  if (routineType == "Colombia") {
    
    ##CORROBORACION CON MUNICIPIOS DE 1964
    #  set para colombia 
    cat("  5. Evaluating municipality match (1964) (5 - 12)", "\n") 
    #selcciona los que no han pasado
    set6col <- subset(set5, set5$sugerencia_pais == "Colombia") #
    select <- bien_mun[which(is.na(bien_mun[, 2])), 1] #
    if(length(select)>0){
      set6A <- set6col[which(set6col$ID %in% select), ] #
      B <- corroboracion(set6A, mpios1964, "MPIOS", "municipio") #
      bien_mun[bien_mun[, 1] %in% B[[1]], 2] <- 1
      bien_mun[bien_mun[, 1] %in% B[[1]], 3] <- 1964  
      rm(B)
      
      ##CORROBORACION CON MUNICIPIOS DE 1973
      cat("  6. Evaluating municipality match (1973) (6 - 12)", "\n")
      
      select <- bien_mun[which(is.na(bien_mun[,2])), 1] #
      if(length(select)>0){
        set6A <- set6col[which(set6col$ID%in% select), ] #
        C <- corroboracion(set6A, mpios1973, "MPIOS", "municipio")#
        bien_mun[bien_mun[, 1] %in% C[[1]], 2] <- 1
        bien_mun[bien_mun[, 1] %in% C[[1]], 3] <- 1973
        rm(C, set6col)
        
        ##CORROBORACION CON MUNICIPIOS DE 1985
        
        cat("  7. Evaluating municipality match (1985) (7 - 12)", "\n")
        
        select <- which(is.na(bien_mun[, 2]))#
        if(length(select)>0){
          set6A <- set5[select,]#
          D <- corroboracion(set6A, mpios1985, "MPIOS", "municipio")#
          bien_mun[bien_mun[, 1] %in% D[[1]], 2] <- 1
          bien_mun[bien_mun[, 1] %in% D[[1]], 3] <- 1985
          rm(D)
          
          ##CORROBORACION CON MUNICIPIOS DE 1993
          cat("  8. Evaluating municipality match (1993) (8 - 12)", "\n")
          
          select <- which(is.na(bien_mun[,2]))#
          if(length(select)>0){            
            set6A <- set5[select,]#
            E <- corroboracion(set6A, mpios1993, "MPIOS", "municipio")#
            bien_mun[bien_mun[, 1] %in% E[[1]], 2] <- 1
            bien_mun[bien_mun[, 1] %in% E[[1]], 3] <- 1993
            rm(E)
            
            ##CORROBORACION CON MUNICIPIOS DE 2003
            cat("  9. Evaluating municipality match (2003) (9 - 12)", "\n")
            
            select <- which(is.na(bien_mun[,2])) #
            if(length(select)>0){
              set6A <- set5[select,] #
              G <- corroboracion(set6A, mpios2003, "MPIOS", "municipio") # 76'
              bien_mun[bien_mun[, 1] %in% G[[1]], 2] <- 1
              bien_mun[bien_mun[, 1] %in% G[[1]], 3] <- 2003    
              rm(G, set6A, select)
              
            }}}}}
    
    set5$bien_muni <- bien_mun[, 2]
    set5$sugerencia_mun <- sugerencia_mun
    set5$mapa <- bien_mun[, 3]
    rm(bien_mun, sugerencia_mun)
    
    # # RURAL/URBANO ----------------------------------------------------------  
    #### Revisar si los registros se encuentran en areas urbanas o rurales
    
    cat("  9. Evaluating records in rural and urban areas (10 - 12)", "\n")
    
    mr <- SpatialPoints(cbind(set5$longitud, set5$latitud)) #
    
    en_casco <- over(mr, casco) #
    rural <- rep("NA", nrow(set5))  #
    rural[which(is.na(en_casco[, 1]))] <-1 #
    
    over_mar <- over(mr,mar) #
    en_mar <- which(!is.na(over_mar[, 1]))
    en_col <- which(set5$pais == "CO" | gsub(" ", "", tolower(set5$pais)) == "colombia")
    set5$bienPais[en_mar[en_mar %in% en_col]] <- 1  
    
    set5$rural <- rural
    rm(rural, en_mar, en_casco)
    # #### 8. DUPLICADOS GEOGRAFICOS  -----------------------------------------
    
    cat("  10. Evaluating duplicity (11 - 12)", "\n")
    
    celda <- over(mr, id) #
    select <- data.frame(ID = set5$ID, nombre = set5$nombre, celda = celda) #
    NAname <- 'NA'
    duplicados <- duplicated(select[which(select$nombre != NAname), 2:3])
    unidos <- as.data.frame(cbind(select[which(select$nombre != NAname), c(1, 3)], duplicados))
    unidos$duplicados[which(unidos$duplicados == 0)] <- NA #
    names(unidos) <- c("ID", "celda", "duplicados") #
    
    set12 <- merge(set5, unidos, by = "ID", all = T) #
    rm(duplicados, unidos, celda, select, set5)
    
    
    # ### 9. EXTREMOS EN ALTURA -----------------------------------------------
    if (evalElevation) {
      cat("  11. Evaluating records elevation (12 - 12)", "\n")
      
      coordALT2 <- data.frame(longitud = set12$longitud, latitud = set12$latitud, ID = set12$ID)
      coordinates(coordALT2)=~longitud+latitud #
      prealt <- over(coordALT2, ALT) #
      colnames(prealt) <- "alt" #
      set12$alt <- as.numeric(prealt[,1])  #
      rm(coordALT2, prealt)
      
      # inicia metodo para detectar outliers (modified z-score method) 
      listsp <- unique(set12$nombre) #
      
      time.alt <- Sys.time()
      
      altDATA <- cbind(set12[, c("id", "nombre", "alt")], "extremo" = NA) 
      count <- 0
      time.alt <- Sys.time()
      for (w in 1:length(listsp)) # 
      {
        cat(w, "de", length(listsp), round((w/length(listsp))* 100, 2), "%", listsp[w])
        pos <- which(altDATA$nombre == listsp[w] & !is.na(altDATA$alt))
        v <- altDATA[pos, c("id", "alt")]; cat (" -", nrow(v), "registros")#; dim(DAT)
        if (nrow(v)>0){
          s <- median(v$alt,na.rm=T)
          N <- length(v$alt)
          m <- abs(v$alt-s) #debug add 
          MAD <- median(m, na.rm=T) # mediana de todos los datos |xi - xm | calculados
          if (MAD > 0) { 
            pZ <- cbind(v$ID, abs(0.6745*(v$alt-s)/MAD)) # formula para calculo de outliers segun  # el modified z-score method
            Z <- which(pZ[, 2]>3.5)
            if (length(Z)!=0){
              altDATA$extremo[pos[-Z]] <- 1
              count <- count + 1
              cat(" - Extremo", count)
            } # 1 no es extremo
          }
        }; cat("\n")
      }
      
      Sys.time() - time.alt
      set12$extremo <- altDATA$extremo
    }
    set16 <- set12
    rm(set12)
  } else {
    set16 <- cbind(set5, "bien_muni" = bien_mun[, 2], sugerencia_mun, mapa = bien_mun[, 3])
    rm(set5)
  }
  
  p16 <- dim(set16)[1]
  
  set16$sugerencia_pais <- capitalize(set16$sugerencia_pais)
  set16$sugerencia_depto <- capitalize(set16$sugerencia_depto)
  set16$sugerencia_mun <- capitalize(set16$sugerencia_mun)
  
  # ####10.  PREPARAR SETS DE DATOS PARA CORRECCION  ----------------------------
  cat("  12. Generating out table", "\n")
  
  #pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)), ] 
  set16Cols <- setdiff(colnames(set16), c("nombre", "pais", "departamento", "municipio", "latitud", "longitud"))
  
  set16 <- merge(x = set2, y = set16[, set16Cols], by = 'ID', sort = TRUE)
  set16 <<- set16[order(set16$ID),]
  
  (p <- nrow(set16))
  (p0 <- length(which(!is.na(set16$decimalLatitude), !is.na(set16$decimalLongitude))))
  (p1 <- length(which(!is.na(set16$scientificName))))
  (p2 <- length(which(set16$bienPais == 1)))
  (p3 <- length(which(set16$bien_depto == 1))) 
  (p4 <- length(which(set16$bien_muni == 1)))
  
  outFile <- list('set16' = set16, 'summary' = c(p, p0, p1, p2, p3, p4))
  cat("\n \n \n  Script end \n \n \n \n")
  return(outFile)
}