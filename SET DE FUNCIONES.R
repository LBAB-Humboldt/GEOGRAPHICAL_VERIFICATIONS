#     CODIGO PARA LA VERIFICACION GEOGRAFICA             #
#                                                        #
#               SET DE  FUNCIONES                        #
#                 Septiembre 2014

# ### FUNCIONES -----------------------------------------------------------
## definir rutas

library(dismo)
library(maptools)
library(sp)
library(maps)
library("svDialogs")
library("xlsx")
library("xlsxjars")
library("rJava")
library(R.utils)


rutas<-function(){
  ruta_datos <<- (dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE ESTAN SUS DATOS")$res)
  ruta_salida <<- (dlgDir(default = ruta_datos, title="ESPECIFIQUE LA RUTA DONDE DESEA SUS RESULTADOS")$res)                 
  ruta_info_geo <<- (dlgDir(default = ruta_salida, title="RUTA DONDE ESTA LA INFORMACI?N GEOGR?FICA")$res)
  }


## info geogarfica
info_geografica<-function(ruta_info_geo){
  setwd(ruta_info_geo) 
  ALT<<-readAsciiGrid("alt.asc")
  id<<-readAsciiGrid("cell_id.asc")
  casco<<-readShapePoly("centros_pob_100k.shp")
  mpios<<-readShapePoly("mun_2011_wgs84_100k.shp")
  mpios2003<<-readShapePoly("mun_2003.shp")
  mpios1993<<-readShapePoly("mun_1993.shp")
  mpios1985<<-readShapePoly("mun_1985.shp")
  mpios1973<<-readShapePoly("mun_1973.shp")
  mpios1964<<-readShapePoly("mun_1964.shp",delete_null_obj=TRUE)
  paises<<-readShapePoly("PAISES_COMPLETO5.shp")
  mar <<- readShapePoly("LimMaritimo.shp")
}

##CARGAR DATOS8
CARGAR_DATOS <- function(ruta_datos){
  setwd(ruta_datos) ### definir el directorio donde estan loa archivos, OJO CAMBIAR SEGUN LA UBICACION DE LA BASE DE DATOS A USAR.
  ObjR <<- VT <<- MIO <<- IPT <<- "NO"
  MIO<<-winDialog("yesno", "IS YOUR OWN DATA?")
  if (MIO=="NO"){
    ObjR<<-winDialog("yesno", "DO YOUR DATA CAME FROM PREVIOUS .RData file?")
    if (ObjR=="NO"){
      VT<<-winDialog("yesno", "DO YOUR DATA CAME FROM PREVIOUS SCRIPT VERIFICACION TAXONOMICA?")
      if (VT=="NO"){
        IPT<<-winDialog("yesno", "DO YOUR DATA CAME FROM IPT or SIB or GBIF?")
      }}}
  
  if (VT=="YES"){
    load(file.choose()) ## Cargar objeto derivado de Verificacion Taxonomica.
    set0=db$db
    nombre_acept=paste(cofTable2[,"genero_aceptado"],cofTable2[,"epiteto_aceptado"],sep="_")
    
    set3<<-cbind(cofTable2[,1:2],cofTable2[,"es_aceptadoCoL"],cofTable2[,"genero_aceptado"],cofTable2[,"epiteto_aceptado"],
                 nombre_acept,cofTable2[,14:16],cofTable2[,18],set0[,c(3:8,11,12,13,16)],stringsAsFactors =F)
    
    names(set3)<<-c("id","especie","aceptadoCol","generoCol","epitetoCol","nombre","familiaCol","ordenCol","claseCol",
                  "reinoCol","pais","departamento","municipio","localidad","latitud","longitud","institucion", "colleccion","catalogNumber", "FechaColectado")
  }
  #### de IPT 
  if (IPT=="YES"){
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="latin1",sep ="\t")
    set3<<-set3[,c(1,20,7,31,32,4,3,26)]
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }
  
  if (MIO=="YES"){
    setwd(ruta_datos)
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".", encoding="latin1",sep ="\t")
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }

  if (ObjR=="YES"){
    setwd(ruta_datos)
    load(file.choose())
    set3 <<- set2[,c("ID","source","nombre_aceptado","country","adm1","adm2","lat","lon","earliestDateCollected","locality")]
    set2 <<- set2
    colnames(set3) <<-c("id","source","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }

    str(set3) # verficar que los  campos de latitud y longirud  cargen en formato numerico 
    cat(" \n   ... Verficar que los  campos de latitud y longitud cargen en formato num?rico ... \n \n")
    }

#HACER GRAFICOS INFORME 
graficos <- function(){
  summary(set16, stringsAsFactors = F)
  ubicacion <- as.data.frame(ubicacion[, ])
  coordinates(ubicacion) =~ longitud+ latitud
  
  registros <- c(p0/p0*100,p2/p0*100,p3/p0*100,p4/p0*100,p5/p0*100, p6/p0*100,p7/p0*100)
  names(registros) <- c("Inicial","Pais","Departamen","Municipio","Urbano","Extremo altitud","Perfectos")
  
  barplot(registros, main="REGISTROS MANTENIDOS",axes=T,las=2,font.axis=3,cex.lab=1,ylab="%")
  tabla_resumen <<- registros
  
  # plotear ubicacion de los registros
  extent <- extent(c(-180, 180, -90, 83.62))
  plot(extent(extent), main = "UBICACION REGISTROS", ylab = "latitud", xlab = "longitud")
  plot(ubicacion, col = "red", add = T, cex = 0.1, pch = 20)
  map(database = "world",add=T)
  plot(ubicacion[which(is.na(ubicacion$bienPais)), ], col = "black", add = T)
  
  #plotear  elementos POR PAISES 
  plot(extent(paises),main="DATOS Y PAISES", ylab="latitud",xlab="longitud")
  #plot.paises <- ubicacion[ubicacion$bienPais==1,]
  #coordinates(ubicacion)=~longitud+latitud
  plot(ubicacion[which(ubicacion$sugerencia_pais=="CO" & ubicacion$bienPais==1),], col="blue",add=T)
  plot(ubicacion[which(ubicacion$sugerencia_pais=="BR" & ubicacion$bienPais==1),], col="green",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="VE" & ubicacion$bienPais==1),], col="black",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="EC" & ubicacion$bienPais==1),], col="yellow",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="PE" & ubicacion$bienPais==1),], col="grey",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="PA" & ubicacion$bienPais==1),], col="magenta",add=T);
  map(database = "world",add=T)
  legend("bottomleft",c("COLOMBIA","BRASIL","VENEZUELA","ECUADOR","PERU","PANAMA"), text.col =c("blue","green","black","yellow","grey","magenta"), fill="white")
  
  write.table(registros,"tabla_resumen.txt",sep ="\t",row.names=F) 
}

# METRICA DESEMPE?O
DESEMPENO <- function(tabla_resumen){
  names(tabla_resumen)=c("Inicial","Pa?s","Departamen","Municipio","Urbano","Extremo_Altitud","Perfectos")  
  METRICA<<-winDialog("yesno", "?DESEA VER CAMBIOS RESPECTO A OTRA VERIFICACION?")
  SET_PREVIO<<-winDialog("yesno", "?TIENE UNA VERIFICACION PREVIA?")
  if (METRICA=="YES" & SET_PREVIO=="YES"){
    cat("SELECCIONES SU ARCHIVO DEL RESUMEN PREVIO= TABLA_RESUMEN.txt")
    PREVIO<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="UTF-8",sep ="\t")
    
  }
  
  CONJUNTA=cbind(rownames(tabla_resumen),PREVIO, tabla_resumen)
  barplot(t(CONJUNTA), beside = TRUE,main="REGISTROS MANTENIDOS",axes=T,las=2,font.axis=3,cex.lab=1,ylab="%",col=c("darkblue","red"))
  RESTA=CONJUNTA[,2]-CONJUNTA[,1]
  barplot(RESTA, main="% DE CAMBIO",axes=T,las=2,font.axis=3,cex.lab=1,ylab="%")

}

### Revisar "overlay" de los registros con el "Shape" de departamento
#datos <- IGeo; mun <- paises
corroboracion_dep <- function(datos, mun){
  a <- Sys.time()
  cat("   i. Coordenadas",'\n')
  coordinates(datos)=~longitud+latitud1
  cat("   ii. Overlay",'\n')
  ovm <- overlay(datos, mun)
  cntrm <- as.character(mun@data$DPTOS[ovm])
  if(length(which(!is.na(datos@data$departamento))) == 1){
    tmp <- agrep(mun$DPTOS[ovm],datos$municipio, max = 1, value=F,ignore.case=T)
    mmx <- c(1,1,1)[tmp]
    nmx <- NA
  }
  if(length(which(!is.na(datos@data$departamento)))>0){
    (imx <-  which(gsub(" ", "", tolower(cntrm)) != gsub(" ", "",tolower(datos@data$departamento))| is.na(datos@data$departamento))) ## datos con diferente municipio
    (jmx <-  which(gsub(" ", "", tolower(cntrm)) == gsub(" ", "",tolower(datos@data$departamento))))## datos con igual municipi
    id.exa <- datos@data$id[jmx]
    CompareDpto <- cbind(imx, cntrm[imx], datos@data$departamento[imx], datos@data$id[imx]); head(CompareDpto)
    ma1x <- NULL 
    uniqueDpto <- unique(CompareDpto[, 3]) # Saco valores ?nicos por municipio
    (uniqueDpto <- uniqueDpto[!is.na(uniqueDpto) & uniqueDpto != ""]) # Eliminos NA's de los municipios
    cat("  iii. Ciclo \n")
    i <- 19
    cat("  iii. Ciclo -", i,"de",length(uniqueDpto),"-",round(i/length(uniqueDpto),2)*100,"% \n")
    for (i in 1:length(uniqueDpto)){
      (pos.mun <- which(CompareDpto[, 3] == uniqueDpto[i]));(uniqueDpto[i])
      (mun.i <- CompareDpto[pos.mun, 2])
      (tmp <- agrep(gsub(" ", "", uniqueDpto[i]), gsub(" ","",mun.i), max = 2, value=F, ignore.case=T))
      max <- cbind(CompareDpto[pos.mun, 4], 0)
      max[tmp,2] <- 1
      ma1x <- rbind(ma1x,max)
    }
    kmx <- as.integer(ma1x[which(ma1x[,2]==0),1]) # municipio diferente
    lmx <- as.integer(ma1x[which(ma1x[,2]==1),1]) # municipio igual 
    
    mmx <- sort(as.integer(c(id.exa,lmx))) # ID's correctos
    nmx <- sort(c(imx,kmx)) # ID's incorrectos
  }else{
    mmx=rep(0,nrow(datos))
    nmx=rep(NA,nrow(datos))
  }
  Xx <- list()
  Xx[[1]] <- mmx
  Xx[[2]] <- nmx
  Xx[[3]] <- cntrm
  return(Xx)
  cat(print(a-Sys.time())[1])
}

corroboracion2=function(datos,mun){
  cat("    i. Coordenadas \n")
  coordinates(datos)=~longitud+latitud1
  cat("   ii. Overlay \n")
  ovm <- overlay(datos,mun);  str(ovm)
  (cntrm <- as.character(mun@data$MPIOS[ovm]))
  if(length(which(!is.na(datos@data$departamento)))>0){
    im <- which(cntrm!=as.character(datos@data$municipio)| is.na(datos@data$municipio)) ## datos con diferente municipio
    jm <- which(cntrm==as.character(datos@data$municipio)) ## datos con igual municipio
    diferente <- cbind(cntrm,as.character(datos@data$municipio))[im,]
    CompareMun <- cbind(im,diferente)
    MunCorrecto <- cbind(cntrm,as.character(datos@data$municipio))[jm,]
    ma1=NULL 
    cat("   iii. Ciclo \n")
    i <- 2
    for (i in 1:nrow(CompareMun)){
      cat("   iii. Ciclo -", i,"de",nrow(CompareMun),"-",round(i/nrow(CompareMun),2)*100,"% \n")
      (tmp <- agrep(CompareMun[i,2],CompareMun[i,3], max = 4,value = F,ignore.case=T))
      if (length(tmp)==0){tmp=0}
      ma<-c(CompareMun[i,1],tmp)
      ma1 <- rbind(ma1,ma)
    }
    km=as.integer(ma1[which(ma1[,2]==0),1]) # municipio diferente
    lm=as.integer(ma1[which(ma1[,2]==1),1]) # municipio igual 
    mm=sort(as.integer(c(jm,lm)))
    nm=sort(c(im,km))
  }else{
    mm=rep(0,nrow(datos))
    nm=rep(NA,nrow(datos))
  }
  X=list()
  X[[1]]=mm
  X[[2]]=nm
  X[[3]]=cntrm
  return(X)
}

# datos <- IGeom; mun <- paises; layer.field <- "NAME_2"; data.field <- "municipio"
# datos <- set6A; mun <- mpios1964
# datos <-IGeo;  mun <- paises; layer.field <- "DPTOS"; data.field <- "departamento"
corroboracion <- function(datos, mun, layer.field, data.field){
  (a <- Sys.time())
  cat("    i. Coordenadas \n")
  coordinates(datos)=~longitud+latitud1
  cat("   ii. Overlay \n")
  ovm <- overlay(datos, mun)#;  str(ovm)
  assign("cntrm", eval(parse(text = paste0("as.character(mun@data$",layer.field,"[ovm])"))))
  assign("l", eval(parse(text = paste0("length(which(!is.na(datos@data$",data.field,")))"))))
  if(l == 1){
    assign("tmp", eval(parse(text = paste0("agrep(mun$",layer.field,"[ovm], datos$",data.field,", max = 1, value=F, ignore.case=T)"))))
    mmx <- c(1, 1, 1)[tmp]
    nmx <- NA
  } else if(l > 0){
    assign("jmx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) == gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con igual entidad
    assign("imx", eval(parse(text = paste0("which(gsub(' ', '', tolower(cntrm)) != gsub(' ', '', tolower(datos@data$",data.field,")))")))) ## datos con diferente entidad
    na.mx <- which(is.na(cntrm))
    id.exa <- datos@data$id[jmx] # ID's de las filas exactas 
      
    CompareMun <- cbind(imx, cntrm[imx], datos@data$municipio[imx], datos@data$id[imx])
    assign("CompareMun", eval(parse(text = paste0("cbind(imx, cntrm[imx], datos@data$", data.field,"[imx], datos@data$id[imx])")))) 

    uniqueMun <- (sort(unique(CompareMun[, 3]))) # Saco valores unicos por municipio reportados en tabla
    (uniqueMun <- uniqueMun[which(!is.na(uniqueMun) & uniqueMun != "" & gsub(" ", "", uniqueMun) != "")]) # Eliminos NA's de los municipios
    mmx <- c(0, 0)
    nmx <- c(0, 0)
    
    if (length(uniqueMun) > 0){
      cat("  iii. Ciclo \n")
      i <- 1
      ma1x <- NULL
      cat("  iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n")
      for (i in 1:length(uniqueMun)){
        (uniqueMun[i])
        (pos.mun <- which(CompareMun[, 3] == uniqueMun[i])) # Selecciono posiciones que cotienen al municipio i del over
        (mun.i <- CompareMun[pos.mun, 2]) # selecciono municipios de tabla para el municipio de tabla i
        (tmp <- agrep(gsub(" ", "", uniqueMun[i]), gsub(" ", "",mun.i), max = 2, value=F, ignore.case=T)) #Comparo similitud entre municipios reportados en tabla y extraidos con coordenada
        (max <- cbind(as.integer(CompareMun[pos.mun, 4]), 0)) # Genero tabla con resultados
        (max[tmp, 2] <- 1) #Asigno 1 para los que esten bien
        (ma1x <- rbind(ma1x,max))
      }
      cat("  iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n") 
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
  cat(print(a-Sys.time()))
}

VERIFICACION_PAISES=function(ruta_salida,set3,ALT,mundo,colombia,casco,mpios,mpios2003,mpios1993,
                             mpios1985,mpios1973,mpios1964,id,paises, evalAlt = FALSE, removeMaps = FALSE){
  setwd(ruta_salida) ### definir el directorio donde se guardaran los archivos
  set3$latitud <- as.numeric(set3$latitud)
  set3$longitud <- as.numeric(set3$longitud)
  set3$municipio <- iconv(set3$municipio)
  set3$pais <- iconv(set3$pais)
  set3$departamento <- iconv(set3$departamento)
  set3$localidad <- iconv(set3$localidad)
  dim(set3)
  
  # NO LATITUD
  
  cat("  1. Evaluando registros registros con coordenadas (1 de 11)", "\n")
  
  set3$latitud1<-set3$latitud#  
  lat <- set3$latitud1#
  (p0 <- nrow(set3))
  
  row.without.lat <- which(!is.na(lat)) # filas sin latitud #
  conlat <- rep(NA,length(lat))
  conlat[row.without.lat] <- 1 #Vector resultados #
  set3conlat <- cbind(set3,conlat)  # pega vector a tabla #
  
  rm(lat,row.without.lat)
  
  # NO LONGITUD
  row.without.lon <- which(!is.na(set3conlat$longitud)) ## filas sin longitud #
  conlon <- rep(NA,nrow(set3conlat))
  conlon[row.without.lon] <- 1 #Vector resultados #
  set5Coord <- cbind(set3conlat,conlon) # pega vector a tabla #
  set5 <- set5Coord[which(!is.na(set5Coord$conlon) & !is.na(set5Coord$conlat)), ] #

  rm(set3conlat, row.without.lon, conlat, conlon)
  
  # ### 6. CONCORDANCIA PAISES -------------------------------------------  
  cat("  2. Evaluando concordancia del pais (2 de 11)", "\n")
  
  ubicacion <- set5 # 
  coordinates(ubicacion)=~longitud+latitud #
  ubicacion <<- ubicacion
  ##evaluar otros paises
  
  bien_pais <- overlay(ubicacion, paises)# 10 minutos
  sugerencia_pais <- as.character(paises@data$ISO3[bien_pais])#
  sugerencia_pais2 <- as.character(paises@data$NAME_0[bien_pais])
  sugerencia_paises <- cbind(unique(sugerencia_pais), unique(sugerencia_pais2))
  sugerencia_paises <- sugerencia_paises <- na.omit(sugerencia_paises)
  
  set5$bienPais <- 0 
  for (p in 1:nrow(sugerencia_paises)){
    set5$bienPais[grep(tolower(sugerencia_paises[p, 1]), tolower(set5$pais))] <- 1    
    set5$bienPais[which(tolower(set5$pais) == tolower(sugerencia_paises[p, 2]))] <- 1
  }
  set5$bienPais[which(set5$pais == sugerencia_pais)] <- 1#

  IGeo <- cbind(set5,sugerencia_pais)#
  
  ubicacion<<-IGeo #
  ubicacion <- IGeo # debug add
  
  rm(bien_pais)
  
  coordinates(ubicacion)<-~longitud+latitud1 # debug
  
  # ##### 7. CONSISTENCIA GEOGR?FICA DEPARTAMENTOS MUNICIPIOS--------------------------------------
  #  DEPARTAMENTOS------
  cat("  3. Evaluando concordancia de departamentos (3 de 11)", "\n")
  
  DEP <- corroboracion(IGeo, paises, "NAME_1", "departamento") #12 seg
  bien_depto <- rep(NA, nrow(IGeo))
  bien_depto[IGeo[, 1] %in% DEP[[1]]] <- 1 #
  sugerencia_depto <- DEP[[3]] #
  set6 <- cbind(IGeo, bien_depto, sugerencia_depto) #

  ## ----MUNICIPIOS------
  
  ### Revisar "overlay" de los registros con el "Shape" de municipio
  
  #Corroboracion municipios todos los paises
  IGeom <- set6 #
  A <- corroboracion(IGeom, paises, "NAME_2", "municipio")   
  bien_mun <- cbind(set6$id, rep(NA, nrow(IGeo)), "Mapa" = rep(NA, nrow(IGeo))) # 
  bien_mun[bien_mun[, 1] %in% A[[1]], 2] <- 1 #
  bien_mun[bien_mun[, 1] %in% A[[1]], 3] <- 2102 #
  
  sugerencia_mun <- A[[3]] # 

  rm(DEP,bien_depto,sugerencia_depto, set5)
  rm(A, IGeo)
  
  ##CORROBORACION CON MUNICIPIOS DE 2011
  #  set para colombia 
  if (ObjR=="YES"){
    cat("  4. Evaluando concordancia de municipios (2011) (4 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  if (removeMaps == TRUE) {
    rm(list = c('paises'))
  }
  
  #selcciona los que no han pasado
  set6col <- subset(set6,set6$sugerencia_pais=="COL") #
  select <- bien_mun[which(is.na(bien_mun[, 2])), 1] #
  set6A <- set6col[which(set6col$id %in% select), ] #
  
  B <- corroboracion(set6A, mpios, "MPIOS", "municipio") #
  bien_mun[bien_mun[, 1] %in% B[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% B[[1]], 3] <- 2011
  
  rm(B)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios'))
  }
  
  ##CORROBORACION CON MUNICIPIOS DE 2003
  #  set para colombia 
  if (ObjR=="YES"){
    cat("  4. Evaluando concordancia de municipios (2003) (4 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  #selcciona los que no han pasado
  set6col <- subset(set6,set6$sugerencia_pais=="COL") #
  select <- bien_mun[which(is.na(bien_mun[, 2])), 1] #
  set6A <- set6col[which(set6col$id %in% select), ] #
  
  B <- corroboracion(set6A, mpios2003, "MPIOS", "municipio") #
  bien_mun[bien_mun[, 1] %in% B[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% B[[1]], 3] <- 2003
  
  rm(B)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios2003'))
  }
  
  ##CORROBORACION CON MUNICIPIOS DE 1993
  
  if (ObjR=="YES"){
    cat("  5. Evaluando concordancia de municipios (1993) (5 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select <- bien_mun[which(is.na(bien_mun[,2])),1] #
  set6A <- set6col[which(set6col$id%in% select),] #
  C <- corroboracion(set6A, mpios1993, "MPIOS", "municipio")#
  bien_mun[bien_mun[, 1] %in% C[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% C[[1]], 3] <- 1993
  
  rm(C,set6col)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios1993'))
  }
  
  ##CORROBORACION CON MUNICIPIOS DE 1985
  
  if (ObjR=="YES"){
    cat("  6. Evaluando concordancia de municipios (1985) (6 de 11)", "\n")
    cat("     ",as.character(Sys.time()), "\n")}
  
  select <- which(is.na(bien_mun[, 2]))#
  set6A <- IGeom[select, ]#
  D <- corroboracion(set6A, mpios1985, "MPIOS", "municipio")#
  bien_mun[bien_mun[, 1] %in% D[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% D[[1]], 3] <- 1985
  
  rm(D)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios1985'))
  }
  
  ##CORROBORACION CON MUNICIPIOS DE 1973
  if (ObjR=="YES"){
    cat("  7. Evaluando concordancia de municipios (1973) (7 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select <- which(is.na(bien_mun[, 2]))#
  set6A <- IGeom[select, ]#
  E <- corroboracion(set6A, mpios1973, "MPIOS", "municipio")#
  bien_mun[bien_mun[, 1] %in% E[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% E[[1]], 3] <- 1973
  
  rm(E)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios1973'))
  }

  ##CORROBORACION CON MUNICIPIOS DE 1964
  if (ObjR == "YES"){
    cat("  8. Evaluando concordancia de municipios (1964) (8 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select <- which(is.na(bien_mun[,2])) #
  set6A <- IGeom[select,] #
  G <- corroboracion(set6A, mpios1964, "MPIOS", "municipio") # 76'
  bien_mun[bien_mun[, 1] %in% G[[1]], 2] <- 1
  bien_mun[bien_mun[, 1] %in% G[[1]], 3] <- 1964
  
  rm(G,set6, set6A)
  
  if (removeMaps == TRUE) {
    rm(list = c('mpios1964'))
  }

  
  set8 <- cbind(IGeom, "bien_muni" = bien_mun[, 2], sugerencia_mun, "mapa_verificacion" = bien_mun[, 3]) #

  rm(IGeom,bien_mun,sugerencia_mun)
  
  coord8 <- cbind(set8$longitud, set8$latitud1) #
  mr <- SpatialPoints(coord8) 
    
  set8$sugerencia_mun <- tolower(as.character(over(mr, mpios)$MPIOS))
  set8$sugerencia_mun <- gsub(pattern = "\\b([a-z])", replacement = "\\U\\1", set8$sugerencia_mun, perl = TRUE)
  
  # # RURAL/URBANO ----------------------------------------------------------  
  #### Revisar si los registros se encuentran en areas urbanas o rurales
  
  if (ObjR=="YES"){
    cat("  9. Evaluando registros para areas rurales y urbanas (9 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}

  rm(coord8)
  
  en_casco <- overlay(mr,casco) #
  rural <- rep("NA",nrow(set8))  #
  rural[which(is.na(en_casco))]=1 #
  
  if (ObjR=="NO"){
    over_mar=overlay(mr,mar) #
    en_mar <- which(!is.na(over_mar))
    en_col <- which(set8$pais == "CO" | gsub(" ", "", tolower(set8$pais)) == "colombia")
    set8$bienPais[en_mar[en_mar%in%en_col]] <- 1  
  }
  
  set10 <- cbind(set8,rural) #
  rm(rural,set8,en_casco)
  # #### 8. DUPLICADOS GEOGRAFICOS  -----------------------------------------
  
  if (ObjR=="YES"){
    cat("  10. Evaluando duplicados (10 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")
  }
  
  Igeodup <- set10  #
  coordinates(Igeodup)=~longitud+latitud1 #
  
  celda <- over(Igeodup, id) #
  
  select <- cbind(set10$id, set10$nombre, celda) #
  names(select) <- c("id", "nombre", "celda") #
  duplicados <- duplicated(select[-which(select$nombre=="NA_NA"),2:3]) #
  unidos <- as.data.frame(cbind(select[-which(select$nombre=="NA_NA"), c(1, 3)], duplicados)) #
  unidos$duplicados[which(unidos$duplicados==0)] <- NA #
  names(unidos) <- c("id","celda","duplicados") #
  
  set12 <- merge(set10, unidos, by="id",all=T) #
  rm(duplicados, set10, unidos)
  rm(Igeodup, celda, select)

  
  # ### 9. EXTREMOS EN ALTURA -----------------------------------------------
  if (ObjR=="YES"){
    cat("11. Evaluando elevaciones de los registros (11 de 11)", "\n")
    cat("     ",as.character(Sys.time()),"\n")
  }
  
  coordALT2 <- data.frame(longitud = set12$longitud, latitud1 = set12$latitud1) #
  coordinates(coordALT2)=~longitud+latitud1 #
  prealt <- over(coordALT2, ALT) #
  colnames(prealt) <- "alt" #
  set12$alt <- as.numeric(prealt[,1])  #
  rm(coordALT2)
  
 
  # inicia metodo para detectar outliers (modified z-score method) 
  listsp <- unique(set12$nombre) #
  rm(IGeoALT2)

  preset16<-NULL #
  time.alt <- Sys.time()

  if (evalAlt == TRUE){
    altDATA <- cbind(set12[, c("id","nombre","alt")],"extremo" = NA) 
    altDATA <- NA
    count <- 0
    time.alt <- Sys.time()
    for (w in 1:length(listsp)) # 
    {
      cat(w,"de",length(listsp), round((w/length(listsp))*100,2) ,"%",listsp[w])
      pos <- which(altDATA$nombre==listsp[w] & !is.na(altDATA$alt))
      v <- altDATA[pos,c("id","alt")]; cat (" -",nrow(v),"registros")#; dim(DAT)
      if (nrow(v)>0){
        s <- median(v$alt,na.rm=T)
        N <- length(v$alt)
        m <- abs(v$alt-s) #debug add 
        MAD <- median(m, na.rm=T) # mediana de todos los datos |xi - xm | calculados
        if (MAD > 0) { 
          pZ <- cbind(v$id, abs(0.6745*(v$alt-s)/MAD)) # formula para calculo de outliers segun  # el modified z-score method
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
    preset16 <- cbind(set12, extremo = altDATA$extremo)
    set16 <- preset16[order(preset16$id), ]
  } else {
    set16 <- set12[order(set12$id), ]
  }
  
  if (removeMaps == TRUE) {
    rm(list = c('ALT', 'id', 'casco','mar'))
  }
  

  p16 <- nrow(set16)
  
  set16$sugerencia_pais <- capitalize(set16$sugerencia_pais)
  set16$sugerencia_depto <- capitalize(set16$sugerencia_depto)
  set16$sugerencia_mun <- capitalize(set16$sugerencia_mun)
  # ####10.  PREPARAR SETS DE DATOS PARA CORRECCION  ----------------------------
  if (ObjR=="YES"){
    cat("  12. Escribiendo datos", "\n")
    cat("     ",as.character(Sys.time()),"\n")}

  if(VT=="YES"){
    nombre_no_valido<-set16[which(is.na(set16$aceptadoCol)),]
    sin_coord<<-set5Coord[which(is.na(set5Coord$conlon)),]
    error_pais<<-set16[which(is.na(set16$bienPais)),]
    mal_departamento<<-set16[which(is.na(set16$bien_depto)),]
    mal_municipio<<-set16[which(is.na(set16$bien_muni)),]
    casco_urbano<<-set16[which(is.na(set16$rural)),]
    duplicados<<-set16[which(set16$duplicados==1),]
    extremos_altura<<-set16[which(set16$extremo==1),]
    pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)),]
    write.table(nombre_no_valido,"nombre_no_valido.txt",sep ="\t",row.names=F)
  }
  if(IPT=="YES"){
    sin_coord<<-set5Coord[which(is.na(set5Coord$conlon)),]
    error_pais<<-set16[which(is.na(set16$bienPais)),]
    mal_departamento<<-set16[which(is.na(set16$bien_depto)),]
    mal_municipio<<-set16[which(is.na(set16$bien_muni)),]
    casco_urbano<<-set16[which(is.na(set16$rural)),]
    duplicados<<-set16[which(set16$duplicados==1),]
    extremos_altura<<-set16[which(set16$extremo==1),]
    pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)),]
  }
  if(MIO=="YES"){
    sin_coord<<-set5Coord[which(is.na(set5Coord$conlon)),]
    error_pais<<-set16[which(is.na(set16$bienPais)),]
    mal_departamento<<-set16[which(is.na(set16$bien_depto)),]
    mal_municipio<<-set16[which(is.na(set16$bien_muni)),]
    casco_urbano<<-set16[which(is.na(set16$rural)),]
    duplicados<<-set16[which(set16$duplicados==1),]
    extremos_altura<<-set16[which(set16$extremo==1),]
    pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1  & is.na(set16$extremo)),]
  }
  if (ObjR=="YES"){
    pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)), ] 
    quitar.columnas <- which(colnames(set16)%in%c("id", "source", "nombre", "pais", "departamento", "municipio", "latitud", "latitud1", "longitud", "fecha_inicial", "localidad", "alt", "nombre_acept", "Nombre", "conlat", "conlon"))
    set16 <- cbind(set2[order(set2$ID),],set16[order(set16$id),-quitar.columnas])
    save(set16,file = paste0(ruta_salida,"/Registros_",as.Date(Sys.Date()),".RData"))
    save(set16,file = paste0("W:/Ocurrencias/Registros_",as.Date(Sys.Date()),".RData"))
    set16 <<- set16
  }
  cat("\n \n \n  Fin del codigo \n")
  

  (p1 <- nrow(set16[which(!is.na(set16$nombre)),]))
  (p2 <- nrow(set16[which(set16$bienPais==1),]))
  (p3 <- nrow(set16[which(set16$bien_depto==1),])) 
  (p4 <- nrow(set16[which(set16$bien_muni==1),]))
  (p5 <- nrow(set16[which(set16$rural==1),]))
  (p6 <- nrow(set16[which(set16$extremo==1),]))
  (p7 <- nrow(pasa_todo))
  
  if(is.null(p1)) {p1<-NA}
  if(is.null(p2)) {p2<-NA}
  if(is.null(p3)) {p3<-NA}
  if(is.null(p4)) {p4<-NA}
  if(is.null(p5)) {p5<-NA}
  if(is.null(p6)) {p6<-NA}
  if(is.null(p7)) {p7<-NA}
  
  p0<<-p0
  p1<<-p1
  p2<<-p2
  p3<<-p3
  p4<<-p4
  p5<<-p5
  p6<<-p6
  p7<<-p7
  
  if (ObjR=="NO"){
    setwd(ruta_salida)
    write.table(sin_coord,"sin_coord.txt",sep ="\t",row.names=F) 
    write.table(error_pais,"error_pais.txt",sep ="\t",row.names=F)
    write.table(mal_departamento,"mal_departamento.txt",sep ="\t",row.names=F)
    write.table(mal_municipio,"mal_municipio.txt",sep ="\t",row.names=F)
    write.table(casco_urbano,"casco_urbano.txt",sep ="\t",row.names=F)
    write.table(duplicados,"duplicados.txt",sep ="\t",row.names=F)
    write.table(extremos_altura,"extremos_altura.txt",sep ="\t",row.names=F)
    write.table(set16,"TABLA_GENERAL.txt",sep ="\t",row.names=F)
    write.table(pasa_todo,"PERFECTOS.txt",sep ="\t",row.names=F)
    write.table(merge(set3, set16[,-c(2:10)], by = "id", all.x = T), "TABLA_COMPLETA.txt", sep ="\t", row.names=F)
  }

  set16<<-set16
  J <- list()
  J[[1]] <- set5Coord
  J[[2]] <- set16
  J[[3]] <- p0 
  J[[4]] <- p1
  J[[5]] <- p2
  J[[6]] <- p3
  J[[7]] <- p4
  J[[8]] <- p5
  J[[9]] <- p6
  J[[10]] <- p7
  J[[11]] <- ubicacion
  
 
  return(J)

  cat("\n \n \n  Fin del codigo \n \n \n \n")
  
  if (ObjR=="YES"){
    col <- set16[which(set16$enCol ==1),c("species","speciesOriginal","especie_aceptada","bienPais","bien_depto","bien_muni","rural","extremo")]
    resumen.salida <- rbind(
      c(nrow(set16),nrow(col)),
      c(length(which(set16$bienPais == 1)),length(which(col$bienPais==1))),
      c(length(which(set16$bienPais == 1 & set16$bien_depto == 1)),length(which(col$bienPais == 1 & col$bien_depto == 1))),
      c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1)),length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1))),
      c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural==1)),length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1 & col$rural==1))),
      c(length(which(set16$bienPais == 1 & set16$bien_depto == 1 & set16$bien_muni == 1 & set16$rural==1 & is.na(set16$extremo))),length(which(col$bienPais == 1 & col$bien_depto == 1 & col$bien_muni == 1 & col$rural == 1 & is.na(col$extremo))))
    )
    colnames(resumen.salida) <- c("Area de estudio", "Colombia")
    rownames(resumen.salida) <- c("Total","Pais","Departamento","Municipio","Rural", "Altura")      
    write.csv(resumen.salida, "W:/Ocurrencias/Resumen/06 Resumen_salida_set16.csv")
  }
}
