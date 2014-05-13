#####-------------------------------------------##########
#     CODIGO PARA LA VERIFICACION GEOGRAFICA             #
#                                                        #
#               SET DE  FUNCIONES                        #
#                   julio 2013
# se introdujo insitutcion colector y fecha inicial, 
#     para los datos que vienen de Taxonomica              
#####-------------------------------------------##########

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

rutas<-function(){
  ruta_datos<<-(dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE ESTAN SUS DATOS")$res)
  ruta_salida<<-(dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE DESEA SUS RESULTADOS")$res)                 
  ruta_info_geo<<-(dlgDir(default = getwd(), title="RUTA DONDE ESTA LA INFORMACIÓN GEOGRÁFICA")$res)
  }

#   # debug
#    ruta_datos <- "C:/Google Drive/DINAVIS/sets/set2/"
#    ruta_salida <- "C:/Google Drive/DINAVIS/sets/set16/"
#    ruta_info_geo <- "C:/IAvH/VERIFICACION PAISES VECINOS/Info geografica"
# # 
#   ruta_datos <- "C:/IAvH/Chicharrones/JeissonZamudio_VerifGeo_Debug/"
#   ruta_salida <- ruta_datos


## info geogarfica
info_geografica<-function(ruta_info_geo){
  setwd(ruta_info_geo) 
  ALT<<-readAsciiGrid("alt.asc")
  casco<<-readShapePoly("centros_pob_100k.shp")
  mpios<<-readShapePoly("mun_2011_wgs84_100k.shp")
  mpios2003<<-readShapePoly("mun_2003.shp")
  mpios1993<<-readShapePoly("mun_1993.shp")
  mpios1985<<-readShapePoly("mun_1985.shp")
  mpios1973<<-readShapePoly("mun_1973.shp")
  mpios1964<<-readShapePoly("mun_1964.shp",delete_null_obj=TRUE)
  id<<-readAsciiGrid("cell_id.asc")
  paises<<-readShapePoly("PAISES_COMPLETO5.shp")
}

##CARGAR DATOS
CARGAR_DATOS <- function(ruta_datos){
  setwd(ruta_datos) ### definir el directorio donde estan loa archivos, OJO CAMBIAR SEGUN LA UBICACION DE LA BASE DE DATOS A USAR.
  ObjR <<- VT <<- MIO <<- IPT <<- "NO"
  ObjR<<-winDialog("yesno", "DO YOUR DATA CAME FROM PREVIOUS .RData file?")
  if (ObjR=="NO"){
    MIO<<-winDialog("yesno", "IS YOUR OWN DATA?")
    if (MIO=="NO"){
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
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="UTF-8",sep ="\t")
    set3<<-set3[,c(1,20,7,31,32,4,3,26)]
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }
  
  if (MIO=="YES"){
    setwd(ruta_datos)
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="UTF-8",sep ="\t")
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }
#debug 
  if (ObjR=="YES"){
    ObjR <- "YES"
    setwd(ruta_datos)
    load(file.choose())
    set2 <<-set2
    set3 <<- set2[,c("ID","source","especie_aceptada","country","adm1","adm2","lat","lon","earliestDateCollected","locality")]
    names(set3) <<-c("id","source","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
    #dbg load("C:/Google Drive/Descarga20140303/set3/Jorge_aves2.RData")
    #dbg datos <- set3
  }

    str(set3) # verficar que los  campos de latitud y longirud  cargen en formato numerico 
    cat(" \n   ... Verficar que los  campos de latitud y longitud cargen en formato numérico ... \n \n")
    }

#HACER GRAFICOS INFORME 
graficos=function(){
  summary(set16,stringsAsFactors =F )
  
  registros=c(p0/p0*100,p2/p0*100,p3/p0*100,p4/p0*100,p5/p0*100, p6/p0*100,p7/p0*100)
  names(registros)=c("Inicial","País","Departamen","Municipio","Urbano","Extremo_Altitud","Perfectos")
  
  barplot(registros, main="REGISTROS MANTENIDOS",axes=T,las=2,font.axis=3,cex.lab=1,ylab="%")
  tabla_resumen<<-registros
  
  
  # plotear ubicacion de los registros
  extent=extent(c(-180, 180, -90, 83.62))
  plot(extent(extent),main="UBICACION REGISTROS", ylab="latitud",xlab="longitud")
  plot(ubicacion, col="red",add=T);map(database = "world",add=T)
  plot(ubicacion[which(is.na(ubicacion$bienPais)),], col="black",add=T);
  
  
  #plotear  elementos POR PAISES 
  plot(extent(paises),main="DATOS Y PAISES", ylab="latitud",xlab="longitud")
  plot(ubicacion[which(ubicacion$sugerencia_pais=="CO" & ubicacion$bienPais==1),], col="blue",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="BR" & ubicacion$bienPais==1),], col="green",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="VE" & ubicacion$bienPais==1),], col="black",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="EC" & ubicacion$bienPais==1),], col="yellow",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="PE" & ubicacion$bienPais==1),], col="grey",add=T);
  plot(ubicacion[which(ubicacion$sugerencia_pais=="PA" & ubicacion$bienPais==1),], col="magenta",add=T);
  map(database = "world",add=T)
  legend("bottomleft",c("COLOMBIA","BRASIL","VENEZUELA","ECUADOR","PERU","PANAMA"), text.col =c("blue","green","black","yellow","grey","magenta"), fill="white")
  
  write.table(registros,"tabla_resumen.txt",sep ="\t",row.names=F) 
}

# METRICA DESEMPEÑO
DESEMPEÑO=function(tabla_resumen){
  names(tabla_resumen)=c("Inicial","País","Departamen","Municipio","Urbano","Extremo_Altitud","Perfectos")
  
  
  METRICA<<-winDialog("yesno", "¿DESEA VER CAMBIOS RESPECTO A OTRA VERIFICACION?")
  SET_PREVIO<<-winDialog("yesno", "¿TIENE UNA VERIFICACION PREVIA?")
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
corroboracion_dep <- function(datos,mun){
  a <- Sys.time()
  str("   i. Coordenadas")
  coordinates(datos)=~longitud+latitud1
  str("   ii. Overlay")
  ovm <- overlay(datos, mun)
  cntrm <- as.character(mun@data$DPTOS[ovm])
  if(length(which(!is.na(datos@data$departamento)))>0){
    (imx <- which(cntrm!=as.character(datos@data$departamento)| is.na(datos@data$departamento))) ## datos con diferente municipio
    (jmx <- which(cntrm==as.character(datos@data$departamento))) ## datos con igual municipio
    (diferente <- cbind(cntrm,as.character(datos@data$departamento))[imx,])
    CompareDpto <- cbind(imx,diferente)
    #(MunCorrecto <- cbind(cntrm,as.character(datos@data$municipio))[jm,])
    ma1x <- NULL 
    uniqueDpto <- unique(CompareDpto[,2]) # Saco valores únicos por municipio
    (uniqueDpto <- uniqueDpto[!is.na(uniqueDpto)]) # Eliminos NA's de los municipios
    cat("  iii. Ciclo \n")
    i <- 1
    cat("  iii. Ciclo -", i,"de",length(uniqueDpto),"-",round(i/length(uniqueDpto),2)*100,"% \n")
    for (i in 1:length(uniqueDpto)){
      (pos.mun <- which(CompareDpto[,2] == uniqueDpto[i]));(uniqueDpto[i])
      (mun.i <- CompareDpto[pos.mun,3])
      (tmp <- agrep(uniqueDpto[i],mun.i, max=4,value=F,ignore.case=T))
      max <- cbind(CompareDpto[pos.mun,1],0)
      max[tmp,2] <- 1
      ma1x <- rbind(ma1x,max)
    }
    kmx <- as.integer(ma1x[which(ma1x[,2]==0),1]) # municipio diferente
    lmx <- as.integer(ma1x[which(ma1x[,2]==1),1]) # municipio igual 
    mmx <- sort(as.integer(c(jmx,lmx)))
    nmx <- sort(c(imx,kmx))
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
#debug{
    # datos <- IGeo[100001:100100,]; datos
    # IGeo2 <- IGeo[100001:100100,]
    #mun <- paises
    #plot(mun)
    #points(datos$longitud, datos$latitud, col = 'orange')
#}
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
      (tmp <- agrep(CompareMun[i,2],CompareMun[i,3], max=4,value=F,ignore.case=T))
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
corroboracion <- function(datos,mun){
  (a <- Sys.time())  
  cat("    i. Coordenadas \n")
  coordinates(datos)=~longitud+latitud1
  cat("   ii. Overlay \n")
  ovm <- overlay(datos,mun)#;  str(ovm)
  (cntrm <- as.character(mun@data$MPIOS[ovm]))
  if(length(which(!is.na(datos@data$municipio)))>0){
    (imx <- which(cntrm!=as.character(datos@data$municipio)| is.na(datos@data$municipio))) ## datos con diferente municipio
    (jmx <- which(cntrm==as.character(datos@data$municipio))) ## datos con igual municipio
    (diferente <- cbind(cntrm,as.character(datos@data$municipio))[imx,])
    (CompareMun <- cbind(imx,diferente))
    #(MunCorrecto <- cbind(cntrm,as.character(datos@data$municipio))[jm,])
    ma1x <- NULL 
    uniqueMun <- unique(CompareMun[,2]) # Saco valores únicos por municipio
    (uniqueMun <- uniqueMun[!is.na(uniqueMun)]) # Eliminos NA's de los municipios
    cat("  iii. Ciclo \n")
    i <- 1
    cat("  iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n")
    for (i in 1:length(uniqueMun)){
      (pos.mun <- which(CompareMun[,2] == uniqueMun[i]));(uniqueMun[i])
      (mun.i <- CompareMun[pos.mun,3])
      (tmp <- agrep(uniqueMun[i],mun.i, max=4,value=F,ignore.case=T))
      max <- cbind(CompareMun[pos.mun,1],0)
      max[tmp,2] <- 1
      ma1x <- rbind(ma1x,max)
    }
    cat("iii. Ciclo -", i,"de",length(uniqueMun),"-",round(i/length(uniqueMun),2)*100,"% \n")
    kmx <- as.integer(ma1x[which(ma1x[,2]==0),1]) # municipio diferente
    lmx <- as.integer(ma1x[which(ma1x[,2]==1),1]) # municipio igual 
    mmx <- sort(as.integer(c(jmx,lmx)))
    nmx <- sort(c(imx,kmx))
  }else{
    mmx=rep(0,nrow(datos))
    nmx=rep(NA,nrow(datos))
  }
  Xx <- list()
  Xx[[1]] <- mmx
  Xx[[2]] <- nmx
  Xx[[3]] <- cntrm
  return(Xx)
  cat(print(a-Sys.time()))
}

VERIFICACION_PAISES=function(ruta_salida,set3,ALT,mundo,colombia,casco,mpios,mpios2003,mpios1993,mpios1985,mpios1973,mpios1964,id,paises){
  setwd(ruta_salida) ### definir el directorio donde se guardaran los archivos
  dim(set3)
  # NO LATITUD
  
  if (ObjR=="YES"){
    cat("  1. Evaluando registros registros con coordenadas", "\n")
    print((Sys.time()))}
  set3[nrow(set3)+1,]=NA #
  set3$latitud1<-set3$latitud#  
  lat=set3$latitud1#
  
  row.without.lat=which(is.na(lat)) # filas sin latitud #
  conlat=rep(NA,length(lat)) ;conlat[-row.without.lat]=1 #Vector resultados #
  set3conlat=cbind(set3,conlat)  # pega vector a tabla #
  
  rm(lat,row.without.lat)
  # NO LONGITUD
  set3conlat[nrow(set3conlat)+1,]=NA #
  row.without.lon=which(is.na(set3conlat$longitud)) ## filas sin longitud #
  conlon=rep(NA,nrow(set3conlat)) ; conlon[-row.without.lon]=1 #Vector resultados #
  set5Coord=cbind(set3conlat,conlon) # pega vector a tabla #
  set5<-set5Coord[-which(is.na(set5Coord$conlon) | is.na(set5Coord$conlat)),] #

  rm(set3conlat,row.without.lon)
  # ### 6. CONCORDANCIA PAISES -------------------------------------------
  #save.image("C:/Google Drive/Descarga20140303/set3/verif_geo.RData")
  
  
  if (ObjR=="YES"){
    cat("  2. Evaluando concordancia del país", "\n")
    print((Sys.time()))}
  
  ubicacion=set5 # 
  coordinates(ubicacion)=~longitud+latitud #
  ##evaluar otros paises
  
  bien_pais <- overlay(ubicacion,paises)# 10 minutos
  sugerencia_pais <- paises@data$PAIS[bien_pais]#
  set5$bienPais <- 0 #debug add
  set5$bienPais[which(set5$pais == sugerencia_pais)] <- 1#
  set5C=cbind(set5,sugerencia_pais)#
  length(which(set5$pais == sugerencia_pais)); length(set5$pais)#
    #plotear todos los elemnetos
  
  ubicacion<<-set5C #
  ubicacion <- set5C # debug add
  
  # coordinates(ubicacion)<<-~longitud+latitud1
  coordinates(ubicacion)<-~longitud+latitud1 # debug
  
  # ##### 7. CONSISTENCIA GEOGRÁFICA DEPARTAMENTOS MUNICIPIOS--------------------------------------
  
  #  DEPARTAMENTOS------
  if (ObjR=="YES"){
    cat("  3. Evaluando concordancia de departamentos", "\n")
    print((Sys.time()))}
  
  IGeo <- set5C #
  DEP <- corroboracion_dep(IGeo,paises) #12 seg
  bien_depto <- rep(NA, nrow(IGeo)); bien_depto[DEP[[1]]] <- 1 #
  sugerencia_depto <- DEP[[3]] #
  set6=cbind(IGeo,bien_depto,sugerencia_depto) #
  ## ----MUNICIPIOS------
  
  ### Revisar "overlay" de los registros con el "Shape" de municipio
  
  #Corroboracion municipios todos los paises
  IGeom=set6 #
  A <- corroboracion(IGeom,paises) #
  bien_mun=rep(NA,nrow(IGeo)) ;bien_mun[A[[1]]]=1 #
  bien_mun=cbind(set6$id,bien_mun) #
  sugerencia_mun=A[[3]] #
  rm(DEP,bien_depto,sugerencia_depto,set5C,set5)
  rm(A, IGeo)
  
  ##CORROBORACION CON MUNICIPIOS DE 1964
  #  set para colombia 
  if (ObjR=="YES"){
    cat("  4. Evaluando concordancia de municipios (1964)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}

  set6col=subset(set6,set6$sugerencia_pais=="CO") #
  #selcciona los que no han pasado
  select=bien_mun[which(is.na(bien_mun[,2])),1] #
  set6A=set6col[which(set6col$id %in% select),] #
  # corroborar
  B=corroboracion(set6A,mpios1964) #
  #selcionar filas de los que pasaron
  filas=set6A$id[B[[1]]] #
  # Rellenar con 1 los que pasaron 
  bien_mun[bien_mun[,1]%in%filas,2]=1 #
  
  rm(B)
  
  ##CORROBORACION CON MUNICIPIOS DE 1973
  
  if (ObjR=="YES"){
    cat("  5. Evaluando concordancia de municipios (1973)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select=bien_mun[which(is.na(bien_mun[,2])),1] #
  set6A=set6col[which(set6col$id%in% select),] #
  C=corroboracion(set6A,mpios1973)#
  filas=set6A$id[C[[1]]]#
  bien_mun[bien_mun[,1]%in%filas,2]=1#
  rm(C)
  
  ##CORROBORACION CON MUNICIPIOS DE 1985
  
  if (ObjR=="YES"){
    cat("  6. Evaluando concordancia de municipios (1985)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select=which(is.na(bien_mun[,2]))#
  set6A=IGeom[select,]#
  D=corroboracion(set6A,mpios1985)#
  filas=set6A$id[D[[1]]]#
  #bien_mun[filas,2]=1#NS
  bien_mun[bien_mun[,1]%in%filas,2] <- 1# debug
  rm(D)
  
  ##CORROBORACION CON MUNICIPIOS DE 1993
  if (ObjR=="YES"){
    cat("  7. Evaluando concordancia de municipios (1993)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select=which(is.na(bien_mun[,2]))#
  set6A=IGeom[select,]#
  E=corroboracion(set6A,mpios1993)#
  filas=set6A$id[E[[1]]]#
  #bien_mun[filas,2]=1# S?
  bien_mun[bien_mun[,1]%in%filas,2] <- 1 #debug
  rm(E)
  
  ##CORROBORACION CON MUNICIPIOS DE 2003
  if (ObjR=="YES"){
    cat("  8. Evaluando concordancia de municipios (2003)", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  select=which(is.na(bien_mun[,2])) #
  set6A=IGeom[select,] #
  G=corroboracion(set6A,mpios2003) # 76'
  filas=set6A$id[G[[1]]] #
  bien_mun[filas,2]=1
  rm(G)
  
  set8=cbind(IGeom,bien_mun[,2],sugerencia_mun) #
  rm(IGeom,bien_mun,sugerencia_mun)
  
  # # RURAL/URBANO ----------------------------------------------------------
  
  
  #### Revisar si los registros se encuentran en areas urbanas o rurales
  
  if (ObjR=="YES"){
    cat("  9. Evaluando registros para áreas rurales y urbanas", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  IGeor=set8 #
  
  coord8=cbind(IGeor$longitud,IGeor$latitud1) #
  
  mr=SpatialPoints(coord8) #
  rm(coord8)
  
  en_casco=overlay(mr,casco) #
  rural <- rep("NA",nrow(set8))  #
  rural[which(is.na(en_casco))]=1 #
  
  set10 <- cbind(set8,rural) #
  
  # #### 8. DUPLICADOS GEOGRAFICOS  -----------------------------------------
  
  if (ObjR=="YES"){
    cat("  10. Evaluando duplicados", "\n")
    cat("     ",as.character(Sys.time()),"\n")}
  
  Igeodup <- set10  #
  coordinates(Igeodup)=~longitud+latitud1 #
  
  celda <- over(Igeodup,id) #
  
  select <- cbind(set10$id,set10$nombre,celda) #
  names(select) <- c("id","nombre","celda") #
  duplicados <- duplicated(select[-which(select$nombre=="NA_NA"),2:3]) #
  unidos <- as.data.frame(cbind(select[-which(select$nombre=="NA_NA"),c(1,3)],duplicados)) #
  unidos$duplicados[which(unidos$duplicados==0)] <- NA #
  names(unidos) <- c("id","celda","duplicados") #
  set12 <- merge(set10,unidos,by="id",all=T) #
  rm(duplicados,set10,unidos)
  rm(Igeodup)
  
  #   
  nombres=names(set12) #
  nombres[which(nombres=="bien_mun[, 2]")]="bien_muni" #
  names(set12)=nombres #
  
  # ### 9. EXTREMOS EN ALTURA -----------------------------------------------
  if (ObjR=="YES"){
    cat("11. Evaluando elevaciones de los registros", "\n")
    cat("     ",as.character(Sys.time()),"\n")
  }
  
  IGeoALT2=set12 #
  coordALT2=cbind(IGeoALT2$longitud,IGeoALT2$latitud1) #
  coordinates(IGeoALT2)=~longitud+latitud1 #
  prealt=over(IGeoALT2,ALT) #
  colnames(prealt)="alt" #
  set12$alt=prealt  #
  
  # inicia metodo para detectar outliers (modified z-score method) 
  (listsp <- unique(set12$nombre)) #
  #listsp[length(listsp)+1]="NA NA" #
  #listsp=listsp[-which(listsp=="NA_NA"|is.na(listsp))]
  rm(IGeoALT2)
  #save.image("C:/Google Drive/Descarga20140303/set3/falta_elevacion.RData")
  
  preset16<-NULL #
  set16<-NULL #
  w <- 1;listsp[w]
  for (w in 1:length(listsp))
  {
    cat(w,"de",length(listsp), round((w/length(listsp))*100,2) ,"%",listsp[w])
    DAT <- subset(set12,set12$nombre==listsp[w]); dim(DAT); cat (" -",nrow(DAT),"registros","\n")
    DAT$extremo <- rep(NA,nrow(DAT))#debug
    v <- as.matrix(DAT$alt)[,1]  
    v <- na.omit(v); length(v)
    if (length(v)!=0){
      s <- median(v,na.rm=T)
      N <- length(v)
      #m=c() # inicializa vector de datos |xi - xm | donde xm es la median de altura maxima
      m <- abs(v-s) #debug add
      MAD <- median(m, na.rm=T) # mediana de todos los datos |xi - xm | calculados
      if (MAD>0) { 
        pZ=cbind(1:N,abs(0.6745*(v-s)/MAD)) # formula para calculo de outliers segun 										   # el modified z-score method 
        Z <- which(pZ[,2]>3.5)
        if (length(Z)!=0){
          DAT$extremo[-Z]=1} # 1 no es extremo
        }  #end if MAD
      #set15<-rbind(set15,preset15) # datos con valores extremos por altura maxima
    }
    preset16<-rbind(preset16,DAT) # datos sin valores extremos por altura maxima, listos para entrar al modelo
  }   #end for w
  
  dim(preset16)
  dim(preset16)
  preset16$extremo[which(is.na(preset16$alt))]="NA"
  nombres=names(preset16)
  nombres[which(nombres=="bien_mun[, 2]")]="bien_muni"; nombres[which(nombres=="nombre")]="nombre_acept"
  names(preset16)=nombres
  
  # por si no hay nombres NA o NA_NA
  if(length(which(set12$nombre=="NA_NA"|is.na(set12$nombre)))!=0){
    set12NA_NA=set12[which(set12$nombre=="NA_NA"|is.na(set12$nombre)),]
    set12NA_NA$extremo="NA"
    names(set12NA_NA)=nombres
    preset16b=rbind(preset16,set12NA_NA)
  }else{
    preset16b=preset16  
  }
   
  set16=preset16b[order(preset16b$id),]
  p16=dim(set16)[1]
  
  # ####10.  PREPARAR SETS DE DATOS PARA CORRECCION  ----------------------------
  if (ObjR=="YES"){
    cat("  12. Escribiendo datos", "\n")
    cat("     ",as.character(Sys.time()),"\n")}

  summary(set16)
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
    head(set16)
    pasa_todo<<-set16[which(set16$bienPais==1 & set16$bien_depto==1 & set16$bien_muni==1 & set16$rural==1 & is.na(set16$extremo)),]
    set16 <<- cbind(set2,set16[,-which((colnames(set16)%in%c("id","source","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")))])
    set16 <- cbind(set2,set16[,-which((colnames(set16)%in%c("id","source","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")))])  
    save(set16,file = paste0(ruta_salida,"/set16_GBIF_",as.Date(Sys.Date()),".RData"))
  }
  

  p0=nrow(set3)
  p1=nrow(set16[which(!is.na(set16$aceptadoCol)),]) 
  p2=nrow(set16[which(set16$bienPais==1),])
  p3<-nrow(set16[which(set16$bien_depto==1),]) 
  p4<-nrow(set16[which(set16$bien_muni==1),]) 
  p5<-nrow(set16[which(set16$rural==1),])
  p6<-nrow(set16[which(set16$extremo==1),])
  p7<-nrow(pasa_todo) 
  
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
  (setwd(ruta_salida))
  write.table(sin_coord,"sin_coord.txt",sep ="\t",row.names=F) 
  write.table(error_pais,"error_pais.txt",sep ="\t",row.names=F)
  write.table(mal_departamento,"mal_departamento.txt",sep ="\t",row.names=F)
  write.table(mal_municipio,"mal_municipio.txt",sep ="\t",row.names=F)
  write.table(casco_urbano,"casco_urbano.txt",sep ="\t",row.names=F)
  write.table(duplicados,"duplicados.txt",sep ="\t",row.names=F)
  write.table(extremos_altura,"extremos_altura.txt",sep ="\t",row.names=F)
  write.table(set16,"TABLA_GENERAL.txt",sep ="\t",row.names=F)
  write.table(pasa_todo,"PERFECTOS.txt",sep ="\t",row.names=F)
  }
  head(set16)
  sort(unique(set16$species))
  set16<<-set16
  J=list()
  J[[1]]=set5Coord
  J[[2]]=set16
  J[[3]]=p0 
  J[[4]]=p1
  J[[5]]=p2
  J[[6]]=p3
  J[[7]]=p4
  J[[8]]=p5
  J[[9]]=p6
  J[[10]]=p7
  J[[11]]=ubicacion
  
  colnames(set3);  dim(set3)
  colnames(set16);  dim(set16)
  return(J)
  #getwd()
  #save(set16,file="C:/IAvH/Chicharrones/Jorge_spAcuaticasVerifGeo/TABLA_GENERAL_set16_2.RData")
  #write.table(set16,"Para analisis en R TABLA GENERAL.txt",sep ="\t",row.names=F)
  cat("\n \n \n  Fin del código \n")
}