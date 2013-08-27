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

rutas<-function(){
  ruta_datos<<-(dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE ESTAN SUS DATOS")$res)
  ruta_salida<<-(dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE DESEA SUS RESULTADOS")$res)                 
  ruta_info_geo<<-(dlgDir(default = getwd(), title="RUTA DONDE ESTA LA INFORMACIÓN GEOGRÁFICA")$res)
  }

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
CARGAR_DATOS=function(ruta_datos){
  setwd(ruta_datos) ### definir el directorio donde estan loa archivos, OJO CAMBIAR SEGUN LA UBICACION DE LA BASE DE DATOS A USAR.
  
  VT<<-winDialog("yesno", "DO YOUR DATA CAME FROM PREVIOUS SCRIPT VERIFICACION TAXONOMICA?")
  IPT<<-winDialog("yesno", "DO YOUR DATA CAME FROM IPT or SIB or GBIF?")
  MIO<<-winDialog("yesno", "IS YOUR OWN DATA?")
  
  if (VT=="YES"){
    load(file.choose()) ## Cargar objeto derivado de Verificacion Taxonomica.
    set0=db$db
    nombre_acept=paste(cofTable2[,"genero_aceptado"],cofTable2[,"epiteto_aceptado"],sep="_")
    
    set3<<-cbind(cofTable2[,1:2],cofTable2[,"es_aceptadoCoL"],cofTable2[,"genero_aceptado"],cofTable2[,"epiteto_aceptado"],
                 nombre_acept,cofTable2[,14:16],cofTable2[,18],set0[,c(3:8,11,12,13,16)],stringsAsFactors =F)
    
    names(set3)<<-c("id","especie","aceptadoCol","generoCol","epitetoCol","nombre","familiaCol","ordenCol","claseCol",
                  "reinoCol","pais","departamento","municipio","localidad","latitud","longitud","institucion", "colleccion","catalogNumber", "FechaColectado")
    set0=cbind(cofTable2[,1],set0)
  }
  #### de IPT 
  if (IPT=="YES"){
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="UTF-8",sep ="\t")
    set3<<-set3[,c(1,20,7,31,32,4,3,26)]
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }
  
  if (MIO=="YES"){
    set3<<-read.delim(file.choose(),h=T,stringsAsFactors=F,dec=".",encoding="UTF-8",sep ="\t")
    names(set3)<<-c("id","nombre","pais","departamento","municipio","latitud","longitud","fecha_inicial", "localidad")
  }
  cat("verficar que los  campos de latitud y longirud  cargen en formato numerico")
  
  
  str(set3) # verficar que los  campos de latitud y longirud  cargen en formato numerico
    
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
corroboracion_dep=function(datos,mun){
  
  coordinates(datos)=~longitud+latitud1
  ovm=overlay(datos,mun)
  cntrm=as.character(mun@data$DPTOS[ovm])
  
  if(length(which(!is.na(datos@data$departamento)))>0){

    im=which(cntrm!=as.character(datos@data$departamento)) ## datos con diferente municipio
    jm=which(cntrm==as.character(datos@data$departamento)) ## datos con igual municipio
    
    diferente=as.data.frame(cbind(cntrm,as.character(datos@data$departamento)))[im,]
    
    CompareDpto=cbind(im,diferente)
    MunCorrecto=cbind(cntrm,as.character(datos@data$departamento))[jm,]
    
    ma1=NULL 
    for  ( i in 1:nrow(CompareDpto)){
      tmp<-agrep(CompareDpto[i,2],CompareDpto[i,3], max=4,value=F,ignore.case=T)
      if (length(tmp)==0) tmp=0
      ma<-c(CompareDpto[i,1],tmp)
      ma1=rbind(ma1,ma)
    }
    
    km=as.integer(ma1[which(ma1[,2]==0),1]) # municipio diferente
    lm=as.integer(ma1[which(ma1[,2]==1),1]) # municipio igual 
    
    mm=sort(as.integer(c(jm,lm)))# municipio igual 
    nm=sort(c(im,km))# municipio diferente
    
  } else{
    mm=rep(0,nrow(datos))
    nm=rep(NA,nrow(datos))
  }
  
  
  X=list()
  X[[1]]=mm # municipio igual 
  X[[2]]=nm# municipio diferente
  X[[3]]=cntrm
  return(X)
}
corroboracion=function(datos,mun){
  coordinates(datos)=~longitud+latitud1
  ovm=overlay(datos,mun)
  cntrm=as.character(mun@data$MPIOS[ovm])
  
  if(length(which(!is.na(datos@data$departamento)))>0){
  
  im=which(cntrm!=as.character(datos@data$municipio)| is.na(datos@data$municipio)) ## datos con diferente municipio
  jm=which(cntrm==as.character(datos@data$municipio)) ## datos con igual municipio
  
  diferente=cbind(cntrm,as.character(datos@data$municipio))[im,]
  
  CompareMun=cbind(im,diferente)
  MunCorrecto=cbind(cntrm,as.character(datos@data$municipio))[jm,]
  
  ma1=NULL 
  for  ( i in 1:nrow(CompareMun)){
    tmp<-agrep(CompareMun[i,2],CompareMun[i,3], max=4,value=F,ignore.case=T)
    if (length(tmp)==0) tmp=0
    ma<-c(CompareMun[i,1],tmp)
    ma1=rbind(ma1,ma)
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

VERIFICACION_PAISES=function (ruta_salida,set3,ALT,mundo,colombia,casco,mpios,mpios2003,mpios1993,mpios1985,mpios1973,mpios1964,id,paises){
  
  setwd(ruta_salida) ### definir el directorio donde se guardaran los archivos
  
  
   # NO LATITUD
  set3[nrow(set3)+1,]=NA
  set3$latitud1<-set3$latitud  
  lat=set3$latitud1
  
  row.without.lat=which(is.na(lat)) # filas sin latitud
  conlat=rep(NA,length(lat)) ;conlat[-row.without.lat]=1 #Vector resultados
  set3conlat=cbind(set3,conlat)  # pega vector a tabla
  
  # NO LONGITUD
  set3conlat[nrow(set3conlat)+1,]=NA
  row.without.lon=which(is.na(set3conlat$longitud)) ## filas sin longitud
  conlon=rep(NA,nrow(set3conlat)) ; conlon[-row.without.lon]=1 #Vector resultados
  set5Coord=cbind(set3conlat,conlon) # pega vector a tabla
  set5<-set5Coord[-which(is.na(set5Coord$conlon) | is.na(set5Coord$conlat)),]

  # ### 6. CONCORDANCIA PAISES -------------------------------------------
  
  ubicacion=set5
  
  coordinates(ubicacion)=~longitud+latitud
  
  ##evaluar otros paises
  
  bien_pais=overlay(ubicacion,paises)
  sugerencia_pais=paises@data$PAIS[bien_pais]
  set5$bienPais[which(set5$pais==sugerencia_pais)]=1
  set5C=cbind(set5,sugerencia_pais)
  
    #plotear todos los elemnetos
  
  ubicacion<<-set5C
  
  coordinates(ubicacion)<<-~longitud+latitud1
  
  
  # ##### 7. CONSISTENCIA GEOGRÁFICA DEPARTAMENTOS MUNICIPIOS--------------------------------------
  
  #  DEPARTAMENTOS------
  
  IGeo=set5C
  DEP=corroboracion_dep(IGeo,paises)
  bien_depto=rep(NA,nrow(IGeo)) ;bien_depto[DEP[[1]]]=1
  sugerencia_depto=DEP[[3]]
  set6=cbind(IGeo,bien_depto,sugerencia_depto)
  
  ## ----MUNICIPIOS------
  
  ### Revisar "overlay" de los registros con el "Shape" de municipio
  
  #Corroboracion municipios todos los paises
  IGeom=set6
  A=corroboracion(IGeom,paises)
  
  bien_mun=rep(NA,nrow(IGeo)) ;bien_mun[A[[1]]]=1
  bien_mun=cbind(set6$id,bien_mun)
  sugerencia_mun=A[[3]]
  
  
  ##CORROBORACION CON MUNICIPIOS DE 1964
  #  set para colombia 
  
  set6col=subset(set6,set6$sugerencia_pais=="CO")
  #selcciona los que no han pasado
  select=bien_mun[which(is.na(bien_mun[,2])),1] 
  set6A=set6col[which(set6col$id %in% select),] 
  # corroborar
  B=corroboracion(set6A,mpios1964)
  #selcionar filas de los que pasaron
  filas=set6A$id[B[[1]]]
  #rellenar con 1 los que pasaron 
  bien_mun[bien_mun[,1]%in%filas,2]=1
  
  
  ##CORROBORACION CON MUNICIPIOS DE 1973
  
  select=bien_mun[which(is.na(bien_mun[,2])),1] 
  set6A=set6col[which(set6col$id%in% select),] 
  C=corroboracion(set6A,mpios1973)
  filas=set6A$id[C[[1]]]
  bien_mun[bien_mun[,1]%in%filas,2]=1
  
  
  ##CORROBORACION CON MUNICIPIOS DE 1985
  select=which(is.na(bien_mun[,2]))
  set6A=IGeom[select,]
  D=corroboracion(set6A,mpios1985)
  filas=set6A$id[D[[1]]]
  bien_mun[filas,2]=1
  
  
  ##CORROBORACION CON MUNICIPIOS DE 1993
  select=which(is.na(bien_mun[,2]))
  set6A=IGeom[select,]
  E=corroboracion(set6A,mpios1993)
  filas=set6A$id[E[[1]]]
  bien_mun[filas,2]=1
  
  ##CORROBORACION CON MUNICIPIOS DE 2003
  select=which(is.na(bien_mun[,2]))
  set6A=IGeom[select,]
  G=corroboracion(set6A,mpios2003)
  filas=set6A$id[G[[1]]]
  bien_mun[filas,2]=1
  
  set8=cbind(IGeom,bien_mun[,2],sugerencia_mun)
  
  # # RURAL/URBANO ----------------------------------------------------------
  
  
  #### Revisar si los registros se encuentran en areas urbanas o rurales
  
  
  IGeor=set8
  
  coord8=cbind(IGeor$longitud,IGeor$latitud1)
  
  mr=SpatialPoints(coord8)
  
  
  en_casco=overlay(mr,casco)
  rural=rep("NA",nrow(set8)) 
  rural[which(is.na(en_casco))]=1
  
  set10=cbind(set8,rural)
  
  # #### 8. DUPLICADOS GEOGRAFICOS  -----------------------------------------
  Igeodup=set10
  
  coordinates(Igeodup)=~longitud+latitud1
  
  celda=over(Igeodup,id)
  
  select=cbind(set10$id,set10$nombre,celda)
  names(select)=c("id","nombre","celda")
  duplicados=duplicated(select[-which(select$nombre=="NA_NA"),2:3])
  unidos=as.data.frame(cbind(select[-which(select$nombre=="NA_NA"),c(1,3)],duplicados))
  unidos$duplicados[which(unidos$duplicados==0)]=NA
  names(unidos)=c("id","celda","duplicados")
  set12=merge(set10,unidos,by="id",all=T)
  
  nombres=names(set12)
  nombres[which(nombres=="bien_mun[, 2]")]="bien_muni"
  names(set12)=nombres
  
  # ### 9. EXTREMOS EN ALTURA -----------------------------------------------
  
  IGeoALT2=set12
  coordALT2=cbind(IGeoALT2$longitud,IGeoALT2$latitud1)
  coordinates(IGeoALT2)=~longitud+latitud1
  prealt=over(IGeoALT2,ALT)
  colnames(prealt)="alt"
  set12$alt=prealt 
  
  # inicia metodo para detectar outliers (modified z-score method) 
  listsp=unique(set12$nombre)
  listsp[length(listsp)+1]="NA_NA"
  #listsp=listsp[-which(listsp=="NA_NA"|is.na(listsp))]
  preset16<-NULL
  set16<-NULL
  for (w in listsp) 
  {
    DAT=subset(set12,set12$nombre==w)
    DAT$extremo=NA
    v=as.matrix(DAT$alt)[,1]  
    v=na.omit(v)
    if (length(v)!=0){
      s=summary(v)
      N=length(v)
      m=c() # inicializa vector de datos |xi - xm | donde xm es la median de altura maxima
      for (j in 1:N) { 
        pm=abs(v[j]-s[3])
        m=c(m,pm) # vector de datos |xi - xm | donde xm es la median de altura maxima
      } #end for j
      MAD=median(m, na.rm=T) # mediana de todos los datos |xi - xm | calculados
      if (MAD>0) { 
        Z=c()
        for (k in 1:N) {  
          pZ=abs(0.6745*(v[k]-s[3])/MAD) # formula para calculo de outliers segun 										   # el modified z-score method 
          if (pZ>3.5) {			   # criterio para identificar outliers 
            #luego sca esta filas 		
            Z=c(Z,k)}
        } #end for k
        if (length(Z)!=0){
          DAT$extremo[-Z]=1} # 1 no es extremo
        #preset16=DAT[Z,] 
        #preset15=DAT[-Z,]	
      }  #end if MAD
      
      preset16<-rbind(preset16,DAT) # datos sin valores extremos por altura maxima, listos para entrar al modelo
      #set15<-rbind(set15,preset15) # datos con valores extremos por altura maxima
    }
  }   #end for w
  
  
  preset16$extremo[which(is.na(preset16$alt))]="NA"
  nombres=names(preset16)
  nombres[which(nombres=="bien_mun[, 2]")]="bien_muni"; nombres[which(nombres=="nombre")]="nombre_acept"
  names(preset16)=nombres
  
  set12NA_NA=set12[(setdiff(set12$id, preset16$id)),]
  set12NA_NA$extremo="NA"
  
  names(set12NA_NA)=names(preset16)=nombres
  preset16b=rbind(preset16,set12NA_NA)
  
  ### PEGAR  INSITUTCION Y COLECTOR
  campos=set3[,c(1,17:20)]
  preset16b=merge(preset16b,campos,by="id")
  
  set16=preset16b[order(preset16b$id),]
  p16=dim(set16)[1]
  
  # ####10.  PREPARAR SETS DE DATOS PARA CORRECCION  ----------------------------
  summary(set16)
  if(VT=="YES"){
    nombre_no_valido<<-set16[which(is.na(set16$aceptadoCol)),]
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
  
  write.table(sin_coord,"sin_coord.txt",sep ="\t",row.names=F) 
  write.table(error_pais,"error_pais.txt",sep ="\t",row.names=F)
  write.table(mal_departamento,"mal_departamento.txt",sep ="\t",row.names=F)
  write.table(mal_municipio,"mal_municipio.txt",sep ="\t",row.names=F)
  write.table(casco_urbano,"casco_urbano.txt",sep ="\t",row.names=F)
  write.table(duplicados,"duplicados.txt",sep ="\t",row.names=F)
  write.table(extremos_altura,"extremos_altura.txt",sep ="\t",row.names=F)
  write.table(set16,"TABLA GENERAL.txt",sep ="\t",row.names=F)
  write.table(pasa_todo,"PERFECTOS.txt",sep ="\t",row.names=F)
  
  save.image("VERIFICACION_PAISES.RData")
  save(set16,file="TABLA_GENERAL.RData")
  
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
    
  
  return(J)
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  