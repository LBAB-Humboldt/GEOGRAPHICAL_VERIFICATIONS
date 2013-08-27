#####-------------------------------------------##########
#     CODIGO PARA LA VERIFICACION GEOGRAFICA            #
#                                                       #
#         version  reporte en tabla                     #
#####-------------------------------------------##########


###Cargar los paquetes de R necesarios para correr el script
rm(list =ls())  # Borra  todo loq ue etse en memoria
gc()
memory.limit(size = 1000000) 
# ####### 1. CARGAR LOS PAQUTES DE R NECESARIOS -------------------------------------

library(dismo)
library(maptools)
library(sp)
library(maps)
library("svDialogs")
library("xlsx")
library("xlsxjars")
library("rJava")

# ## 2. CARGAR LAS FUNCIONES --------------------------------------
ruta_funciones=(dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE ESTA EL ARCHIVO SET DE FUNCIONES.R")$res)

setwd(ruta_funciones)
source("SET DE FUNCIONES.R")


# ### 3. DEFINIR RUTAS DE TRABAJO DE LOS ARCHIVOS -------------------------

rutas()

# ### 4. CARGAR DATOS -----------------------------------------------------

### Cargar datos de informacion geografica: 
info_geografica(ruta_info_geo)
  
# #### 5. ARREGLAR TABLA --------------------------------------------------
### verificacion taxonomica ###
CARGAR_DATOS(ruta_datos)

# NOMBRES=c("id","nombre","pais", "departamento","municipio", "vereda", "localizaciongenero","especie","author","subspecies", "sub_author", "morphophysiology_description", "native_distribution_area", "natural_environment", "economic_use", "world_invasive_places", "invasion_preferential_environments", "biodiversity_impact", "economic_impact", "social_impact", "health_impact", "confirmed", "biblio_id", "abundance_id", "risk_analysis", "physical_control", "chemical_control", "control_biological", "prevention", "risk_analysis_type", "log_update", "user_update", "date_update", "user_delete", "date_delete", "user_check", "date_check", "latitude", "longitude", "user_update1", "log_update1", "date_update1", "user_delete1", "date_delete1", "user_check1", "date_check1", "specimen_number", "general_description", "invasion_description", "municipio", "state", "location", "protected_area", "coordinates_source", "es_aceptadoCoL", "id_registro_CoL", "id_familia_CoL", "id_nombre_CoL", "id_nombre_aceptado", "nombre_aceptado", "autor_nombre_aceptado", "genero_aceptado", "epiteto_aceptado", "familia_CoL", "orden_CoL", "clase_CoL", "phylum_CoL", "reino_CoL", "especie_aceptada")
# names(set3)=NOMBRES
# 
# pais=rep("CO",nrow(set0))
# set0=set3
# set3=as.data.frame(cbind(as.numeric(set0$ID), set0$especie_aceptada, pais, set0$state, set0$municipio, set0$latitude, set0$longitude, set0$date_check, set0$location), stringsAsFactors =F)
# names(set3)=c("id","nombre","pais","departamento","municipio", "longitud","latitud", "fecha_inicial", "localidad","vereda","altitud")
# set3$ID=as.numeric(set3$ID)
# set3$latitud=as.numeric(set3$latitud)
# set3$longitud=as.numeric(set3$longitud)

###### ARREGLO DATOS PARA  INVASORAS

# set0=read.delim("taxonomica_registros.txt",stringsAsFactors =F,dec = ".")
# set_taxonomica=read.delim("verificacion_taxonomica_registros.txt")
# set2=merge(set0,set_taxonomica,by="id",stringsAsFactors =F)
# 
# 
# especies=as.data.frame(as.character(set2[,24]))
# longitude=as.numeric(set2$longitude)
# latitude=as.numeric(set2$latitude)
# 
# set3=as.data.frame(cbind(set2$id,especies,pais=rep("CO",nrow(set2)),
#            Departamento=rep(NA,nrow(set2)), municipio=rep(NA,nrow(set2)),longitude,latitude,
#            fecha_inicial=rep(NA,nrow(set2)),localidad=rep(NA,nrow(set2))),stringsAsFactors =F)
# names(set3)=c("id","nombre","pais","departamento","municipio", "longitud","latitud", "fecha_inicial", "localidad")
# set3$id=as.numeric(set3$id); set3$longitud=as.numeric(set3$longitud);set3$latitud=as.numeric(set3$latitud)
# set3$nombre=as.character(set3$nombre); set3$pais=as.character(set3$pais); set3$departamento=as.character(set3$departamento);
# set3$municipio=as.character(set3$municipio); set3$pais=as.character(set3$pais); set3$departamento=as.character(set3$departamento);

# # ##### 6. HACER LA VERIFICACION GEOGRAFICA  -------------------------------

RESULTADOS=VERIFICACION_PAISES(ruta_salida,set3,ALT,mundo,colombia,casco,mpios,mpios2003,mpios1993,mpios1985,mpios1973,mpios1964,id,paises)

 # ### 7. GENERAR GRAFICOS ----------------------------------------------------

graficos()

GENERO=NULL
EPITETO=NULL
for (i in 1:nrow(set3)){
  genero=strsplit(set3$nombre[i]," ")[[1]][1]
  epiteto_especifico=strsplit(set3$nombre[i]," ")[[1]][2]
  GENERO=rbind(GENERO,genero)
  EPITETO=rbind(EPITETO,epiteto_especifico)
  
}



# ### 8. PARA GUARDAR LA TABLA GENERAL EN EXCEL ---------------------------



write.xlsx(set16[,-23], "TABLA_GENERAL.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=TRUE, append=FALSE)


#### METRICAS DE DESEMPEÑO



DESEMPEÑO(tabla_resumen)