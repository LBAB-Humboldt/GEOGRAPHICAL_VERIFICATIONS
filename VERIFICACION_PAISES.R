#####-------------------------------------------##########
#     CODIGO PARA LA VERIFICACION GEOGRAFICA            #
#                                                       #
#         version  reporte en tabla                     #
#####-------------------------------------------##########


###Cargar los paquetes de R necesarios para correr el script
rm(list=ls())  # Borra  todo loq ue etse en memoria
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

##  2. CARGAR LAS FUNCIONES

# Seleccionar ruta de las funciones
ruta_funciones <- dlgDir(default = getwd(), title="ESPECIFIQUE LA RUTA DONDE ESTA EL ARCHIVO SET DE FUNCIONES.R")$res

#setwd(ruta_funciones)
source(paste0(ruta_funciones,"/SET DE FUNCIONES.R"))
source("C:/IAvH/VERIFICACION PAISES VECINOS/Debug_SET DE FUNCIONES.R")

# ### 3. DEFINIR RUTAS DE TRABAJO DE LOS ARCHIVOS -------------------------
rutas()

# ### 4. CARGAR DATOS -----------------------------------------------------
### Cargar datos de informacion geografica: 
info_geografica(ruta_info_geo)
  
# #### 5. ARREGLAR TABLA --------------------------------------------------
### verificacion taxonomica ###
CARGAR_DATOS(ruta_datos)

# # ##### 6. HACER LA VERIFICACION GEOGRAFICA  -------------------------------
RESULTADOS <- VERIFICACION_PAISES(ruta_salida,set3,ALT,mundo,colombia,casco,mpios,mpios2003,mpios1993,mpios1985,mpios1973,mpios1964,id,paises)

 # ### 7. GENERAR GRAFICOS ----------------------------------------------------
graficos()

# ### 8. PARA GUARDAR LA TABLA GENERAL EN EXCEL ---------------------------
write.xlsx(set16, "TABLA_GENERAL.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=TRUE, append=FALSE)

#### METRICAS DE DESEMPEÑO
DESEMPEÑO(tabla_resumen)