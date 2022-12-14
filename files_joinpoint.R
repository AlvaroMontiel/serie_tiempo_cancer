################# Librerías
cancer <- readline(prompt="Ingresar Cáncer a analizar: ")

library(readr)
library(dplyr)
library(tidyverse)
library(readxl)
#library(xlsx)

################################################################################
################################ Carga de datos ################################
################################################################################
####                      Cargar datos de Incidencia
if (Sys.info()[['sysname']] == "Darwin"){
  bd <- read_delim("/Users/alvaro/Documents/Data_Science/R/proyecto_grado/cr5_18052022.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
} else {
  bd <- read_delim("C:/Users/Usuario/Documents/R/proyecto de grado/cr5_18052022.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) 
}

####                     Cargar datos de Poblacion INE
if (Sys.info()[['sysname']] == "Darwin"){
  poblacion_ine <- read_excel("/Users/alvaro/Desktop/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")
} else {
  poblacion_ine <- read_excel("D:/Datasets/poblacion/pob_ine_censo2017_proyecciones.xlsx")
}

####                    Cargar datos de Poblacion Estandar
pob_estandar = data.frame(AGE_GROUP = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                          POB_STANDAR = c(12000,10000,9000,9000,
                                          8000,8000,6000,6000,
                                          6000,6000,5000,4000,
                                          4000,3000,2000,1000,
                                          1000))

################################################################################
############################# Define funciones #################################
################################################################################

###### completar grupos de edad
completa_grupo_edad <- function(periodo, datos_edad_grupo){
  vector_tiempo <- periodo
  resta <- vector_tiempo[2] - vector_tiempo[1]
  final = vector_tiempo[1] + resta
  vector = 2003:final
  data_agrupada = data.frame()
  for (i in vector) {
    vector_longitud = rep(i, 17)
    if (nrow(subset(datos_edad_grupo, datos_edad_grupo$YEAR_DIAG == i)) < 17)
    {
      grupo_edad = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
      data_frame = subset(datos_edad_grupo, datos_edad_grupo$YEAR_DIAG == i) }
    faltantes_ = grupo_edad[!grupo_edad %in% data_frame$AGE_GROUP]
    data = data.frame(YEAR_DIAG = i, 
                      AGE_GROUP =  faltantes_,
                      n = 0) 
    data_frame <- rbind(data, data_frame)
    data_agrupada <- rbind(data_frame, data_agrupada)
  }
  return (data_agrupada)
}

### agrupar poblacion por grupo de edad y por año
datos_poblacion <- function(poblacion){
  poblacion_edad <- as.data.frame(poblacion) %>% group_by(AGE_GROUP) %>% 
    summarise(pob_2003 = sum(`Poblacion 2003`),
              pob_2004 = sum(`Poblacion 2004`),
              pob_2005 = sum(`Poblacion 2005`),
              pob_2006 = sum(`Poblacion 2006`),
              pob_2007 = sum(`Poblacion 2007`),
              pob_2008 = sum(`Poblacion 2008`),
              pob_2009 = sum(`Poblacion 2009`),
              pob_2010 = sum(`Poblacion 2010`),
              pob_2011 = sum(`Poblacion 2011`),
              pob_2012 = sum(`Poblacion 2012`),
              pob_2013 = sum(`Poblacion 2013`),
              pob_2014 = sum(`Poblacion 2014`),
              pob_2015 = sum(`Poblacion 2015`)
    )
  posicion = 2
  year = 2003
  data_poblacion = data.frame()
  for (i in colnames(poblacion_edad[2:14])) {
    dataframe <- poblacion_edad[posicion]
    dataframe$YEAR_DIAG <- year
    dataframe$AGE_GROUP <- poblacion_edad$AGE_GROUP
    dataframe = rename(dataframe, 'POB' = i)
    posicion = posicion + 1
    year = year + 1
    data_poblacion <- rbind(data_poblacion, dataframe)
  }  
  return(data_poblacion)
}

#### CREAR EL MARCO DE DATOS PARA EL PROGRAMA DE JOINTPOINT
data_joinpoint <- function(datos_incidencia, periodo, poblacion){
  bd_ = datos_incidencia
  #### Ambos sexos
  bd_agrupada <- bd_ %>% group_by(YEAR_DIAG, AGE_GROUP) %>% count()
  by_age_year <- completa_grupo_edad(periodo, bd_agrupada)
  by_age_year$sex = 0
  by_age_year <- merge(by_age_year, pob_estandar) %>% arrange(YEAR_DIAG, AGE_GROUP)
  poblacion_agrupada <- datos_poblacion(poblacion)
  data_jp <- merge(by_age_year, poblacion_agrupada, by = c('AGE_GROUP', 'YEAR_DIAG')) %>% 
    arrange(YEAR_DIAG, AGE_GROUP)
  ### Hombres
  bd_hombres <- subset(bd_, SEXO == 1)
  bd_agrupada_h <- bd_hombres %>% group_by(YEAR_DIAG, AGE_GROUP) %>% count()
  by_age_year_h <- completa_grupo_edad(periodo, bd_agrupada_h)
  by_age_year_h$sex = 1
  by_age_year_h <- merge(by_age_year_h, pob_estandar) %>% arrange(YEAR_DIAG, AGE_GROUP)
  poblacion_hombres <- subset(poblacion, Sexo == 1)
  poblacion_agrupada_h <- datos_poblacion(poblacion_hombres)
  data_jp_hombres <- merge(by_age_year_h, poblacion_agrupada_h, by = c('AGE_GROUP', 'YEAR_DIAG')) %>% 
    arrange(YEAR_DIAG, AGE_GROUP)
  ### Mujeres
  bd_mujeres <- subset(bd_, SEXO == 2)
  bd_agrupada_m <- bd_mujeres %>% group_by(YEAR_DIAG, AGE_GROUP) %>% count()
  by_age_year_m <- completa_grupo_edad(periodo, bd_agrupada_m)
  by_age_year_m$sex = 2
  by_age_year_m <- merge(by_age_year_m, pob_estandar) %>% arrange(YEAR_DIAG, AGE_GROUP)
  poblacion_mujeres <- subset(poblacion, Sexo == 2)
  poblacion_agrupada_m <- datos_poblacion(poblacion_mujeres)
  data_jp_mujeres <- merge(by_age_year_m, poblacion_agrupada_m, by = c('AGE_GROUP', 'YEAR_DIAG')) %>% 
    arrange(YEAR_DIAG, AGE_GROUP)
  data_joinpoint_total <- rbind(data_jp, data_jp_hombres, data_jp_mujeres)
  return(data_joinpoint_total)
}

##############################################################################
########################## Selección tipo de cancer ##########################
##############################################################################
if (cancer == "vejiga"){
  bd_ <- bd %>% 
    filter(C10=="C670" | C10=="C671" | C10=="C672" | C10=="C674" | C10=="C675" | 
             C10=="C676" | C10=="C677" | C10=="C678" | C10=="C679") %>%
    select(TUMOURID, NOCASO, FECNAC, EDAD, SEXO, REGCOM, FECDIAG,
           TOP, MORF, COMP, GRA, EXT, LAT, BASE, C10, PMSEC, PMTOT, FUE1, RUT)
  
  bd_ <- bd_ %>% mutate(MORF_GRUPO = case_when(
    MORF == 8000 | MORF == 8001 | MORF == 8002 ~ 1,
    MORF == 8120 | MORF == 8121 | MORF == 8130 ~ 2,
    MORF == 8010 | MORF == 8012 | MORF == 8013 | MORF == 8021 | MORF == 8032 |
      MORF == 8033 | MORF == 8041 | MORF == 8042 | MORF == 8043 | MORF == 8044 |
      MORF == 8045 | MORF == 8046 | MORF == 8050 | MORF == 8070 | MORF == 8071 | 
      MORF == 8072 | MORF == 8073 | MORF == 8075 | MORF == 8076 | MORF == 8140 | 
      MORF == 8210 | MORF == 8211 | MORF == 8230 | MORF == 8240 | MORF == 8246 | 
      MORF == 8250 | MORF == 8252 | MORF == 8260 | MORF == 8263 | MORF == 8310 | 
      MORF == 8323 | MORF == 8430 | MORF == 8480 | MORF == 8481 | MORF == 8490 | 
      MORF == 8550 | MORF == 8801 | MORF == 8890 | MORF == 8980  ~ 3)
  )
} else if (cancer == "pulmon") {
  bd_ <- bd %>% 
    filter(C10=="C340" | C10=="C341" | C10=="C342" | C10=="C343" | C10=="C348" 
           | C10=="C349") %>% 
    select(TUMOURID, NOCASO, FECNAC, EDAD, SEXO, REGCOM, FECDIAG,
           TOP, MORF, COMP, GRA, EXT, LAT, BASE, C10, PMSEC, PMTOT, FUE1, RUT)
  
  bd_ <- bd_ %>% mutate(MORF_GRUPO = case_when(
    MORF == 8000 | MORF == 8001 | MORF == 8002 ~ 1,
    MORF == 8010 | MORF == 8012 | MORF == 8013 | MORF == 8021 | MORF == 8032 |
      MORF == 8033 | MORF == 8041 | MORF == 8042 | MORF == 8043 | MORF == 8044 |
      MORF == 8045 | MORF == 8046 ~ 2,
    MORF == 8050 | MORF == 8070 | MORF == 8071 | MORF == 8072 | MORF == 8073 | 
      MORF == 8075 | MORF == 8076 ~ 3,
    MORF == 8140 | MORF == 8211 | MORF == 8230 | MORF == 8240 | MORF == 8246 | 
      MORF == 8250 | MORF == 8252 | MORF == 8260 | MORF == 8263 | MORF == 8310 | 
      MORF == 8323 ~ 4,
    MORF == 8430 | MORF == 8480 | MORF == 8481 | MORF == 8490 | MORF == 8550 | 
      MORF == 8800 | MORF == 8990 | MORF == 9050 | MORF == 8090 ~ 5)
  )
} else if (cancer == "piel") {
  bd_ <- bd %>% 
    filter(C10=="C440" | C10=="C441" | C10=="C442" | C10=="C443" | C10=="C444" |
             C10=="C445" | C10=="C446" | C10=="C447" | C10=="C448" | 
             C10=="C449") %>%
    select(TUMOURID, NOCASO, FECNAC, EDAD, SEXO, REGCOM, FECDIAG,
           TOP, MORF, COMP, GRA, EXT, LAT, BASE, C10, PMSEC, PMTOT, FUE1, RUT)
  
  bd_ <- bd_ %>% mutate(MORF_GRUPO = case_when(
    MORF == 8050 | MORF == 8051 | MORF == 8070 | MORF == 8071 | MORF == 8072 | 
      MORF == 8074 | MORF == 8075 | MORF == 8076 | MORF == 8082 | MORF == 8083 | 
      MORF == 8084 ~ 1,
    MORF == 8090 | MORF == 8091 | MORF == 8091 | MORF == 8092 | MORF == 8093 |
      MORF == 8094 | MORF == 8095 | MORF == 8097 | MORF == 8098 | MORF == 8102 ~ 2,
    MORF == 8000 | MORF == 8004 | MORF == 8010 | MORF == 8011 | MORF == 8030 |
      MORF == 8140 | MORF == 8230 | MORF == 8246 | MORF == 8247 | MORF == 8310 | 
      MORF == 8401 | MORF == 8410 | MORF == 8430 | MORF == 8480 | MORF == 8560 | 
      MORF == 8574 | MORF == 8832 | MORF == 8833 | MORF == 8980 ~ 3)
  )
}

##############################################################################
############################# CREAR EDADES AGRUPADAS #########################
##############################################################################

#### En dataset incidencia
bd_ <- bd_ %>% mutate(AGE_GROUP = case_when(
  EDAD >= 0 & EDAD <= 4 ~ 1,
  EDAD >= 5 & EDAD <= 9 ~ 2,
  EDAD >= 10 & EDAD <= 14 ~ 3,
  EDAD >= 15 & EDAD <= 19 ~ 4,
  EDAD >= 20 & EDAD <= 24 ~ 5,
  EDAD >= 25 & EDAD <= 29 ~ 6,
  EDAD >= 30 & EDAD <= 34 ~ 7,
  EDAD >= 35 & EDAD <= 39 ~ 8,
  EDAD >= 40 & EDAD <= 44 ~ 9,
  EDAD >= 45 & EDAD <= 49 ~ 10,
  EDAD >= 50 & EDAD <= 54 ~ 11,
  EDAD >= 55 & EDAD <= 59 ~ 12,
  EDAD >= 60 & EDAD <= 64 ~ 13,
  EDAD >= 65 & EDAD <= 69 ~ 14,
  EDAD >= 70 & EDAD <= 74 ~ 15,
  EDAD >= 75 & EDAD <= 79 ~ 16,
  EDAD >= 80  ~ 17
))

### en sataset poblacion ine
poblacion_ine <- poblacion_ine %>% mutate(AGE_GROUP = case_when(
  Edad >= 0 & Edad <= 4 ~ 1,
  Edad >= 5 & Edad <= 9 ~ 2,
  Edad >= 10 & Edad <= 14 ~ 3,
  Edad >= 15 & Edad <= 19 ~ 4,
  Edad >= 20 & Edad <= 24 ~ 5,
  Edad >= 25 & Edad <= 29 ~ 6,
  Edad >= 30 & Edad <= 34 ~ 7,
  Edad >= 35 & Edad <= 39 ~ 8,
  Edad >= 40 & Edad <= 44 ~ 9,
  Edad >= 45 & Edad <= 49 ~ 10,
  Edad >= 50 & Edad <= 54 ~ 11,
  Edad >= 55 & Edad <= 59 ~ 12,
  Edad >= 60 & Edad <= 64 ~ 13,
  Edad >= 65 & Edad <= 69 ~ 14,
  Edad >= 70 & Edad <= 74 ~ 15,
  Edad >= 75 & Edad <= 79 ~ 16,
  Edad >= 80  ~ 17
))

##############################################################################
#############################  Año y mes diagnóstico ##########################
##############################################################################
year <- ""
mes <- ""
contador <- 1   
for(i in bd_$FECDIAG){
  year[[contador]] <- as.integer(str_sub(i, 1, 4))
  mes[[contador]] <- as.integer(str_sub(i,5,6))
  contador <- contador +  1
}  

bd_$YEAR_DIAG <- as.integer(year)
bd_$MES_DIAG <- as.integer(mes)

##############################################################################
####################### DATOS JP REGION DE ANTOFAGASTA #######################
##############################################################################
tiempo_estudio = c(2003, 2015)

### SUBSETS 
##### POBLACION ANTOFAGASTA
poblacion <- as.data.frame(subset(poblacion_ine, Region == 2))

joinpoint_reg_afta <- data_joinpoint(datos_incidencia = bd_, 
                                    periodo = tiempo_estudio, 
                                    poblacion = poblacion)

##############################################################################
####################### DATOS JP COMUNA DE ANTOFAGASTA #######################
##############################################################################
### SUBSETS 
##### POBLACION COMUNA ANTOFAGASTA
poblacion <- as.data.frame(subset(poblacion_ine, Comuna == 2101))

#### INCIDENCIA COMUNA ANTOFAGASTA
bd_comafta = subset(bd_, REGCOM = "02101")
joinpoint_com_afta <- data_joinpoint(datos_incidencia = bd_comafta,
                                     periodo = tiempo_estudio, 
                                     poblacion = poblacion)

##############################################################################
####################### DATOS JP COMUNA DE CALAMA ############################
##############################################################################
### SUBSETS 
##### POBLACION COMUNA CALAMA
poblacion <- as.data.frame(subset(poblacion_ine, Comuna == 2201))

#### INCIDENCIA COMUNA CALAMA
bd_comcalama = subset(bd_, REGCOM = "02201")
joinpoint_com_calama <- data_joinpoint(datos_incidencia = bd_comcalama,
                                     periodo = tiempo_estudio, 
                                     poblacion = poblacion)

##############################################################################
####################### DATOS JP COMUNA DE TOCOPILLA #########################
##############################################################################
### SUBSETS 
##### POBLACION COMUNA TOCOPILLA
poblacion <- as.data.frame(subset(poblacion_ine, Comuna == 2301))

#### INCIDENCIA COMUNA TOCOPILLA
bd_comtocopilla = subset(bd_, REGCOM = "02301")
joinpoint_com_tocopilla <- data_joinpoint(datos_incidencia = bd_comtocopilla,
                                     periodo = tiempo_estudio, 
                                     poblacion = poblacion)

##############################################################################
################################ DATOS A CSV ###############################
##############################################################################

if(cancer == "pulmon") {
write_csv(joinpoint_reg_afta, 
          file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_reg_afta_pulmon.csv")
write_csv(joinpoint_com_afta, 
          file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_afta_pulmon.csv")
write_csv(joinpoint_com_calama, 
          file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_calama_pulmon.csv")
write_csv(joinpoint_com_tocopilla, 
          file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_tocopilla_pulmon.csv")

} else if (cancer == "piel") {
  write_csv(joinpoint_reg_afta, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_reg_afta_piel.csv")
  write_csv(joinpoint_com_afta, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_afta_piel.csv")
  write_csv(joinpoint_com_calama, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_calama_piel.csv")
  write_csv(joinpoint_com_tocopilla, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_tocopilla_piel.csv")
} else if (cancer == "vejiga") {
  write_csv(joinpoint_reg_afta, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_reg_afta_vejiga.csv")
  write_csv(joinpoint_com_afta, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_afta_vejiga.csv")
  write_csv(joinpoint_com_calama, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_calama_vejiga.csv")
  write_csv(joinpoint_com_tocopilla, 
            file = "/Users/alvaro/Documents/Data_Science/R/proyecto_grado/jointpoint/jp_com_tocopilla_vejiga.csv")
}


