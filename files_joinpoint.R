################# Librerías
library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

################ Carga de datos
if (Sys.info()[['sysname']] == "Darwin"){
  bd <- read_delim("/Users/alvaro/Documents/Data_Science/R/proyecto_grado/cr5_18052022.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
} else {
  bd <- read_delim("C:/Users/Usuario/Documents/R/proyecto de grado/cr5_18052022.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE) 
}


############# Selección tipo de cancer
cancer <- readline(prompt="Ingresar Cáncer a analizar: ")

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
} 
else if (cancer == "pulmon") {
  bd_pulmon <- bd %>% 
    filter(C10=="C340" | C10=="C341" | C10=="C342" | C10=="C343" | C10=="C348" 
           | C10=="C349") %>% 
    select(TUMOURID, NOCASO, FECNAC, EDAD, SEXO, REGCOM, FECDIAG,
           TOP, MORF, COMP, GRA, EXT, LAT, BASE, C10, PMSEC, PMTOT, FUE1, RUT)
  
  bd_pulmon <- bd_pulmon %>% mutate(MORF_GRUPO = case_when(
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
} 
else if (cancer == "piel") {
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

################  Edad grupo
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


################  Año y mes diagnóstico
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
##############################################################################
##############################################################################
#######################  Región de Antofagasta
##############################################################################
##############################################################################
##############################################################################
by_age_year <- bd_ %>% group_by(YEAR_DIAG, AGE_GROUP) %>% count()
by_age_year$sex = 0



tiempo_estudio = c(2003, 2015)
resta = tiempo_estudio[2] - tiempo_estudio[1]
final = tiempo_estudio[1] + resta
vector = 2003:final
data_agrupada = data.frame()
for (i in vector) {
  vector_longitud = rep(i, 17)
  if (nrow(subset(by_age_year, by_age_year$YEAR_DIAG == i)) < 17)
    {
    grupo_edad = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
    dataframe = subset(by_age_year, by_age_year$YEAR_DIAG == i)
    #faltantes_ = grupo_edad[!grupo_edad %in% dataframe[2]]
    for(a in dataframe[2])
      faltantes = grupo_edad[!grupo_edad %in% a]
      data = data.frame(YEAR_DIAG = i, 
                        AGE_GROUP =  faltantes,
                        n = 0) }
  dataframe_ <- rbind(data, dataframe)
  data_agrupada <- rbind(data_frame_, data_agrupada)
}

data_agrupada


typeof(dataframe_)




##### Agregar la poblacion estándar
by_age_year = by_age_year %>% mutate(POB_STANDAR = case_when(
  AGE_GROUP == 1 ~ 12000,
  AGE_GROUP == 2 ~ 10000,
  AGE_GROUP == 3 ~ 9000,
  AGE_GROUP == 4 ~ 9000,
  AGE_GROUP == 5 ~ 8000,
  AGE_GROUP == 6 ~ 8000,
  AGE_GROUP == 7 ~ 6000,
  AGE_GROUP == 8 ~ 6000,
  AGE_GROUP == 9 ~ 6000,
  AGE_GROUP == 10 ~ 6000,
  AGE_GROUP == 11 ~ 5000,
  AGE_GROUP == 12 ~ 4000,
  AGE_GROUP == 13 ~ 4000,
  AGE_GROUP == 14 ~ 3000,
  AGE_GROUP == 15 ~ 2000,
  AGE_GROUP == 16 ~ 1000,
  AGE_GROUP == 17 ~ 1000
))

############## CARGAR POBLACION INE
if (Sys.info()[['sysname']] == "Darwin"){
  #poblacion_ine <- read_delim("/Users/alvaro/Documents/Data_Science/R/proyecto_grado/cr5_18052022.txt", 
  #delim = "\t", escape_double = FALSE, 
  #trim_ws = TRUE)
} else {
  poblacion_ine <- read_excel("D:/Datasets/poblacion/pob_ine_censo2017_proyecciones.xlsx")
}


### AGE GROUP EN LA POBLACION INE
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


### SUBSET POBLACION ANTOFAGASTA
poblacion <- as.data.frame(subset(poblacion_ine, Region == 2))

### GROUPBY INE ANTOFAGASTA
poblacion_edad <- as.data.frame(poblacion_antofagasta) %>% group_by(AGE_GROUP) %>% 
  #(`Poblacion 2003`)
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
            pob_2015 = sum(`Poblacion 2015`),
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

data_jp <- merge(by_age_year, data_poblacion, by = c('AGE_GROUP', 'YEAR_DIAG'))
data_jp <- data_jp %>% arrange(YEAR_DIAG, AGE_GROUP)

###Variable para jointpoint
## Count
## Population
## age recode with < 1 year olds no unknowns
## Std Population
## Year of diagnosis



