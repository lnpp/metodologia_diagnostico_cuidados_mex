# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de AM_CP_Cob_Hog: 
# Calcular el indicador de cobertura de cuidados personales
# para personas adultas mayores desde el hogar.

# Fecha de actualización: 27 de diciembre 2020
# Preparado por Natalia Achicanoy, Damián Lugo, Diana Laura Ramírez,
# Rodrigo Salas, Natalia Torres.
# Contacto: Natalia Torres, correo: nataliato94@gmail.com

######################## Preparar área de trabajo ##############################

# 1. Remover notación científica
options(scipen = 999)

# 2. Carácteres usados en el idioma español 
Sys.setlocale("LC_ALL", "Spanish") #PC
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac

# 3. Eliminar variables precargadas 
rm(list = ls())

# 4. Cargar paquetes a utilizar
library(pacman)
p_load(tidyverse, readxl, dplyr, tidyr, janitor, writexl, foreign)

# 5. Establecer directorio de trabajo
setwd("C:/R/cuidados/")

########################### Cargar bases ####################################

# 1. Cargar bases
#Bases para indicador sobre casas hogares privadas
usuarios_caas <- read_excel("input/CAAS/Poblacion usuaria residente.xls")
alojamientos_caas <- read_excel("input/CAAS/Alojamientos de asistencia social.xls")
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.CSV")

#Bases para indicador sobre personas cuidadoras remuneradas
eness <- read.csv("input/ENESS/eness2017_cuestionario_basico.csv")

# 2. Limpiar nombres de variables
usuarios_caas <- clean_names(usuarios_caas)
intercensal <- clean_names(intercensal)
alojamientos_caas <- clean_names(alojamientos_caas)
eness <- clean_names(eness)

# 3. Eliminar filas sin datos
usuarios_caas <- usuarios_caas[-1, ]
alojamientos_caas <- alojamientos_caas[-1, ]

################## Seleccionar municipio y estado de interés ###################

# 1. Revisar este documento para obtener el código del estado y municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en los siguientes objetos:
estado1 <- "06"
estado2 <- 6
municipio1 <- "007"
municipio2 <- 7

# 3. Filtrar bases por estado y municipio de interés
usuarios_caas <- usuarios_caas %>% 
  filter(entidad == estado1, 
         mun == municipio1)  
alojamientos_caas <- alojamientos_caas %>% 
  filter(entidad == estado1, 
         mun == municipio1)
intercensal <- intercensal %>% 
  filter(mun == municipio2)  
eness <- eness %>%
  filter(ent == estado2)

#################### Calcular información complementaria ######################

# 1. Proporción 1 - Casas hogares privadas
  #Denominador
  poblacion_adulta_mayor <- intercensal %>%
    filter(edad>=65 & edad<=110) 
  denominador.1 <- sum(poblacion_adulta_mayor$factor)
  #Numerador
  usuarios_caas <- usuarios_caas %>% 
    subset(select = c("id_aloja", "r_t_65ym")) #Población usuario de 65 años y más
  CAAS <- left_join(alojamientos_caas, usuarios_caas, by="id_aloja") 
  CAAS <- CAAS %>%
    filter(clasealoja == 2, #Casa hogar para personas adultas mayores
           fig_juri!=5 & fig_juri!=7) #Privada
  numerador.1 <- sum(CAAS$r_t_65ym)
  #Proporción
  prop1 <- (numerador.1/denominador.1)

# 2. Proporción 2 - Personas cuidadoras remuneradas
  #Denominador
  total <- eness[eness$p1_eda %in% c(65:99),] #65 años y más
  denominador.2 <- sum(total$factor)
  #Numerador
  cuid_remun <- total[total$p29>0 & total$p29<99999,]
  numerador.2 <- sum(cuid_remun$factor, na.rm = TRUE)
  #Proporción
  prop2 <- (numerador.2/denominador.2)

# 1. Proporción 3 - Casas hogares públicas
  #Numerador
  usuarios_caas <- usuarios_caas %>% 
    subset(select = c("id_aloja", "r_t_65ym")) #Población usuario de 65 años y más
  CAAS <- left_join(alojamientos_caas, usuarios_caas, by="id_aloja") 
  CAAS <- CAAS %>%
    filter(clasealoja == 2, #Casa hogar para personas adultas mayores
           fig_juri==5) #Pública
  numerador.3 <- sum(CAAS$r_t_65ym)
  #Proporción
  prop3 <- (numerador.3/denominador.1)

# 3. Total
AM_CP_Cob_Hog_Comp <- (1-(prop1 + prop2 + prop3))*100
AM_CP_Cob_Hog_Comp

#Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
AM_CP_Cob_Hog_Comp <- as.data.frame(AM_CP_Cob_Hog_Comp) 
write_xlsx(AM_CP_Cob_Hog_Comp, "output/AM_CP_Cob_Hog.xlsx")
