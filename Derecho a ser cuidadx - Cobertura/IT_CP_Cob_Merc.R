# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CP_Cob_Merc: 
# Calcular el indicador de cobertura de cuidados personales para infancia
# temprana desde el mercado.

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

############################### Cargar bases ###################################

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

############### Seleccionar estado y municipio de interés ######################

# 1. Revisar este documento para obtener el código del estado y municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en los siguientes objetos (siguiendo el formato correspondiente):
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

###################### Extraer información para indicadores ####################

# 1. Proporción 1
  #Denominador
  total <- eness[eness$p1_eda %in% c(0:5) & !is.na(eness$p23),] #0-5 años
  denominador.1 <- sum(total$factor)
  #Numerador
  cuid_remun <- total[total$p23==12 & (total$p25>0 & total$p25<99999),]
  numerador.1 <- sum(cuid_remun$factor)
  #Proporción
  prop1 <- (numerador.1/denominador.1)

# 2. Proporción 2
  #Denominador
  poblacion_primera_infancia <- intercensal %>%
  filter(edad>=0 & edad<=4) #Se utiliza 0-4 porque ese es el rango utilizado en el CAAS
  denominador.2 <- sum(poblacion_primera_infancia$factor)
  #Numerador
  usuarios_caas <- usuarios_caas %>% 
    subset(select = c("id_aloja", "r_t_0_4")) #Población usuario de 0-4 años
  CAAS <- left_join(alojamientos_caas, usuarios_caas, by="id_aloja") 
  CAAS <- CAAS %>%
    filter(clasealoja == 1, #Casa hogar para menores de edad
           fig_juri!=5 & fig_juri!=7) #Privada
  numerador.2 <- sum(CAAS$r_t_0_4)
  #Proporción
  prop2 <- (numerador.2/denominador.2)

# 3. Total
  IT_CP_Cob_Merc <- (prop1 + prop2)*100
  IT_CP_Cob_Merc

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
  IT_CP_Cob_Merc <- as.data.frame(IT_CP_Cob_Merc) 
  write_xlsx(IT_CP_Cob_Merc, "output/IT_CP_Cob_Merc.xlsx")