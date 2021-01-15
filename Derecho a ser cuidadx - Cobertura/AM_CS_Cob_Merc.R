# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de AM_CS_Cob_Merc: 
# Calcular el indicador de cobertura de cuidados de salud
# para personas adultas mayores desde el mercado.

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

#1. Cargar base
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.CSV")

#2. Limpiar nombres variables
intercensal <- clean_names(intercensal)

#################### Seleccionar municipio de interés ##########################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 
# Nótese que no es necesario seleccionar el estado porque la base descarga ya
# corresponde al estado de interés para el análisis. 

# 2. Sustituir código en el siguiente objeto
municipio <- 7

# 3. Filtrar base por municipio de interés
intercensal <- intercensal %>% 
  filter(mun == municipio)  

##################### Extraer información para indicador #######################

# 1. Calcular denominador
personas_adultas_mayores <- intercensal %>%
  filter(edad>=65 & edad<=110)
denominador <- sum(personas_adultas_mayores$factor, na.rm = TRUE)

# 2. Calcular numerador
# Número total con seguro médico privado en primera o segunda opción de respuesta
seg_priv <- personas_adultas_mayores[personas_adultas_mayores$dhsersal1==6 | personas_adultas_mayores$dhsersal2==6,]
numerador <- sum(seg_priv$factor, na.rm = TRUE)

# 3. Resultado del indicador 
AM_CS_Cob_Merc <- (numerador/denominador)*100
AM_CS_Cob_Merc

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
AM_CS_Cob_Merc <- as.data.frame(AM_CS_Cob_Merc) 
write_xlsx(AM_CS_Cob_Merc, "output/AM_CS_Cob_Merc.xlsx")
