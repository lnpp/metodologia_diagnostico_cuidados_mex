# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CS_Acc_Merc: 
# Calcular el indicador de accesibilidad de cuidados de salud
# para infancia temprana desde el mercado.

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

#1. Cargar base
intercensal <- read.csv("input/Intercensal/TR_PERSONA06.CSV")

#2. Limpiar nombres variables
intercensal <- clean_names(intercensal)

################## Seleccionar municipio de interés ############################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 
# Nótese que no es necesario seleccionar el estado porque la base descarga ya
# corresponde al estado de interés para el análisis.

# 2. Sustituir código en el siguiente objeto
municipio <- 7

# 3. Filtrar base por municipio de interés
intercensal <- intercensal %>% 
  filter(mun == municipio)  

###################### Extraer información para indicador ·#####################

#1. Calcular denominador
primera_infancia <- intercensal %>%
  filter(edad>=0 & edad<=5)
#Número total con seguro médico privado en primera o segunda opción de respuesta
seg_priv <- primera_infancia[primera_infancia$dhsersal1==6 | primera_infancia$dhsersal2==6,]
denominador.1 <- sum(seg_priv$factor, na.rm = TRUE)
  
#2. Calcular numerador
# Número total que se atiende en un consultorio, clínica u hospital privado cuando tiene problemas de salud
# y que tiene cobertura de seguro privado
usa_seg_priv <- seg_priv[seg_priv$sersalud==6,]
numerador.1 <- sum(usa_seg_priv$factor, na.rm = TRUE)
  
#3. Resultado indicador
IT_CS_Acc_Merc <- (numerador.1/denominador.1)*100
IT_CS_Acc_Merc
  
###################### Calcular información complementaria #####################
  
#1. Calcular denominador
denominador.2 <- sum(primera_infancia$factor, na.rm = TRUE)
  
#2. Calcular numerador
usa_seg_priv.2 <- primera_infancia[primera_infancia$sersalud==6 | primera_infancia$sersalud==7,]
numerador.2 <- sum(usa_seg_priv.2$factor, naa.rm = TRUE)

#3. Resultado indicador
IT_CS_Acc_Merc_Comp <- (numerador.2/denominador.2)*100
IT_CS_Acc_Merc_Comp

#4. Guardar ambos resultados en la carpeta de output como archivo .xlsx de Excel
IT_CS_Acc_Merc <- as.data.frame(IT_CS_Acc_Merc)
IT_CS_Acc_Merc_Comp <- as.data.frame(IT_CS_Acc_Merc_Comp)
write_xlsx(list(IT_CS_Acc_Merc,IT_CS_Acc_Merc_Comp), "output/IT_CS_Acc_Merc.xlsx")

