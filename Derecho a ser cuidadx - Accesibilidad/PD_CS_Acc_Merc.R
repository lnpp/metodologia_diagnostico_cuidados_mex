# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de PD_CS_Acc_Merc: 
# Calcular el indicador de accesibilidad de cuidados de salud
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

########################### Cargar bases #######################################

# 1. Cargar base
censo_cuestamp <- read.dta("input/Censo/personas_06.dta")
# Nota: cambiar el archivo por el del estado de interés.

# 2. Limpiar nombres variables
censo_cuestamp <- clean_names(censo_cuestamp)

############### Seleccionar municipio y estado de interés ######################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 
# Nótese que no es necesario seleccionar el estado porque la base descarga ya
# corresponde al estado de interés para el análisis.

# 2. Sustituir código en el siguiente objeto
municipio <- "007"

# 3. Filtrar base por municipio de interés
censo_cuestamp <- censo_cuestamp %>%
  filter(mun == municipio)

####################### Extraer información para indicador #####################

#1. Calcular denominador
personas_discap <- censo_cuestamp[is.na(censo_cuestamp$discap8),]
#Número total con seguro médico privado en primera o segunda opción de respuesta
seg_priv <- personas_discap[personas_discap$dhsersal1==6 | personas_discap$dhsersal2==6,]
denominador.1 <- sum(seg_priv$factor, na.rm = TRUE)

#2. Calcular numerador
# Número total que se atiende en un consultorio, clínica u hospital privado cuando tiene problemas de salud
# y que tiene cobertura de seguro privado
usa_seg_priv <- seg_priv[seg_priv$sersalud==7,]
numerador.1 <- sum(usa_seg_priv$factor, na.rm = TRUE)

#3. Resultado indicador
PD_CS_Acc_Merc <- (numerador.1/denominador.1)*100
PD_CS_Acc_Merc

####################### Calcular información complementaria ####################

#1. Calcular denominador
denominador.2 <- sum(personas_discap$factor, na.rm = TRUE)

#2. Calcular numerador
usa_seg_priv.2 <- personas_discap[personas_discap$sersalud==7,]
numerador.2 <- sum(usa_seg_priv.2$factor, naa.rm = TRUE)

#3. Resultado indicador
PD_CS_Acc_Merc_Comp <- (numerador.2/denominador.2)*100
PD_CS_Acc_Merc_Comp

#Guardar los resultados de ambos indicadores en la carpeta de output como archivo .xlsx de Excel
PD_CS_Acc_Merc <- as.data.frame(PD_CS_Acc_Merc)
PD_CS_Acc_Merc_Comp <- as.data.frame(PD_CS_Acc_Merc_Comp)
write_xlsx(list(PD_CS_Acc_Merc, PD_CS_Acc_Merc_Comp), "output/PD_CS_Acc_Merc.xlsx")

