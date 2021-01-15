# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de AM_CDI_Acc_Hog: 
# Calcular el indicador de accesibilidad de cuidados para el desarrollo integral
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

##################### Seleccionar municipio de interés #########################

# 1. Revisar este documento para obtener el código del municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 
# Nótese que no es necesario seleccionar el estado porque la base descarga ya
# corresponde al estado de interés para el análisis.

# 2. Sustituir código en el siguiente objeto
municipio <- 7

############################### Cargar bases ####################################

# 1. Cargar bases
datos1 <- read.csv("input/Intercensal/TR_PERSONA06.csv")
datos2 <- read.csv("input/Intercensal/TR_VIVIENDA06.csv")

# 2. Unir bases
intercensal <- left_join(datos2, datos1)

# 3. Filtrarlas solo para municipio de interés
inter_per_mun <- intercensal %>%
  filter(MUN==municipio)

###################### Extraer información para indicadores ####################

# 1. Calcular denominador
inter_per_mun_mayor <- inter_per_mun[inter_per_mun$EDAD >= 65 & inter_per_mun$EDAD != 999,]
denominador <- sum(inter_per_mun_mayor$FACTOR, na.rm = TRUE)

# 2. Calcular numerador
inter_per_mun_mayor <- inter_per_mun_mayor %>%
  mutate(pers_viv = NUMPERS-1) #quitar a la persona adulta mayor del cálculo
numerador <- sum(inter_per_mun_mayor$pers_viv*inter_per_mun_mayor$FACTOR, na.rm = TRUE)

# 3.Resultado del indicador 
AM_CDI_Acc_Hog <- (numerador/denominador)
AM_CDI_Acc_Hog

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
AM_CDI_Acc_Hog <- as.data.frame(AM_CDI_Acc_Hog)
write_xlsx(AM_CDI_Acc_Hog, "output/AM_CDI_Acc_Hog.xlsx")

