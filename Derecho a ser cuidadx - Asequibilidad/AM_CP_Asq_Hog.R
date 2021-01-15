# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de AM_CP_Asq_Hog: 
# Calcular el indicador de asequibilidad de cuidados personales para 
# personas adultas mayores desde el hogar.

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

#################### Seleccionar estado de interés #############################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

########################### Cargar bases #######################################

# 1. Cargar bases
enigh <- read.csv("input/ENIGH/pobreza_18.csv")

# 2. Filtrarlas solo para estado de interés
enigh <- enigh %>%
  filter(ent == estado)

#################### Extraer informacion para indicador ########################

# 1. Calcular denominador - Personas adultas mayores
personas_adultas_mayores <- enigh[enigh$edad>=65 & enigh$edad<=110,]
denominador <- sum(personas_adultas_mayores$factor)

# 2. Calcular numerador
no_pobreza <- personas_adultas_mayores[personas_adultas_mayores$plb==0,]
numerador <- sum(no_pobreza$factor)

# 3. Resultado del indicador
AM_CP_Asq_Hog <- (numerador/denominador)*100
AM_CP_Asq_Hog

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
AM_CP_Asq_Hog <- as.data.frame(AM_CP_Asq_Hog)
write_xlsx(AM_CP_Asq_Hog, "output/AM_CP_Asq_Hog.xlsx")
