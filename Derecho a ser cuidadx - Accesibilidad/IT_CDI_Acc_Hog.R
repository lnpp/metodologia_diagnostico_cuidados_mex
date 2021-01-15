# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CDI_Acc_Hog: 
# Calcular el indicador de accesibilidad de cuidados para el desarrollo integral
# para infancia temprana desde el hogar.

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

##################### Seleccionar estado de interés ############################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

############################## Cargar bases ####################################

# 1. Cargar bases
datos <- read.csv("input/ENESS/eness2017_cuestionario_basico.csv")

# 2. Filtrarlas solo para estado de interés
datos_estado <- filter(datos, ENT == estado)

##################### Extraer información para indicador #######################

# 1. Cálculo del indicador
niños <- datos_estado[datos_estado$P1_EDA %in% c(0:5) & !is.na(datos_estado$P23),] #0-5 años
p23_hogar_adultos <- niños[niños$P23 %in% c(0, 09, 10),] #Cuidado por su mamá, su papá, su abuelo/a

# 2. Resultado del indicador
IT_CDI_Acc_Hog <- sum(p23_hogar_adultos$FACTOR)/sum(niños$FACTOR)*100
IT_CDI_Acc_Hog

# 3. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
IT_CDI_Acc_Hog <- as.data.frame(IT_CDI_Acc_Hog) 
write_xlsx(IT_CDI_Acc_Hog, "output/IT_CDI_Acc_Hog.xlsx")
