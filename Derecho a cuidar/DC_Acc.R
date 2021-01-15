# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de DC_Acc: 
# Calcular indicador del atributo de accesibilidad del derecho a cuidar

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

############################# Cargar bases #####################################

# 1. Cargar base
sdem <- read.csv("input/ENOE/SDEMT120.csv")

# Corregir nombre variables - este paso solo es necesario cuando la computadora importa la primera columna con el prefijo "ï.."
sdem <- sdem %>%
  rename(r_def = ï..r_def)

# 2. Filtrar solo entrevistas realizadas 
sdem <- sdem %>%
  filter(r_def == 0 & c_res != 2 & eda >=15 & eda != 99)

# 3. Filtrar solo para el estado de interés
sdem <- sdem %>% 
  filter(ent == estado)

########################### Calcular indicadores ###############################

# 1. Generar bases por género para PNEA
mujeres <- sdem %>%
  filter(sex==2 & clase1==2) %>%
  filter(eda>=15 & eda<=64)
hombres <- sdem %>%
  filter(sex==1 & clase1==2) %>%
  filter(eda>=15 & eda<=64)

# 2. Calcular proporción que pertenece a la PNEA por dedicarse a los quehaceres domésticos

#Mujeres
pnea_trabdom_m <- (weighted.mean(mujeres$c_inac5c==2, mujeres$fac, na.rm = TRUE))*100

#Hombres
pnea_trabdom_h <- (weighted.mean(hombres$c_inac5c==2, hombres$fac, na.rm = TRUE))*100

# 3. Diferencia (indicador)
DC_Acc <- pnea_trabdom_m - pnea_trabdom_h
DC_Acc

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
DC_Acc <- as.data.frame(DC_Acc)
write_xlsx(DC_Acc, "output/DC_Acc.xlsx")

