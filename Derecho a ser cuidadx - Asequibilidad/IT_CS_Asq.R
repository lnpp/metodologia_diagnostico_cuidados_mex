# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósitos de IT_CS_Asq: 
# Calcular el indicador de asequibilidad de cuidados de salud
# para la primera infancia, desde todos los proveedores.

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

######################### Seleccionar estado de interés ########################

# 1. Revisar este documento para obtener el código del estado de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en el siguiente objeto
estado <- 6

############################## Cargar bases ####################################

# 1. Cargar bases
hogar <- read.csv("input/ENIGH/concentradohogar.csv")
personas <- read.csv("input/ENIGH//poblacion.csv")

# 2. Limpiar nombres variables
hogar <- clean_names(hogar)
personas <- clean_names(personas)

# Corregir nombre variables - este paso solo es necesario cuando la computadora 
# importa la variable "folioviv" con el prefijo "i_"
hogar <- rename(hogar, "folioviv" = i_folioviv)
personas <- rename(personas, "folioviv" = i_folioviv)

# 3. Unir bases
enigh <- left_join(personas, hogar, by=c("folioviv", "foliohog"))

# 4. Filtrarlas solo para estado de interés
enigh$ent <- str_sub(enigh$ubica_geo,end = -4)

enigh <- enigh %>%
  filter(ent == estado)

##################### Extraer información para indicador #######################

# 1. Calcular denominador
infancia_temprana <- enigh[enigh$edad>=0 & enigh$edad<=5,]
denominador <- sum(infancia_temprana$factor, na.rm = TRUE)
denominador

# 2. Calcular numerador
  #Gastos en salud como porcentaje de ingresos
  infancia_temprana$prop_gasto_salud <- (infancia_temprana$salud/infancia_temprana$ing_cor)
  #Gastan 10 por ciento o menos
  no_onerosos <- infancia_temprana[infancia_temprana$prop_gasto_salud<=0.10,]
  numerador <- sum(no_onerosos$factor, na.rm = TRUE)
  numerador
  
# 3. Resultado del indicador 
IT_CS_Asq <- (numerador/denominador)*100
IT_CS_Asq

# 4. Guardar el resultado del indicador en la carpeta de output (como archivo .xlsx de Excel)
IT_CS_Asq <- as.data.frame(IT_CS_Asq)
write_xlsx(IT_CS_Asq, "output/IT_CS_Asq.xlsx")
