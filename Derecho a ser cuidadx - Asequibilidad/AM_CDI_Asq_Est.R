# Código abierto para un diagnóstico de cuidados a nivel municipal
# Laboratorio Nacional de Políticas Públicas (LNPP)
# Centro de Investigación y Docencia Económicas (CIDE)

# Propósito de AM_CDI_Asq_Est:
# Calcular el indicador de asequibilidad de cuidados para el 
# desarrollo integral de adultos mayores desde el Estado

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


############################## Cargar bases ####################################

# 1. Cargar bases
alojamientos_caas <- read_excel("input/caas/Alojamientos de asistencia social.xls")
usuarios_caas <- read_excel("input/caas/Poblacion usuaria residente.xls")

# 2. Limpiar nombres columnas
alojamientos_caas <- clean_names(alojamientos_caas)
usuarios_caas <- clean_names(usuarios_caas)

# 3. Eliminar fila sin datos 
alojamientos_caas <- alojamientos_caas[-1, ]
usuarios_caas <- usuarios_caas[-1, ]

################## Seleccionar municipio y estado de interés ##################

# 1. Revisar este documento para obtener el código del estado y municipio de interés 
# (https://www.inegi.org.mx/app/ageeml/). 

# 2. Sustituir código en los siguientes objetos
estado <- "06"
municipio <- "007"

# 3. Filtrar bases
alojamientos_caas <- alojamientos_caas %>% 
  filter(entidad == "06", mun == "007")

usuarios_caas <- usuarios_caas %>% 
  filter(entidad == "06", mun == "007") 

# 4. Juntar bases de datos
caas <- alojamientos_caas %>%
  left_join(usuarios_caas, by = "id_aloja")

##################### Extraer información para indicador #######################

# 1. Calcular denominador
adultos_mayores_pub <- caas %>%
  filter(clasealoja.x == 2, fig_juri == 5) 
denominador <- sum(adultos_mayores_pub$r_t_65ym, na.rm = TRUE)
denominador

# 2. Calcular numerador
adultos_mayores_pub_gratis <- caas %>%
  filter(clasealoja.x == 2, fig_juri == 5,
                               cuotas == 2, gobierno == 1) 
numerador <- sum(adultos_mayores_pub_gratis$r_t_65ym, na.rm = TRUE)
numerador

# 3. Indicador
AM_CDI_Asq_Est <- (numerador/denominador)*100
AM_CDI_Asq_Est
# Si el denominador y numerador son 0, el indicador saldrá como NaN, lo cual 
# significa que, en el municipio de interés, no existen estos servicios

# 4. Guardar los resultados del indicador en la carpeta de output como archivo .xlsx de Excel
AM_CDI_Asq_Est <- as.data.frame(AM_CDI_Asq_Est)
write_xlsx(AM_CDI_Asq_Est, "output/AM_CDI_Asq_Est.xlsx")

